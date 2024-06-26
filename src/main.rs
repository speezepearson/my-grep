use std::{
    collections::HashMap,
    fmt::Display,
    io::{BufRead as _, Write as _},
    str::FromStr,
};

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::Parser as _;

#[derive(Debug, Clone, PartialEq, Eq)]
enum GroupCapturingness {
    NonCapturing,
    Positional,
    Named(String),
}
#[derive(Debug, Clone, PartialEq, Eq)]
enum Pattern {
    Exact(String),
    Repeat {
        pat: Box<Self>,
        min: u16,
        max: Option<u16>,
    },
    Concat(Vec<Self>),
    Group {
        pat: Box<Self>,
        capture: GroupCapturingness,
    },
    Or(Vec<Self>),
    Dot,
    PositionalBackreference(u16),
    NamedBackreference(String),
    Ranges {
        inverted: bool,
        ranges: Vec<(char, char)>,
    },
    Beginning,
    End,
}
impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Exact(c) => f.write_str(c),
            Self::Repeat { pat, min, max } => {
                pat.fmt(f)?;
                match (min, max) {
                    (0, None) => f.write_str("*"),
                    (1, None) => f.write_str("+"),
                    (0, Some(1)) => f.write_str("?"),
                    (min, None) => f.write_fmt(format_args!("{{{},}}", min)),
                    (0, Some(max)) => f.write_fmt(format_args!("{{,{}}}", max)),
                    (min, Some(max)) if min == max => f.write_fmt(format_args!("{{{}}}", min)),
                    (min, Some(max)) => f.write_fmt(format_args!("{{{},{}}}", min, max)),
                }
            }
            Self::Concat(pats) => {
                for pat in pats {
                    pat.fmt(f)?;
                }
                Ok(())
            }
            Self::Group { pat, capture } => {
                f.write_str("(")?;
                match capture {
                    GroupCapturingness::NonCapturing => f.write_str("?:")?,
                    GroupCapturingness::Positional => {}
                    GroupCapturingness::Named(name) => f.write_fmt(format_args!("?P<{}>", name))?,
                }
                f.write_fmt(format_args!("{}", pat.to_string()))?;
                f.write_str(")")
            }
            Self::Or(pats) => {
                for (i, pat) in pats.iter().enumerate() {
                    if i != 0 {
                        f.write_str("|")?;
                    }
                    pat.fmt(f)?;
                }
                Ok(())
            }
            Self::Dot => f.write_str("."),
            Self::PositionalBackreference(n) => f.write_fmt(format_args!("\\{}", n)),
            Self::NamedBackreference(name) => f.write_fmt(format_args!("\\g<{}>", name)),
            Self::Ranges { inverted, ranges } => {
                f.write_str("[")?;
                if *inverted {
                    f.write_str("^")?;
                }
                for (start, end) in ranges {
                    if start == end {
                        f.write_fmt(format_args!("{}", start))?;
                    } else {
                        f.write_fmt(format_args!("{}-{}", start, end))?;
                    }
                }
                f.write_str("]")
            }
            Self::Beginning => f.write_str("^"),
            Self::End => f.write_str("$"),
        }
    }
}

impl FromStr for Pattern {
    type Err = Vec<chumsky::error::Simple<char>>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parser().parse(s)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MatchState<'a> {
    positional_groups: Vec<&'a str>,
    named_groups: HashMap<String, &'a str>,
    remainder: &'a str,
    is_beginning: bool,
}
impl Pattern {
    fn execute<'a>(&self, target: &'a str) -> Vec<MatchState<'a>> {
        self.step(MatchState {
            positional_groups: vec![],
            named_groups: HashMap::new(),
            remainder: target,
            is_beginning: true,
        })
    }
    fn step<'a>(&self, state: MatchState<'a>) -> Vec<MatchState<'a>> {
        match self {
            Pattern::Exact(s) => {
                if state.remainder.starts_with(s) {
                    vec![MatchState {
                        positional_groups: state.positional_groups.clone(),
                        named_groups: state.named_groups.clone(),
                        remainder: &state.remainder[s.len()..],
                        is_beginning: state.is_beginning && s.is_empty(),
                    }]
                } else {
                    vec![]
                }
            }
            Pattern::Concat(pats) => {
                let mut states = vec![state];
                for pat in pats {
                    // eprintln!("running subpat {}", pat.to_string());
                    states = states
                        .into_iter()
                        .flat_map(|state| {
                            // let prefix = format!("  {:?} -> ", state);
                            let r = pat.step(state);
                            // eprintln!("{}{:?}", prefix, r);
                            r
                        })
                        .collect();
                }
                states
            }
            Pattern::Repeat { pat, min, max } => {
                let mut states = vec![state];
                for _ in 0..*min {
                    states = states
                        .into_iter()
                        .flat_map(|state| pat.step(state))
                        .collect();
                }
                let mut frontier = states.clone();
                for _ in *min..max.unwrap_or(u16::MAX) {
                    let mut new_frontier = vec![];
                    for state in frontier {
                        new_frontier.extend(pat.step(state));
                    }
                    if new_frontier.is_empty() {
                        break;
                    }
                    states.extend(new_frontier.clone());
                    frontier = new_frontier;
                }
                states
            }
            Pattern::Group { pat, capture } => {
                let orig_state = state.clone();
                let states = pat.step(state);
                match capture {
                    GroupCapturingness::NonCapturing => states,
                    GroupCapturingness::Positional => states
                        .into_iter()
                        .map(|state| MatchState {
                            positional_groups: [
                                orig_state.positional_groups.clone(),
                                vec![
                                    &orig_state.remainder
                                        [..orig_state.remainder.len() - state.remainder.len()],
                                ],
                                state.positional_groups[orig_state.positional_groups.len()..]
                                    .to_vec(),
                            ]
                            .concat(),
                            is_beginning: orig_state.is_beginning
                                && orig_state.remainder == state.remainder,
                            ..state
                        })
                        .collect(),
                    GroupCapturingness::Named(name) => states
                        .into_iter()
                        .map(|mut state| {
                            state.named_groups.insert(
                                name.clone(),
                                &orig_state.remainder
                                    [..orig_state.remainder.len() - state.remainder.len()],
                            );
                            state
                        })
                        .collect(),
                }
            }
            Pattern::Or(_) => todo!(),
            Pattern::Dot => {
                if state.remainder.is_empty() {
                    vec![]
                } else {
                    vec![MatchState {
                        remainder: &state.remainder[1..],
                        is_beginning: false,
                        ..state
                    }]
                }
            }
            Pattern::PositionalBackreference(n) => {
                // eprintln!("backref {} -> {:?}", n, state.positional_groups);
                match state.positional_groups.get(*n as usize - 1) {
                    Some(group) => {
                        // eprintln!("group {:?}", group);
                        if state.remainder.starts_with(group) {
                            vec![MatchState {
                                remainder: &state.remainder[group.len()..],
                                is_beginning: state.is_beginning && group.is_empty(),
                                ..state
                            }]
                        } else {
                            vec![]
                        }
                    }
                    None => vec![],
                }
            }
            Pattern::NamedBackreference(name) => match state.named_groups.get(name) {
                Some(group) => {
                    if state.remainder.starts_with(group) {
                        vec![MatchState {
                            remainder: &state.remainder[group.len()..],
                            is_beginning: state.is_beginning && group.is_empty(),
                            ..state
                        }]
                    } else {
                        vec![]
                    }
                }
                None => vec![],
            },
            Pattern::Ranges { inverted, ranges } => {
                if state.remainder.is_empty() {
                    vec![]
                } else {
                    let mut chars = state.remainder.chars();
                    match chars.next() {
                        None => vec![],
                        Some(c)
                            if inverted
                                ^ ranges.iter().any(|(start, end)| c >= *start && c <= *end) =>
                        {
                            vec![MatchState {
                                remainder: chars.as_str(),
                                is_beginning: false,
                                ..state
                            }]
                        }
                        Some(_) => vec![],
                    }
                }
            }
            Pattern::Beginning => {
                if state.is_beginning {
                    vec![state]
                } else {
                    vec![]
                }
            }
            Pattern::End => {
                if state.remainder.is_empty() {
                    vec![state]
                } else {
                    vec![]
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_execute_exact() {
        let pat: Pattern = "a".parse().unwrap();
        let states = pat.execute("a");
        assert_eq!(
            states,
            vec![MatchState {
                positional_groups: vec![],
                named_groups: HashMap::new(),
                remainder: "",
                is_beginning: false,
            }]
        );
    }

    #[test]
    fn test_execute_dot() {
        let pat: Pattern = ".".parse().unwrap();
        let states = pat.execute("a");
        assert_eq!(
            states,
            vec![MatchState {
                positional_groups: vec![],
                named_groups: HashMap::new(),
                remainder: "",
                is_beginning: false,
            }]
        );
    }

    #[test]
    fn test_execute_positional_capture() {
        let pat: Pattern = "(.)".parse().unwrap();
        let states = pat.execute("a");
        assert_eq!(
            states,
            vec![MatchState {
                positional_groups: vec!["a"],
                named_groups: HashMap::new(),
                remainder: "",
                is_beginning: false,
            },]
        );
    }

    #[test]
    fn test_execute_named_capture() {
        let pat: Pattern = "(?P<foo>.)".parse().unwrap();
        let states = pat.execute("a");
        assert_eq!(
            states,
            vec![MatchState {
                positional_groups: vec![],
                named_groups: [("foo".to_string(), "a")].iter().cloned().collect(),
                remainder: "",
                is_beginning: false,
            },]
        );
    }

    #[test]
    fn test_execute_questionmark() {
        let pat: Pattern = "a?".parse().unwrap();
        let mut states = pat.execute("a");
        states.sort_by(|a, b| a.remainder.len().cmp(&b.remainder.len()));
        assert_eq!(
            states,
            vec![
                MatchState {
                    positional_groups: vec![],
                    named_groups: HashMap::new(),
                    remainder: "",
                    is_beginning: false,
                },
                MatchState {
                    positional_groups: vec![],
                    named_groups: HashMap::new(),
                    remainder: "a",
                    is_beginning: true,
                },
            ]
        );
    }

    #[test]
    fn test_execute_star() {
        let pat: Pattern = "a*".parse().unwrap();
        let mut states = pat.execute("aaa");
        assert_eq!(states.len(), 4);
        states.sort_by(|a, b| a.remainder.len().cmp(&b.remainder.len()));
        assert_eq!(
            states,
            vec![
                MatchState {
                    positional_groups: vec![],
                    named_groups: HashMap::new(),
                    remainder: "",
                    is_beginning: false,
                },
                MatchState {
                    positional_groups: vec![],
                    named_groups: HashMap::new(),
                    remainder: "a",
                    is_beginning: false,
                },
                MatchState {
                    positional_groups: vec![],
                    named_groups: HashMap::new(),
                    remainder: "aa",
                    is_beginning: false,
                },
                MatchState {
                    positional_groups: vec![],
                    named_groups: HashMap::new(),
                    remainder: "aaa",
                    is_beginning: true,
                },
            ]
        );
    }

    #[test]
    fn test_execute_star_and_capture() {
        let pat: Pattern = "a(b*)".parse().unwrap();
        let mut states = pat.execute("abbb");
        assert_eq!(states.len(), 4);
        states.sort_by(|a, b| a.remainder.len().cmp(&b.remainder.len()));
        assert_eq!(
            states,
            vec![
                MatchState {
                    positional_groups: vec!["bbb"],
                    named_groups: HashMap::new(),
                    remainder: "",
                    is_beginning: false,
                },
                MatchState {
                    positional_groups: vec!["bb"],
                    named_groups: HashMap::new(),
                    remainder: "b",
                    is_beginning: false,
                },
                MatchState {
                    positional_groups: vec!["b"],
                    named_groups: HashMap::new(),
                    remainder: "bb",
                    is_beginning: false,
                },
                MatchState {
                    positional_groups: vec![""],
                    named_groups: HashMap::new(),
                    remainder: "bbb",
                    is_beginning: false,
                },
            ]
        );

        let pat: Pattern = "a(b*)c".parse().unwrap();
        let states = pat.execute("abbb");
        assert_eq!(states.len(), 0);

        let pat: Pattern = "a(b*)c".parse().unwrap();
        assert_eq!(
            pat,
            Pattern::Concat(vec![
                Pattern::Exact("a".to_string()),
                Pattern::Group {
                    pat: Box::new(Pattern::Repeat {
                        pat: Box::new(Pattern::Exact("b".to_string())),
                        min: 0,
                        max: None,
                    }),
                    capture: GroupCapturingness::Positional,
                },
                Pattern::Exact("c".to_string()),
            ])
        );
        let states = pat.execute("abbbc");
        assert_eq!(
            states,
            vec![MatchState {
                positional_groups: vec!["bbb"],
                named_groups: HashMap::new(),
                remainder: "",
                is_beginning: false,
            },]
        );
    }

    #[test]
    fn test_positional_backreference() {
        let pat: Pattern = "a(b*)\\1c".parse().unwrap();
        let states = pat.execute("abbbbc");
        assert_eq!(
            states,
            vec![MatchState {
                positional_groups: vec!["bb"],
                named_groups: HashMap::new(),
                remainder: "",
                is_beginning: false,
            },]
        );

        let states = pat.execute("abbbc");
        assert_eq!(states, vec![]);
    }

    #[test]
    fn test_named_backreference() {
        let pat: Pattern = "a(?P<glorp>b*)\\g<glorp>c".parse().unwrap();
        let states = pat.execute("abbbbc");
        assert_eq!(
            states,
            vec![MatchState {
                positional_groups: vec![],
                named_groups: [("glorp".to_string(), "bb")].iter().cloned().collect(),
                remainder: "",
                is_beginning: false,
            },]
        );

        let states = pat.execute("abbbc");
        assert_eq!(states, vec![]);
    }

    #[test]
    fn test_uncaptured_backreference() {
        let pat: Pattern = "(?:a)(b)c".parse().unwrap();
        let states = pat.execute("abc");
        assert_eq!(
            states,
            vec![MatchState {
                positional_groups: vec!["b"],
                named_groups: HashMap::new(),
                remainder: "",
                is_beginning: false,
            },]
        );

        let states = pat.execute("abbbc");
        assert_eq!(states, vec![]);
    }

    #[test]
    fn test_nested_positional_captures() {
        let pat: Pattern = "(a((b)(c))d)e".parse().unwrap();
        let states = pat.execute("abcde");
        assert_eq!(
            states,
            vec![MatchState {
                positional_groups: vec!["abcd", "bc", "b", "c"],
                named_groups: HashMap::new(),
                remainder: "",
                is_beginning: false,
            },]
        );

        let states = pat.execute("abbbc");
        assert_eq!(states, vec![]);
    }

    #[test]
    fn test_beginning() {
        let pat: Pattern = "^a".parse().unwrap();
        let states = pat.execute("a");
        assert_eq!(
            states,
            vec![MatchState {
                positional_groups: vec![],
                named_groups: HashMap::new(),
                remainder: "",
                is_beginning: false,
            }]
        );

        let states = pat.execute("ba");
        assert_eq!(states, vec![]);
    }

    #[test]
    fn test_end() {
        let pat: Pattern = "a?".parse().unwrap();
        let mut states = pat.execute("a");
        states.sort_by(|a, b| a.remainder.len().cmp(&b.remainder.len()));
        assert_eq!(
            states,
            vec![
                MatchState {
                    positional_groups: vec![],
                    named_groups: HashMap::new(),
                    remainder: "",
                    is_beginning: false,
                },
                MatchState {
                    positional_groups: vec![],
                    named_groups: HashMap::new(),
                    remainder: "a",
                    is_beginning: true,
                },
            ]
        );

        let pat: Pattern = "a?$".parse().unwrap();
        let states = pat.execute("a");
        assert_eq!(
            states,
            vec![MatchState {
                positional_groups: vec![],
                named_groups: HashMap::new(),
                remainder: "",
                is_beginning: false,
            }]
        );
    }

    // #[test]
    // fn test_tricky_backreference() {
    //     let pat: Pattern = "(?:)".parse().unwrap();
    //     let states = pat.execute("abc");
    //     assert_eq!(
    //         states,
    //         vec![MatchState {
    //             positional_groups: vec!["b"],
    //             named_groups: HashMap::new(),
    //             remainder: "",
    //         },]
    //     );

    //     let states = pat.execute("abbbc");
    //     assert_eq!(states, vec![]);
    // }

    #[test]
    fn test_parse_smoke() {
        let pat = "a(b*)".parse::<Pattern>();
        assert_eq!(
            pat,
            Ok(Pattern::Concat(vec![
                Pattern::Exact("a".to_string()),
                Pattern::Group {
                    pat: Box::new(Pattern::Repeat {
                        pat: Box::new(Pattern::Exact("b".to_string())),
                        min: 0,
                        max: None,
                    }),
                    capture: GroupCapturingness::Positional,
                },
            ]))
        );
    }

    #[test]
    fn test_parse_range() {
        let pat = "a[b]".parse::<Pattern>();
        assert_eq!(
            pat,
            Ok(Pattern::Concat(vec![
                Pattern::Exact("a".to_string()),
                Pattern::Ranges {
                    inverted: false,
                    ranges: vec![('b', 'b')]
                },
            ]))
        );

        let pat = "a[^b]".parse::<Pattern>();
        assert_eq!(
            pat,
            Ok(Pattern::Concat(vec![
                Pattern::Exact("a".to_string()),
                Pattern::Ranges {
                    inverted: true,
                    ranges: vec![('b', 'b')]
                },
            ]))
        );

        let pat = "a[b-d]".parse::<Pattern>();
        assert_eq!(
            pat,
            Ok(Pattern::Concat(vec![
                Pattern::Exact("a".to_string()),
                Pattern::Ranges {
                    inverted: false,
                    ranges: vec![('b', 'd')]
                },
            ]))
        );

        let pat = "a[bc]".parse::<Pattern>();
        assert_eq!(
            pat,
            Ok(Pattern::Concat(vec![
                Pattern::Exact("a".to_string()),
                Pattern::Ranges {
                    inverted: false,
                    ranges: vec![('b', 'b'), ('c', 'c')]
                },
            ]))
        );

        let pat = "a[bd-e]".parse::<Pattern>();
        assert_eq!(
            pat,
            Ok(Pattern::Concat(vec![
                Pattern::Exact("a".to_string()),
                Pattern::Ranges {
                    inverted: false,
                    ranges: vec![('b', 'b'), ('d', 'e')]
                },
            ]))
        );
    }

    #[test]
    fn test_parse_smoke_2() {
        let pat = "f([aeiou])\\1*[^aeiou]".parse::<Pattern>();
        assert_eq!(
            pat,
            Ok(Pattern::Concat(vec![
                Pattern::Exact("f".to_string()),
                Pattern::Group {
                    pat: Box::new(Pattern::Ranges {
                        inverted: false,
                        ranges: vec![('a', 'a'), ('e', 'e'), ('i', 'i'), ('o', 'o'), ('u', 'u')]
                    }),
                    capture: GroupCapturingness::Positional,
                },
                Pattern::Repeat {
                    pat: Box::new(Pattern::PositionalBackreference(1)),
                    min: 0,
                    max: None,
                },
                Pattern::Ranges {
                    inverted: true,
                    ranges: vec![('a', 'a'), ('e', 'e'), ('i', 'i'), ('o', 'o'), ('u', 'u')]
                },
            ]))
        );
    }
}

fn parser() -> impl chumsky::Parser<char, Pattern, Error = chumsky::error::Simple<char>> {
    use chumsky::prelude::*;

    let u16_pat = text::int(10).try_map(|s: String, span| {
        s.parse::<u16>()
            .map_err(|e| Simple::custom(span, format!("invalid u16: {}", e)))
    });

    let basic_char = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_- ");
    let range_piece = basic_char
        .clone()
        .then(just('-').ignore_then(basic_char.clone()).or_not());
    let range = just('^')
        .or_not()
        .map(|inverted| inverted.is_some())
        .then(range_piece.clone().repeated().at_least(1))
        .delimited_by(just('['), just(']'))
        .map(|(inverted, ranges)| Pattern::Ranges {
            inverted,
            ranges: ranges.iter().map(|(a, b)| (*a, b.unwrap_or(*a))).collect(),
        })
        .labelled("char range");
    let pos_backref = just('\\')
        .ignore_then(u16_pat)
        .map(Pattern::PositionalBackreference)
        .labelled("positional backreference");
    let named_backref = just("\\g<")
        .ignore_then(basic_char.clone().repeated().at_least(1))
        .then_ignore(just('>'))
        .map(|name| Pattern::NamedBackreference(name.iter().collect()))
        .labelled("named backreference");
    let dot = just('.').to(Pattern::Dot);

    recursive(|pat| {
        let group_opts = choice((
            just(":").map(|_| GroupCapturingness::NonCapturing),
            just("P<")
                .ignore_then(basic_char.clone().repeated().at_least(1))
                .then_ignore(just('>'))
                .map(|name| GroupCapturingness::Named(name.iter().collect()))
                .labelled("group name"),
        ));
        let group = just('?')
            .ignore_then(group_opts.clone().labelled("group opts"))
            .or_not()
            .map(|opts| opts.unwrap_or(GroupCapturingness::Positional))
            .then(pat)
            .delimited_by(just('('), just(')'))
            .map(|(capture, p)| Pattern::Group {
                pat: Box::new(p),
                capture,
            });
        let atom = choice((
            basic_char
                .clone()
                .repeated()
                .at_least(1)
                .map(|cs| Pattern::Exact(cs.into_iter().collect())),
            dot,
            pos_backref,
            named_backref,
            range,
            group,
            just('^').to(Pattern::Beginning),
            just('$').to(Pattern::End),
        ));
        let repeat = atom
            .clone()
            .then(
                choice((
                    just('?').to((0, Some(1))),
                    just('+').to((1, None)),
                    just('*').to((0, None)),
                    just('{')
                        .ignore_then(choice((
                            just(',')
                                .ignore_then(u16_pat.clone())
                                .map(|max| (0, Some(max))),
                            u16_pat
                                .clone()
                                .then_ignore(just(','))
                                .then(u16_pat)
                                .map(|(min, max)| (min, Some(max))),
                            u16_pat
                                .clone()
                                .then_ignore(just(','))
                                .map(|min| (min, None)),
                            u16_pat.clone().map(|n| (n, None)),
                        )))
                        .then_ignore(just('}'))
                        .labelled("repetition limits"),
                ))
                .or_not(),
            )
            .map(|(p, minmax)| match minmax {
                None => p,
                Some((min, max)) => Pattern::Repeat {
                    pat: Box::new(p),
                    min,
                    max,
                },
            });
        let alt_branch = repeat.clone().repeated().map(|pats| {
            if pats.len() == 1 {
                pats[0].clone()
            } else {
                Pattern::Concat(pats)
            }
        });
        let alts = alt_branch
            .clone()
            .separated_by(just('|'))
            .at_least(1)
            .map(|x| {
                if x.len() == 1 {
                    x[0].clone()
                } else {
                    Pattern::Or(x)
                }
            });
        alts
    })
    .then_ignore(end())
}

#[derive(clap::Parser)]
struct Args {
    input: String,
}

fn main() -> Result<(), Vec<chumsky::error::Simple<char>>> {
    let args = <Args as clap::Parser>::parse();
    let pat: Pattern = match parser()
        .map_with_span(|c, s| (c, s))
        .parse(args.input.clone())
    {
        Ok((pat, _)) => Pattern::Concat(vec![Pattern::from_str(".*").unwrap(), pat]),
        Err(errs) => {
            for err in errs {
                Report::build(ReportKind::Error, (), err.span().start)
                    .with_code(3)
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(err.span())
                            .with_message(format!("{:?}", err.reason()))
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(&args.input))
                    .unwrap();
            }
            std::process::exit(3);
        }
    };

    for line in std::io::stdin().lock().lines() {
        let line = line.unwrap();
        let states = pat.execute(&line);
        if !states.is_empty() {
            let mut stdout = std::io::stdout().lock();
            stdout.write_all(line.as_bytes()).unwrap();
            stdout.write_all(b"\n").unwrap();
            stdout.flush().unwrap();
        }
    }

    Ok(())
}
