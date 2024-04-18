use std::{
    collections::HashMap,
    io::{BufRead as _, Write as _},
    str::FromStr,
};

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
}
impl ToString for Pattern {
    fn to_string(&self) -> String {
        match self {
            Self::Exact(c) => c.clone(),
            Self::Repeat { pat, min, max } => match (min, max) {
                (0, None) => format!("{}*", pat.to_string()),
                (1, None) => format!("{}+", pat.to_string()),
                (min, None) => format!("{}{{{},}}", pat.to_string(), min),
                (0, Some(1)) => format!("{}?", pat.to_string()),
                (min, Some(max)) => {
                    format!(
                        "{}{{{},{}}}",
                        pat.to_string(),
                        if *min == 0 {
                            "".to_string()
                        } else {
                            min.to_string()
                        },
                        max
                    )
                }
            },
            Self::Concat(pats) => pats.iter().map(Self::to_string).collect(),
            Self::Group { pat, capture } => format!(
                "({}{})",
                match capture {
                    GroupCapturingness::NonCapturing => "?:".to_string(),
                    GroupCapturingness::Positional => "".to_string(),
                    GroupCapturingness::Named(name) => format!("?P<{}>", name),
                },
                pat.to_string()
            ),
            Self::Or(pats) => format!(
                "({})",
                pats.iter()
                    .map(Self::to_string)
                    .collect::<Vec<_>>()
                    .join("|")
            ),
            Self::Dot => ".".to_string(),
            Self::PositionalBackreference(n) => format!("\\{}", n),
            Self::NamedBackreference(name) => format!("\\g<{}>", name),
            Self::Ranges { inverted, ranges } => {
                let mut s = "[".to_string();
                if *inverted {
                    s.push('^');
                }
                for (start, end) in ranges {
                    if start == end {
                        s.push(*start);
                    } else {
                        s.push(*start);
                        s.push('-');
                        s.push(*end);
                    }
                }
                s.push(']');
                s
            }
        }
    }
}

impl FromStr for Pattern {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parser = parser();
        match parser.parse(s) {
            Ok(result) => Ok(result),
            Err(_) => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MatchState<'a> {
    positional_groups: Vec<&'a str>,
    named_groups: HashMap<String, &'a str>,
    remainder: &'a str,
}
impl Pattern {
    fn execute<'a>(&self, target: &'a str) -> Vec<MatchState<'a>> {
        self.step(MatchState {
            positional_groups: vec![],
            named_groups: HashMap::new(),
            remainder: target,
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
                        .map(|state| {
                            MatchState {
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
                                ..state
                            }
                            // state.positional_groups.push(
                            //     &orig_state.remainder
                            //         [..orig_state.remainder.len() - state.remainder.len()],
                            // );
                            // state
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
                                ..state
                            }]
                        }
                        Some(_) => vec![],
                    }
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
                },
                MatchState {
                    positional_groups: vec![],
                    named_groups: HashMap::new(),
                    remainder: "a",
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
                },
                MatchState {
                    positional_groups: vec![],
                    named_groups: HashMap::new(),
                    remainder: "a",
                },
                MatchState {
                    positional_groups: vec![],
                    named_groups: HashMap::new(),
                    remainder: "aa",
                },
                MatchState {
                    positional_groups: vec![],
                    named_groups: HashMap::new(),
                    remainder: "aaa",
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
                },
                MatchState {
                    positional_groups: vec!["bb"],
                    named_groups: HashMap::new(),
                    remainder: "b",
                },
                MatchState {
                    positional_groups: vec!["b"],
                    named_groups: HashMap::new(),
                    remainder: "bb",
                },
                MatchState {
                    positional_groups: vec![""],
                    named_groups: HashMap::new(),
                    remainder: "bbb",
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
            },]
        );

        let states = pat.execute("abbbc");
        assert_eq!(states, vec![]);
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
        });
    let pos_backref = just('\\')
        .ignore_then(one_of("0123456789").repeated().at_least(1))
        .map(|cs| {
            Pattern::PositionalBackreference(cs.into_iter().collect::<String>().parse().unwrap())
        });
    let named_backref = just("\\g<")
        .ignore_then(basic_char.clone().repeated().at_least(1))
        .then_ignore(just('>'))
        .map(|name| Pattern::NamedBackreference(name.iter().collect()));
    let dot = just('.').to(Pattern::Dot);

    recursive(|pat| {
        let group_opts = choice((
            just(":").map(|_| GroupCapturingness::NonCapturing),
            just("P<")
                .ignore_then(basic_char.clone().repeated().at_least(1))
                .then_ignore(just('>'))
                .map(|name| GroupCapturingness::Named(name.iter().collect())),
        ));
        let group = just('?')
            .ignore_then(group_opts)
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
        ));
        let repeat = atom
            .clone()
            .then(
                choice((
                    just('?').to((0, Some(1))),
                    just('+').to((1, None)),
                    just('*').to((0, None)),
                    just('{')
                        .ignore_then(one_of("0123456789").repeated().map(|cs| {
                            if cs.is_empty() {
                                0
                            } else {
                                cs.into_iter().collect::<String>().parse().unwrap()
                                // TODO: error handling
                            }
                        }))
                        .then_ignore(just(','))
                        .then(one_of("0123456789").repeated().map(|cs| {
                            if cs.is_empty() {
                                None
                            } else {
                                Some(cs.into_iter().collect::<String>().parse().unwrap())
                                // TODO: error handling
                            }
                        }))
                        .then_ignore(just('}')),
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
    let pat: Pattern = format!(".*(?:{})", args.input).parse().unwrap();

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
