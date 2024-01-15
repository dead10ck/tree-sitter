use super::ExtractedSyntaxGrammar;
use crate::generate::grammars::{Variable, VariableType};
use crate::generate::rules::{Rule, RuleType, Symbol};
use std::collections::HashMap;
use std::mem;

struct Expander {
    variable_name: String,
    repeat_count_in_variable: usize,
    preceding_symbol_count: usize,
    auxiliary_variables: Vec<Variable>,
    existing_repeats: HashMap<Rule, Symbol>,
}

impl Expander {
    fn expand_variable(&mut self, index: usize, variable: &mut Variable) -> bool {
        self.variable_name.clear();
        self.variable_name.push_str(&variable.name);
        self.repeat_count_in_variable = 0;
        let mut rule = Rule::blank();
        mem::swap(&mut rule, &mut variable.rule);

        // In the special case of a hidden variable with a repetition at its top level,
        // convert that rule itself into a binary tree structure instead of introducing
        // another auxiliary rule.
        match (variable.kind, rule) {
            (
                VariableType::Hidden,
                Rule {
                    kind: RuleType::Repeat(repeated_content),
                    ..
                },
            ) => {
                let inner_rule = self.expand_rule(*repeated_content);
                variable.rule =
                    self.wrap_rule_in_binary_tree(Symbol::non_terminal(index), inner_rule);
                variable.kind = VariableType::Auxiliary;
                true
            }
            (_, rule) => {
                variable.rule = self.expand_rule(rule);
                false
            }
        }
    }

    fn expand_rule<R: Into<Rule>>(&mut self, rule: R) -> Rule {
        let rule = rule.into();

        let kind = match rule.kind {
            // For choices, sequences, and metadata, descend into the child rules,
            // replacing any nested repetitions.
            RuleType::Choice(elements) => RuleType::Choice(
                elements
                    .into_iter()
                    .map(|element| self.expand_rule(element))
                    .collect(),
            ),

            RuleType::Seq(elements) => RuleType::Seq(
                elements
                    .into_iter()
                    .map(|element| self.expand_rule(element))
                    .collect(),
            ),

            // For repetitions, introduce an auxiliary rule that contains the the
            // repeated content, but can also contain a recursive binary tree structure.
            RuleType::Repeat(content) => {
                let inner_rule = self.expand_rule(&*content);

                if let Some(existing_symbol) = self.existing_repeats.get(&inner_rule) {
                    RuleType::Symbol(*existing_symbol)
                } else {
                    self.repeat_count_in_variable += 1;

                    let rule_name = format!(
                        "{}_repeat{}",
                        self.variable_name, self.repeat_count_in_variable
                    );

                    let repeat_symbol = Symbol::non_terminal(
                        self.preceding_symbol_count + self.auxiliary_variables.len(),
                    );

                    self.existing_repeats
                        .insert(inner_rule.clone(), repeat_symbol);

                    self.auxiliary_variables.push(Variable {
                        name: rule_name,
                        kind: VariableType::Auxiliary,
                        rule: self.wrap_rule_in_binary_tree(repeat_symbol, inner_rule),
                    });

                    RuleType::Symbol(repeat_symbol)
                }
            }

            // For primitive rules, don't change anything.
            kind => kind,
        };

        Rule {
            kind,
            params: rule.params,
        }
    }

    fn wrap_rule_in_binary_tree(&self, symbol: Symbol, rule: Rule) -> Rule {
        Rule::choice(vec![
            Rule::seq(vec![
                RuleType::Symbol(symbol).into(),
                RuleType::Symbol(symbol).into(),
            ]),
            rule,
        ])
    }
}

pub(super) fn expand_repeats(mut grammar: ExtractedSyntaxGrammar) -> ExtractedSyntaxGrammar {
    let mut expander = Expander {
        variable_name: String::new(),
        repeat_count_in_variable: 0,
        preceding_symbol_count: grammar.variables.len(),
        auxiliary_variables: Vec::new(),
        existing_repeats: HashMap::new(),
    };

    for (i, mut variable) in grammar.variables.iter_mut().enumerate() {
        let expanded_top_level_repetition = expander.expand_variable(i, &mut variable);

        // If a hidden variable had a top-level repetition and it was converted to
        // a recursive rule, then it can't be inlined.
        if expanded_top_level_repetition {
            grammar
                .variables_to_inline
                .retain(|symbol| *symbol != Symbol::non_terminal(i));
        }
    }

    grammar
        .variables
        .extend(expander.auxiliary_variables.into_iter());

    grammar
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_repeat_expansion() {
        // Repeats nested inside of sequences and choices are expanded.
        let grammar = expand_repeats(build_grammar(vec![Variable::named(
            "rule0",
            Rule::seq(vec![
                Rule::terminal(10),
                Rule::choice(vec![
                    Rule::repeat(Rule::terminal(11)),
                    Rule::repeat(Rule::terminal(12)),
                ]),
                Rule::terminal(13),
            ]),
        )]));

        assert_eq!(
            grammar.variables,
            vec![
                Variable::named(
                    "rule0",
                    Rule::seq(vec![
                        Rule::terminal(10),
                        Rule::choice(vec![Rule::non_terminal(1), Rule::non_terminal(2),]),
                        Rule::terminal(13),
                    ])
                ),
                Variable::auxiliary(
                    "rule0_repeat1",
                    Rule::choice(vec![
                        Rule::seq(vec![Rule::non_terminal(1), Rule::non_terminal(1),]),
                        Rule::terminal(11),
                    ])
                ),
                Variable::auxiliary(
                    "rule0_repeat2",
                    Rule::choice(vec![
                        Rule::seq(vec![Rule::non_terminal(2), Rule::non_terminal(2),]),
                        Rule::terminal(12),
                    ])
                ),
            ]
        );
    }

    #[test]
    fn test_repeat_deduplication() {
        // Terminal 4 appears inside of a repeat in three different places.
        let grammar = expand_repeats(build_grammar(vec![
            Variable::named(
                "rule0",
                Rule::choice(vec![
                    Rule::seq(vec![Rule::terminal(1), Rule::repeat(Rule::terminal(4))]),
                    Rule::seq(vec![Rule::terminal(2), Rule::repeat(Rule::terminal(4))]),
                ]),
            ),
            Variable::named(
                "rule1",
                Rule::seq(vec![Rule::terminal(3), Rule::repeat(Rule::terminal(4))]),
            ),
        ]));

        // Only one auxiliary rule is created for repeating terminal 4.
        assert_eq!(
            grammar.variables,
            vec![
                Variable::named(
                    "rule0",
                    Rule::choice(vec![
                        Rule::seq(vec![Rule::terminal(1), Rule::non_terminal(2)]),
                        Rule::seq(vec![Rule::terminal(2), Rule::non_terminal(2)]),
                    ])
                ),
                Variable::named(
                    "rule1",
                    Rule::seq(vec![Rule::terminal(3), Rule::non_terminal(2),])
                ),
                Variable::auxiliary(
                    "rule0_repeat1",
                    Rule::choice(vec![
                        Rule::seq(vec![Rule::non_terminal(2), Rule::non_terminal(2),]),
                        Rule::terminal(4),
                    ])
                )
            ]
        );
    }

    #[test]
    fn test_expansion_of_nested_repeats() {
        let grammar = expand_repeats(build_grammar(vec![Variable::named(
            "rule0",
            Rule::seq(vec![
                Rule::terminal(10),
                Rule::repeat(Rule::seq(vec![
                    Rule::terminal(11),
                    Rule::repeat(Rule::terminal(12)),
                ])),
            ]),
        )]));

        assert_eq!(
            grammar.variables,
            vec![
                Variable::named(
                    "rule0",
                    Rule::seq(vec![Rule::terminal(10), Rule::non_terminal(2),])
                ),
                Variable::auxiliary(
                    "rule0_repeat1",
                    Rule::choice(vec![
                        Rule::seq(vec![Rule::non_terminal(1), Rule::non_terminal(1),]),
                        Rule::terminal(12),
                    ])
                ),
                Variable::auxiliary(
                    "rule0_repeat2",
                    Rule::choice(vec![
                        Rule::seq(vec![Rule::non_terminal(2), Rule::non_terminal(2),]),
                        Rule::seq(vec![Rule::terminal(11), Rule::non_terminal(1),]),
                    ])
                ),
            ]
        );
    }

    #[test]
    fn test_expansion_of_repeats_at_top_of_hidden_rules() {
        let grammar = expand_repeats(build_grammar(vec![
            Variable::named("rule0", Rule::non_terminal(1)),
            Variable::hidden(
                "_rule1",
                Rule::repeat(Rule::choice(vec![Rule::terminal(11), Rule::terminal(12)])),
            ),
        ]));

        assert_eq!(
            grammar.variables,
            vec![
                Variable::named("rule0", Rule::non_terminal(1),),
                Variable::auxiliary(
                    "_rule1",
                    Rule::choice(vec![
                        Rule::seq(vec![Rule::non_terminal(1), Rule::non_terminal(1)]),
                        Rule::terminal(11),
                        Rule::terminal(12),
                    ]),
                ),
            ]
        );
    }

    fn build_grammar(variables: Vec<Variable>) -> ExtractedSyntaxGrammar {
        ExtractedSyntaxGrammar {
            variables,
            ..Default::default()
        }
    }
}
