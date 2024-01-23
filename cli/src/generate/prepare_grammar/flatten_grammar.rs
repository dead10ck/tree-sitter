use super::ExtractedSyntaxGrammar;
use crate::generate::grammars::{
    Production, ProductionStep, SyntaxGrammar, SyntaxVariable, Variable,
};
use crate::generate::rules::{Alias, Associativity, Precedence, Rule, RuleType, Symbol};
use anyhow::{anyhow, Result};

struct RuleFlattener {
    production: Production,
    precedence_stack: Vec<Precedence>,
    associativity_stack: Vec<Associativity>,
    alias_stack: Vec<Alias>,
    field_name_stack: Vec<String>,
}

impl RuleFlattener {
    fn new() -> Self {
        Self {
            production: Production {
                steps: Vec::new(),
                dynamic_precedence: 0,
            },
            precedence_stack: Vec::new(),
            associativity_stack: Vec::new(),
            alias_stack: Vec::new(),
            field_name_stack: Vec::new(),
        }
    }

    fn flatten(mut self, rule: Rule) -> Production {
        self.apply(rule, true);
        self.production
    }

    fn apply(&mut self, rule: Rule, at_end: bool) -> bool {
        let params = rule.params;
        let mut has_precedence = false;
        let mut has_associativity = false;
        let mut has_alias = false;
        let mut has_field_name = false;

        if let Some(params) = params {
            if !params.precedence.is_none() {
                has_precedence = true;
                self.precedence_stack.push(params.precedence);
            }

            if let Some(associativity) = params.associativity {
                has_associativity = true;
                self.associativity_stack.push(associativity);
            }

            if let Some(alias) = params.alias {
                has_alias = true;
                self.alias_stack.push(alias);
            }

            if let Some(field_name) = params.field_name {
                has_field_name = true;
                self.field_name_stack.push(field_name);
            }

            if params.dynamic_precedence.abs() > self.production.dynamic_precedence.abs() {
                self.production.dynamic_precedence = params.dynamic_precedence;
            }
        }

        let did_push = match rule.kind {
            RuleType::Seq(members) => {
                let mut result = false;
                let last_index = members.len() - 1;

                for (i, member) in members.into_iter().enumerate() {
                    result |= self.apply(member, i == last_index && at_end);
                }

                result
            }
            RuleType::Symbol(symbol) => {
                self.production.steps.push(ProductionStep {
                    symbol,
                    precedence: self
                        .precedence_stack
                        .last()
                        .cloned()
                        .unwrap_or(Precedence::None),
                    associativity: self.associativity_stack.last().cloned(),
                    alias: self.alias_stack.last().cloned(),
                    field_name: self.field_name_stack.last().cloned(),
                });
                true
            }
            _ => false,
        };

        if has_precedence {
            self.precedence_stack.pop();
            if did_push && !at_end {
                self.production.steps.last_mut().unwrap().precedence = self
                    .precedence_stack
                    .last()
                    .cloned()
                    .unwrap_or(Precedence::None);
            }
        }

        if has_associativity {
            self.associativity_stack.pop();
            if did_push && !at_end {
                self.production.steps.last_mut().unwrap().associativity =
                    self.associativity_stack.last().cloned();
            }
        }

        if has_alias {
            self.alias_stack.pop();
        }

        if has_field_name {
            self.field_name_stack.pop();
        }

        did_push
    }
}

fn extract_choices(rule: Rule) -> Vec<Rule> {
    let input_params = rule.params;

    match rule.kind {
        RuleType::Seq(elements) => {
            let mut result = vec![Rule::blank()];

            for element in elements {
                let extraction = extract_choices(element);
                let mut next_result = Vec::new();

                for entry in result {
                    for extraction_entry in extraction.iter() {
                        next_result.push(Rule::seq(vec![entry.clone(), extraction_entry.clone()]));
                    }
                }

                result = next_result;
            }

            // each top-level rule should inherit the params
            result
                .into_iter()
                .map(|elt| {
                    if let Some(input_params) = &input_params {
                        elt.add_metadata(|params| {
                            *params = input_params.clone();
                        })
                    } else {
                        elt
                    }
                })
                .collect()
        }
        RuleType::Choice(elements) => {
            let mut result = Vec::new();

            for elt in elements {
                for rule in extract_choices(elt) {
                    result.push(rule);
                }
            }

            result
                .into_iter()
                .map(|elt| match &input_params {
                    Some(input_params) if input_params.is_immediate => {
                        elt.add_metadata(|params| params.is_immediate |= input_params.is_immediate)
                    }
                    _ => elt,
                })
                .collect()
        }
        kind => vec![Rule {
            kind,
            params: input_params,
        }],
    }
}

fn flatten_variable(variable: Variable) -> Result<SyntaxVariable> {
    let mut productions = Vec::new();
    for rule in extract_choices(variable.rule) {
        let production = RuleFlattener::new().flatten(rule);
        if !productions.contains(&production) {
            productions.push(production);
        }
    }
    Ok(SyntaxVariable {
        name: variable.name,
        kind: variable.kind,
        productions,
    })
}

fn symbol_is_used(variables: &Vec<SyntaxVariable>, symbol: Symbol) -> bool {
    for variable in variables {
        for production in &variable.productions {
            for step in &production.steps {
                if step.symbol == symbol {
                    return true;
                }
            }
        }
    }
    false
}

pub(super) fn flatten_grammar(grammar: ExtractedSyntaxGrammar) -> Result<SyntaxGrammar> {
    let mut variables = Vec::new();
    for variable in grammar.variables {
        variables.push(flatten_variable(variable)?);
    }
    for (i, variable) in variables.iter().enumerate() {
        for production in &variable.productions {
            if production.steps.is_empty() && symbol_is_used(&variables, Symbol::non_terminal(i)) {
                return Err(anyhow!(
                    "The rule `{}` matches the empty string.

Tree-sitter does not support syntactic rules that match the empty string
unless they are used only as the grammar's start rule.
",
                    variable.name
                ));
            }
        }
    }
    Ok(SyntaxGrammar {
        extra_symbols: grammar.extra_symbols,
        expected_conflicts: grammar.expected_conflicts,
        variables_to_inline: grammar.variables_to_inline,
        precedence_orderings: grammar.precedence_orderings,
        external_tokens: grammar.external_tokens,
        supertype_symbols: grammar.supertype_symbols,
        word_token: grammar.word_token,
        variables,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::generate::grammars::VariableType;
    use crate::generate::rules::Symbol;

    #[test]
    fn test_flatten_grammar() {
        let result = flatten_variable(Variable {
            name: "test".to_string(),
            kind: VariableType::Named,
            rule: Rule::seq(vec![
                Rule::non_terminal(1),
                Rule::prec_left(
                    Precedence::Integer(101),
                    Rule::seq(vec![
                        Rule::non_terminal(2),
                        Rule::choice(vec![
                            Rule::prec_right(
                                Precedence::Integer(102),
                                Rule::seq(vec![Rule::non_terminal(3), Rule::non_terminal(4)]),
                            ),
                            Rule::non_terminal(5),
                        ]),
                        Rule::non_terminal(6),
                    ]),
                ),
                Rule::non_terminal(7),
            ]),
        })
        .unwrap();

        assert_eq!(
            result.productions,
            vec![
                Production {
                    dynamic_precedence: 0,
                    steps: vec![
                        ProductionStep::new(Symbol::non_terminal(1)),
                        ProductionStep::new(Symbol::non_terminal(2))
                            .with_prec(Precedence::Integer(101), Some(Associativity::Left)),
                        ProductionStep::new(Symbol::non_terminal(3))
                            .with_prec(Precedence::Integer(102), Some(Associativity::Right)),
                        ProductionStep::new(Symbol::non_terminal(4))
                            .with_prec(Precedence::Integer(101), Some(Associativity::Left)),
                        ProductionStep::new(Symbol::non_terminal(6)),
                        ProductionStep::new(Symbol::non_terminal(7)),
                    ]
                },
                Production {
                    dynamic_precedence: 0,
                    steps: vec![
                        ProductionStep::new(Symbol::non_terminal(1)),
                        ProductionStep::new(Symbol::non_terminal(2))
                            .with_prec(Precedence::Integer(101), Some(Associativity::Left)),
                        ProductionStep::new(Symbol::non_terminal(5))
                            .with_prec(Precedence::Integer(101), Some(Associativity::Left)),
                        ProductionStep::new(Symbol::non_terminal(6)),
                        ProductionStep::new(Symbol::non_terminal(7)),
                    ]
                },
            ]
        );
    }

    #[test]
    fn test_flatten_grammar_with_maximum_dynamic_precedence() {
        let result = flatten_variable(Variable {
            name: "test".to_string(),
            kind: VariableType::Named,
            rule: Rule::seq(vec![
                Rule::non_terminal(1),
                Rule::prec_dynamic(
                    101,
                    Rule::seq(vec![
                        Rule::non_terminal(2),
                        Rule::choice(vec![
                            Rule::prec_dynamic(
                                102,
                                Rule::seq(vec![Rule::non_terminal(3), Rule::non_terminal(4)]),
                            ),
                            Rule::non_terminal(5),
                        ]),
                        Rule::non_terminal(6),
                    ]),
                ),
                Rule::non_terminal(7),
            ]),
        })
        .unwrap();

        assert_eq!(
            result.productions,
            vec![
                Production {
                    dynamic_precedence: 102,
                    steps: vec![
                        ProductionStep::new(Symbol::non_terminal(1)),
                        ProductionStep::new(Symbol::non_terminal(2)),
                        ProductionStep::new(Symbol::non_terminal(3)),
                        ProductionStep::new(Symbol::non_terminal(4)),
                        ProductionStep::new(Symbol::non_terminal(6)),
                        ProductionStep::new(Symbol::non_terminal(7)),
                    ],
                },
                Production {
                    dynamic_precedence: 101,
                    steps: vec![
                        ProductionStep::new(Symbol::non_terminal(1)),
                        ProductionStep::new(Symbol::non_terminal(2)),
                        ProductionStep::new(Symbol::non_terminal(5)),
                        ProductionStep::new(Symbol::non_terminal(6)),
                        ProductionStep::new(Symbol::non_terminal(7)),
                    ],
                },
            ]
        );
    }

    #[test]
    fn test_flatten_grammar_with_final_precedence() {
        let result = flatten_variable(Variable {
            name: "test".to_string(),
            kind: VariableType::Named,
            rule: Rule::prec_left(
                Precedence::Integer(101),
                Rule::seq(vec![Rule::non_terminal(1), Rule::non_terminal(2)]),
            ),
        })
        .unwrap();

        assert_eq!(
            result.productions,
            vec![Production {
                dynamic_precedence: 0,
                steps: vec![
                    ProductionStep::new(Symbol::non_terminal(1))
                        .with_prec(Precedence::Integer(101), Some(Associativity::Left)),
                    ProductionStep::new(Symbol::non_terminal(2))
                        .with_prec(Precedence::Integer(101), Some(Associativity::Left)),
                ]
            }]
        );

        let result = flatten_variable(Variable {
            name: "test".to_string(),
            kind: VariableType::Named,
            rule: Rule::prec_left(
                Precedence::Integer(101),
                Rule::seq(vec![Rule::non_terminal(1)]),
            ),
        })
        .unwrap();

        assert_eq!(
            result.productions,
            vec![Production {
                dynamic_precedence: 0,
                steps: vec![ProductionStep::new(Symbol::non_terminal(1))
                    .with_prec(Precedence::Integer(101), Some(Associativity::Left)),]
            }]
        );
    }

    #[test]
    fn test_flatten_grammar_with_field_names() {
        let result = flatten_variable(Variable {
            name: "test".to_string(),
            kind: VariableType::Named,
            rule: Rule::seq(vec![
                Rule::field("first-thing".to_string(), Rule::terminal(1)),
                Rule::terminal(2),
                Rule::choice(vec![
                    Rule::blank(),
                    Rule::field("second-thing".to_string(), Rule::terminal(3)),
                ]),
            ]),
        })
        .unwrap();

        assert_eq!(
            result.productions,
            vec![
                Production {
                    dynamic_precedence: 0,
                    steps: vec![
                        ProductionStep::new(Symbol::terminal(1)).with_field_name("first-thing"),
                        ProductionStep::new(Symbol::terminal(2))
                    ]
                },
                Production {
                    dynamic_precedence: 0,
                    steps: vec![
                        ProductionStep::new(Symbol::terminal(1)).with_field_name("first-thing"),
                        ProductionStep::new(Symbol::terminal(2)),
                        ProductionStep::new(Symbol::terminal(3)).with_field_name("second-thing"),
                    ]
                },
            ]
        );
    }
}
