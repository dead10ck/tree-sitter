use super::{ExtractedLexicalGrammar, ExtractedSyntaxGrammar, InternedGrammar};
use crate::generate::grammars::{ExternalToken, Variable, VariableType};
use crate::generate::rules::{Rule, RuleType, Symbol, SymbolType};
use anyhow::{anyhow, Result};
use std::collections::HashMap;
use std::mem;

pub(super) fn extract_tokens(
    mut grammar: InternedGrammar,
) -> Result<(ExtractedSyntaxGrammar, ExtractedLexicalGrammar)> {
    let mut extractor = TokenExtractor {
        current_variable_name: String::new(),
        current_variable_token_count: 0,
        extracted_variables: Vec::new(),
        extracted_usage_counts: Vec::new(),
    };

    for variable in grammar.variables.iter_mut() {
        extractor.extract_tokens_in_variable(variable);
    }

    for variable in grammar.external_tokens.iter_mut() {
        extractor.extract_tokens_in_variable(variable);
    }

    let mut lexical_variables = Vec::with_capacity(extractor.extracted_variables.len());
    for variable in extractor.extracted_variables {
        lexical_variables.push(Variable {
            name: variable.name,
            kind: variable.kind,
            rule: variable.rule,
        });
    }

    // If a variable's entire rule was extracted as a token and that token didn't
    // appear within any other rule, then remove that variable from the syntax
    // grammar, giving its name to the token in the lexical grammar. Any symbols
    // that pointed to that variable will need to be updated to point to the
    // variable in the lexical grammar. Symbols that pointed to later variables
    // will need to have their indices decremented.
    let mut variables = Vec::new();
    let mut symbol_replacer = SymbolReplacer {
        replacements: HashMap::new(),
    };
    for (i, variable) in grammar.variables.into_iter().enumerate() {
        if let RuleType::Symbol(Symbol {
            kind: SymbolType::Terminal,
            index,
        }) = variable.rule.kind
        {
            if i > 0 && extractor.extracted_usage_counts[index] == 1 {
                let lexical_variable = &mut lexical_variables[index];
                lexical_variable.kind = variable.kind;
                lexical_variable.name = variable.name;
                symbol_replacer.replacements.insert(i, index);
                continue;
            }
        }
        variables.push(variable);
    }

    for variable in variables.iter_mut() {
        variable.rule = symbol_replacer.replace_symbols_in_rule(variable.rule.clone());
    }

    let expected_conflicts = grammar
        .expected_conflicts
        .into_iter()
        .map(|conflict| {
            let mut result: Vec<_> = conflict
                .iter()
                .map(|symbol| symbol_replacer.replace_symbol(*symbol))
                .collect();
            result.sort_unstable();
            result.dedup();
            result
        })
        .collect();

    let supertype_symbols = grammar
        .supertype_symbols
        .into_iter()
        .map(|symbol| symbol_replacer.replace_symbol(symbol))
        .collect();

    let variables_to_inline = grammar
        .variables_to_inline
        .into_iter()
        .map(|symbol| symbol_replacer.replace_symbol(symbol))
        .collect();

    let mut separators = Vec::new();
    let mut extra_symbols = Vec::new();

    for rule in grammar.extra_symbols {
        if let RuleType::Symbol(symbol) = rule.kind {
            extra_symbols.push(symbol_replacer.replace_symbol(symbol));
        } else if let Some(index) = lexical_variables.iter().position(|v| v.rule == rule) {
            extra_symbols.push(Symbol::terminal(index));
        } else {
            separators.push(rule);
        }
    }

    let mut external_tokens = Vec::new();
    for external_token in grammar.external_tokens {
        let rule = symbol_replacer.replace_symbols_in_rule(external_token.rule);
        if let RuleType::Symbol(symbol) = rule.kind {
            if symbol.is_non_terminal() {
                return Err(anyhow!(
                    "Rule '{}' cannot be used as both an external token and a non-terminal rule",
                    &variables[symbol.index].name,
                ));
            }

            if symbol.is_external() {
                external_tokens.push(ExternalToken {
                    name: external_token.name,
                    kind: external_token.kind,
                    corresponding_internal_token: None,
                })
            } else {
                external_tokens.push(ExternalToken {
                    name: lexical_variables[symbol.index].name.clone(),
                    kind: external_token.kind,
                    corresponding_internal_token: Some(symbol),
                })
            }
        } else {
            return Err(anyhow!(
                "Non-symbol rules cannot be used as external tokens"
            ));
        }
    }

    let mut word_token = None;
    if let Some(token) = grammar.word_token {
        let token = symbol_replacer.replace_symbol(token);
        if token.is_non_terminal() {
            return Err(anyhow!(
                "Non-terminal symbol '{}' cannot be used as the word token",
                &variables[token.index].name
            ));
        }
        word_token = Some(token);
    }

    Ok((
        ExtractedSyntaxGrammar {
            variables,
            expected_conflicts,
            extra_symbols,
            variables_to_inline,
            supertype_symbols,
            external_tokens,
            word_token,
            precedence_orderings: grammar.precedence_orderings,
        },
        ExtractedLexicalGrammar {
            variables: lexical_variables,
            separators,
        },
    ))
}

struct TokenExtractor {
    current_variable_name: String,
    current_variable_token_count: usize,
    extracted_variables: Vec<Variable>,
    extracted_usage_counts: Vec<usize>,
}

struct SymbolReplacer {
    replacements: HashMap<usize, usize>,
}

impl TokenExtractor {
    fn extract_tokens_in_variable(&mut self, variable: &mut Variable) {
        self.current_variable_name.clear();
        self.current_variable_name.push_str(&variable.name);
        self.current_variable_token_count = 0;
        let mut rule = Rule::blank();
        mem::swap(&mut rule, &mut variable.rule);
        variable.rule = self.extract_tokens_in_rule(rule);
    }

    fn extract_tokens_in_rule(&mut self, input: Rule) -> Rule {
        let is_immediate = input.is_immediate();

        if is_immediate {
            println!("rule: {:?}", input);
        }

        if input.params.is_token {
            return self.extract_token(input).into();
        }

        let kind = match input.kind {
            RuleType::String(_) | RuleType::Pattern(..) => {
                RuleType::Symbol(self.extract_token(input.clone()))
            }
            RuleType::Repeat(content) => {
                RuleType::Repeat(Box::new(self.extract_tokens_in_rule(*content)))
            }
            RuleType::Seq(elements) => RuleType::Seq(
                elements
                    .into_iter()
                    .enumerate()
                    .map(|(i, elt)| {
                        // for an immediate sequence, only the first rule must
                        // be immediate
                        let elt = if i == 0 && is_immediate {
                            Rule::immediate(elt)
                        } else {
                            elt
                        };

                        self.extract_tokens_in_rule(elt)
                    })
                    .collect(),
            ),
            RuleType::Choice(elements) => RuleType::Choice(
                elements
                    .into_iter()
                    .map(|elt| {
                        // apply immediate to all choices
                        let elt = if is_immediate {
                            Rule::immediate(elt)
                        } else {
                            elt
                        };

                        self.extract_tokens_in_rule(elt)
                    })
                    .collect(),
            ),
            kind => kind,
        };

        Rule {
            kind,
            params: input.params,
        }
    }

    fn extract_token(&mut self, rule: Rule) -> Symbol {
        for (i, variable) in self.extracted_variables.iter_mut().enumerate() {
            if variable.rule == rule {
                self.extracted_usage_counts[i] += 1;
                return Symbol::terminal(i);
            }
        }

        let string_value = match &rule.kind {
            RuleType::String(value) => Some(value),
            _ => None,
        };

        let index = self.extracted_variables.len();

        let variable = if let Some(string_value) = string_value {
            Variable {
                name: string_value.clone(),
                kind: VariableType::Anonymous,
                rule,
            }
        } else {
            self.current_variable_token_count += 1;
            Variable {
                name: format!(
                    "{}_token{}",
                    &self.current_variable_name, self.current_variable_token_count
                ),
                kind: VariableType::Auxiliary,
                rule,
            }
        };

        self.extracted_variables.push(variable);
        self.extracted_usage_counts.push(1);
        Symbol::terminal(index)
    }
}

impl SymbolReplacer {
    fn replace_symbols_in_rule(&mut self, rule: Rule) -> Rule {
        let kind = match rule.kind {
            RuleType::Symbol(symbol) => self.replace_symbol(symbol).into(),
            RuleType::Choice(elements) => RuleType::Choice(
                elements
                    .into_iter()
                    .map(|e| self.replace_symbols_in_rule(e))
                    .collect(),
            ),
            RuleType::Seq(elements) => RuleType::Seq(
                elements
                    .into_iter()
                    .map(|e| self.replace_symbols_in_rule(e))
                    .collect(),
            ),
            RuleType::Repeat(content) => {
                RuleType::Repeat(Box::new(self.replace_symbols_in_rule(*content)))
            }
            kind => kind,
        };

        Rule {
            kind,
            params: rule.params,
        }
    }

    fn replace_symbol(&self, symbol: Symbol) -> Symbol {
        if !symbol.is_non_terminal() {
            return symbol;
        }

        if let Some(replacement) = self.replacements.get(&symbol.index) {
            return Symbol::terminal(*replacement);
        }

        let mut adjusted_index = symbol.index;
        for (replaced_index, _) in self.replacements.iter() {
            if *replaced_index < symbol.index {
                adjusted_index -= 1;
            }
        }

        Symbol::non_terminal(adjusted_index)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::generate::grammars::VariableType;

    #[test]
    fn test_extraction() {
        let (syntax_grammar, lexical_grammar) = extract_tokens(build_grammar(vec![
            Variable::named(
                "rule_0",
                Rule::repeat(Rule::seq(vec![
                    Rule::string("a"),
                    Rule::pattern("b", ""),
                    Rule::choice(vec![
                        Rule::non_terminal(1),
                        Rule::non_terminal(2),
                        Rule::token(Rule::repeat(Rule::choice(vec![
                            Rule::string("c"),
                            Rule::string("d"),
                        ]))),
                    ]),
                ])),
            ),
            Variable::named("rule_1", Rule::pattern("e", "")),
            Variable::named("rule_2", Rule::pattern("b", "")),
            Variable::named(
                "rule_3",
                Rule::seq(vec![Rule::non_terminal(2), Rule::blank()]),
            ),
        ]))
        .unwrap();

        assert_eq!(
            syntax_grammar.variables,
            vec![
                Variable::named(
                    "rule_0",
                    Rule::repeat(Rule::seq(vec![
                        // The string "a" was replaced by a symbol referencing the lexical grammar
                        Rule::terminal(0),
                        // The pattern "b" was replaced by a symbol referencing the lexical grammar
                        Rule::terminal(1),
                        Rule::choice(vec![
                            // The symbol referencing `rule_1` was replaced by a symbol referencing
                            // the lexical grammar.
                            Rule::terminal(3),
                            // The symbol referencing `rule_2` had its index decremented because
                            // `rule_1` was moved to the lexical grammar.
                            Rule::non_terminal(1),
                            // The rule wrapped in `token` was replaced by a symbol referencing
                            // the lexical grammar.
                            Rule::terminal(2),
                        ])
                    ]))
                ),
                // The pattern "e" was only used in once place: as the definition of `rule_1`,
                // so that rule was moved to the lexical grammar. The pattern "b" appeared in
                // two places, so it was not moved into the lexical grammar.
                Variable::named("rule_2", Rule::terminal(1)),
                Variable::named(
                    "rule_3",
                    Rule::seq(vec![Rule::non_terminal(1), Rule::blank(),])
                ),
            ]
        );

        assert_eq!(
            lexical_grammar.variables,
            vec![
                Variable::anonymous("a", Rule::string("a")),
                Variable::auxiliary("rule_0_token1", Rule::pattern("b", "")),
                Variable::auxiliary(
                    "rule_0_token2",
                    Rule::repeat(Rule::choice(vec![Rule::string("c"), Rule::string("d"),]))
                ),
                Variable::named("rule_1", Rule::pattern("e", "")),
            ]
        );
    }

    #[test]
    fn test_start_rule_is_token() {
        let (syntax_grammar, lexical_grammar) =
            extract_tokens(build_grammar(vec![Variable::named(
                "rule_0",
                Rule::string("hello"),
            )]))
            .unwrap();

        assert_eq!(
            syntax_grammar.variables,
            vec![Variable::named("rule_0", Rule::terminal(0)),]
        );
        assert_eq!(
            lexical_grammar.variables,
            vec![Variable::anonymous("hello", Rule::string("hello")),]
        )
    }

    #[test]
    fn test_extracting_extra_symbols() {
        let mut grammar = build_grammar(vec![
            Variable::named("rule_0", Rule::string("x")),
            Variable::named("comment", Rule::pattern("//.*", "")),
        ]);
        grammar.extra_symbols = vec![Rule::string(" "), Rule::non_terminal(1)];

        let (syntax_grammar, lexical_grammar) = extract_tokens(grammar).unwrap();
        assert_eq!(syntax_grammar.extra_symbols, vec![Symbol::terminal(1),]);
        assert_eq!(lexical_grammar.separators, vec![Rule::string(" "),]);
    }

    #[test]
    fn test_extract_externals() {
        let mut grammar = build_grammar(vec![
            Variable::named(
                "rule_0",
                Rule::seq(vec![
                    Rule::external(0),
                    Rule::string("a"),
                    Rule::non_terminal(1),
                    Rule::non_terminal(2),
                ]),
            ),
            Variable::named("rule_1", Rule::string("b")),
            Variable::named("rule_2", Rule::string("c")),
        ]);
        grammar.external_tokens = vec![
            Variable::named("external_0", Rule::external(0)),
            Variable::anonymous("a", Rule::string("a")),
            Variable::named("rule_2", Rule::non_terminal(2)),
        ];

        let (syntax_grammar, _) = extract_tokens(grammar).unwrap();

        assert_eq!(
            syntax_grammar.external_tokens,
            vec![
                ExternalToken {
                    name: "external_0".to_string(),
                    kind: VariableType::Named,
                    corresponding_internal_token: None,
                },
                ExternalToken {
                    name: "a".to_string(),
                    kind: VariableType::Anonymous,
                    corresponding_internal_token: Some(Symbol::terminal(0)),
                },
                ExternalToken {
                    name: "rule_2".to_string(),
                    kind: VariableType::Named,
                    corresponding_internal_token: Some(Symbol::terminal(2)),
                },
            ]
        );
    }

    #[test]
    fn test_error_on_external_with_same_name_as_non_terminal() {
        let mut grammar = build_grammar(vec![
            Variable::named(
                "rule_0",
                Rule::seq(vec![Rule::non_terminal(1), Rule::non_terminal(2)]),
            ),
            Variable::named(
                "rule_1",
                Rule::seq(vec![Rule::non_terminal(2), Rule::non_terminal(2)]),
            ),
            Variable::named("rule_2", Rule::string("a")),
        ]);
        grammar.external_tokens = vec![Variable::named("rule_1", Rule::non_terminal(1))];

        match extract_tokens(grammar) {
            Err(e) => {
                assert_eq!(e.to_string(), "Rule 'rule_1' cannot be used as both an external token and a non-terminal rule");
            }
            _ => {
                panic!("Expected an error but got no error");
            }
        }
    }

    fn build_grammar(variables: Vec<Variable>) -> InternedGrammar {
        InternedGrammar {
            variables,
            ..Default::default()
        }
    }
}
