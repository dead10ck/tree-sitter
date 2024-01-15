use super::grammars::VariableType;
use smallbitvec::SmallBitVec;
use std::iter::FromIterator;
use std::{collections::HashMap, fmt};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum SymbolType {
    External,
    End,
    EndOfNonTerminalExtra,
    Terminal,
    NonTerminal,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum Associativity {
    Left,
    Right,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) struct Alias {
    pub value: String,
    pub is_named: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Precedence {
    None,
    Integer(i32),
    Name(String),
}

pub(crate) type AliasMap = HashMap<Symbol, Alias>;

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub(crate) struct MetadataParams {
    pub precedence: Precedence,
    pub dynamic_precedence: i32,
    pub associativity: Option<Associativity>,
    pub is_token: bool,
    pub is_string: bool,
    pub is_active: bool,
    pub is_main_token: bool,
    pub is_immediate: bool,
    pub alias: Option<Alias>,
    pub field_name: Option<String>,
}

impl MetadataParams {
    pub fn default_immediate() -> Self {
        MetadataParams {
            is_immediate: true,
            ..Default::default()
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) struct Symbol {
    pub kind: SymbolType,
    pub index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct Rule {
    pub kind: RuleType,
    pub params: MetadataParams,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum RuleType {
    Blank,
    String(String),
    Pattern(String, String),
    NamedSymbol(String),
    Symbol(Symbol),
    Choice(Vec<Rule>),
    Repeat(Box<Rule>),
    Seq(Vec<Rule>),
}

// Because tokens are represented as small (~400 max) unsigned integers,
// sets of tokens can be efficiently represented as bit vectors with each
// index corresponding to a token, and each value representing whether or not
// the token is present in the set.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct TokenSet {
    terminal_bits: SmallBitVec,
    external_bits: SmallBitVec,
    eof: bool,
    end_of_nonterminal_extra: bool,
}

impl From<RuleType> for Rule {
    fn from(kind: RuleType) -> Self {
        Rule {
            kind,
            params: MetadataParams::default(),
        }
    }
}

impl From<&Rule> for Rule {
    fn from(rule: &Rule) -> Self {
        rule.clone()
    }
}

impl Rule {
    pub fn field(name: String, mut rule: Rule) -> Self {
        rule.params.field_name = Some(name);
        rule
    }

    pub fn alias(mut rule: Rule, value: String, is_named: bool) -> Self {
        rule.params.alias = Some(Alias { is_named, value });
        rule
    }

    pub fn token(mut rule: Rule) -> Self {
        rule.params.is_token = true;
        rule
    }

    pub fn immediate_token(mut rule: Rule) -> Self {
        rule.params.is_token = true;
        rule.params.is_main_token = true;
        rule.params.is_immediate = true;
        rule
    }

    pub fn immediate(mut rule: Rule) -> Self {
        rule.params.is_immediate = true;
        rule
    }

    pub fn is_immediate(&self) -> bool {
        self.params.is_immediate
    }

    pub fn prec(value: Precedence, mut rule: Rule) -> Self {
        rule.params.precedence = value;
        rule
    }

    pub fn prec_left(value: Precedence, mut rule: Rule) -> Self {
        rule.params.associativity = Some(Associativity::Left);
        rule.params.precedence = value;
        rule
    }

    pub fn prec_right(value: Precedence, mut rule: Rule) -> Self {
        rule.params.associativity = Some(Associativity::Right);
        rule.params.precedence = value;
        rule
    }

    pub fn prec_dynamic(value: i32, mut rule: Rule) -> Self {
        rule.params.dynamic_precedence = value;
        rule
    }

    pub fn blank() -> Self {
        Rule {
            kind: RuleType::Blank,
            params: Default::default(),
        }
    }

    pub fn terminal(index: usize) -> Self {
        RuleType::Symbol(Symbol::terminal(index)).into()
    }

    pub fn non_terminal(index: usize) -> Self {
        RuleType::Symbol(Symbol::non_terminal(index)).into()
    }

    pub fn external(index: usize) -> Self {
        RuleType::Symbol(Symbol::external(index)).into()
    }

    pub fn named<S: ToString>(name: S) -> Self {
        RuleType::NamedSymbol(name.to_string()).into()
    }

    pub fn string<S: ToString>(value: S) -> Self {
        RuleType::String(value.to_string()).into()
    }

    pub fn pattern<S: ToString>(value: S, flags: S) -> Self {
        RuleType::Pattern(value.to_string(), flags.to_string()).into()
    }

    pub fn repeat(mut rule: Rule) -> Self {
        RuleType::Repeat(Box::new(rule)).into()
    }

    pub fn choice(rules: Vec<Rule>) -> Self {
        let mut elements = Vec::with_capacity(rules.len());

        for rule in rules {
            choice_helper(&mut elements, rule);
        }

        RuleType::Choice(elements).into()
    }

    pub fn seq(rules: Vec<Rule>) -> Self {
        RuleType::Seq(rules).into()
    }
}

impl Alias {
    pub fn kind(&self) -> VariableType {
        if self.is_named {
            VariableType::Named
        } else {
            VariableType::Anonymous
        }
    }
}

impl Precedence {
    pub fn is_none(&self) -> bool {
        matches!(self, Precedence::None)
    }
}

impl Symbol {
    pub fn is_terminal(&self) -> bool {
        self.kind == SymbolType::Terminal
    }

    pub fn is_non_terminal(&self) -> bool {
        self.kind == SymbolType::NonTerminal
    }

    pub fn is_external(&self) -> bool {
        self.kind == SymbolType::External
    }

    pub fn is_eof(&self) -> bool {
        self.kind == SymbolType::End
    }

    pub fn non_terminal(index: usize) -> Self {
        Symbol {
            kind: SymbolType::NonTerminal,
            index,
        }
    }

    pub fn terminal(index: usize) -> Self {
        Symbol {
            kind: SymbolType::Terminal,
            index,
        }
    }

    pub fn external(index: usize) -> Self {
        Symbol {
            kind: SymbolType::External,
            index,
        }
    }

    pub fn end() -> Self {
        Symbol {
            kind: SymbolType::End,
            index: 0,
        }
    }

    pub fn end_of_nonterminal_extra() -> Self {
        Symbol {
            kind: SymbolType::EndOfNonTerminalExtra,
            index: 0,
        }
    }
}

impl From<Symbol> for Rule {
    fn from(symbol: Symbol) -> Self {
        RuleType::Symbol(symbol).into()
    }
}

impl From<Symbol> for RuleType {
    fn from(symbol: Symbol) -> Self {
        RuleType::Symbol(symbol)
    }
}

impl TokenSet {
    pub fn new() -> Self {
        Self {
            terminal_bits: SmallBitVec::new(),
            external_bits: SmallBitVec::new(),
            eof: false,
            end_of_nonterminal_extra: false,
        }
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = Symbol> + 'a {
        self.terminal_bits
            .iter()
            .enumerate()
            .filter_map(|(i, value)| {
                if value {
                    Some(Symbol::terminal(i))
                } else {
                    None
                }
            })
            .chain(
                self.external_bits
                    .iter()
                    .enumerate()
                    .filter_map(|(i, value)| {
                        if value {
                            Some(Symbol::external(i))
                        } else {
                            None
                        }
                    }),
            )
            .chain(if self.eof { Some(Symbol::end()) } else { None })
            .chain(if self.end_of_nonterminal_extra {
                Some(Symbol::end_of_nonterminal_extra())
            } else {
                None
            })
    }

    pub fn terminals<'a>(&'a self) -> impl Iterator<Item = Symbol> + 'a {
        self.terminal_bits
            .iter()
            .enumerate()
            .filter_map(|(i, value)| {
                if value {
                    Some(Symbol::terminal(i))
                } else {
                    None
                }
            })
    }

    pub fn contains(&self, symbol: &Symbol) -> bool {
        match symbol.kind {
            SymbolType::NonTerminal => panic!("Cannot store non-terminals in a TokenSet"),
            SymbolType::Terminal => self.terminal_bits.get(symbol.index).unwrap_or(false),
            SymbolType::External => self.external_bits.get(symbol.index).unwrap_or(false),
            SymbolType::End => self.eof,
            SymbolType::EndOfNonTerminalExtra => self.end_of_nonterminal_extra,
        }
    }

    pub fn contains_terminal(&self, index: usize) -> bool {
        self.terminal_bits.get(index).unwrap_or(false)
    }

    pub fn insert(&mut self, other: Symbol) {
        let vec = match other.kind {
            SymbolType::NonTerminal => panic!("Cannot store non-terminals in a TokenSet"),
            SymbolType::Terminal => &mut self.terminal_bits,
            SymbolType::External => &mut self.external_bits,
            SymbolType::End => {
                self.eof = true;
                return;
            }
            SymbolType::EndOfNonTerminalExtra => {
                self.end_of_nonterminal_extra = true;
                return;
            }
        };
        if other.index >= vec.len() {
            vec.resize(other.index + 1, false);
        }
        vec.set(other.index, true);
    }

    pub fn remove(&mut self, other: &Symbol) -> bool {
        let vec = match other.kind {
            SymbolType::NonTerminal => panic!("Cannot store non-terminals in a TokenSet"),
            SymbolType::Terminal => &mut self.terminal_bits,
            SymbolType::External => &mut self.external_bits,
            SymbolType::End => {
                return if self.eof {
                    self.eof = false;
                    true
                } else {
                    false
                }
            }
            SymbolType::EndOfNonTerminalExtra => {
                return if self.end_of_nonterminal_extra {
                    self.end_of_nonterminal_extra = false;
                    true
                } else {
                    false
                };
            }
        };

        if other.index < vec.len() && vec[other.index] {
            vec.set(other.index, false);
            return true;
        }

        false
    }

    pub fn is_empty(&self) -> bool {
        !self.eof
            && !self.end_of_nonterminal_extra
            && !self.terminal_bits.iter().any(|a| a)
            && !self.external_bits.iter().any(|a| a)
    }

    pub fn insert_all_terminals(&mut self, other: &TokenSet) -> bool {
        let mut result = false;
        if other.terminal_bits.len() > self.terminal_bits.len() {
            self.terminal_bits.resize(other.terminal_bits.len(), false);
        }
        for (i, element) in other.terminal_bits.iter().enumerate() {
            if element {
                result |= !self.terminal_bits[i];
                self.terminal_bits.set(i, element);
            }
        }
        result
    }

    fn insert_all_externals(&mut self, other: &TokenSet) -> bool {
        let mut result = false;
        if other.external_bits.len() > self.external_bits.len() {
            self.external_bits.resize(other.external_bits.len(), false);
        }
        for (i, element) in other.external_bits.iter().enumerate() {
            if element {
                result |= !self.external_bits[i];
                self.external_bits.set(i, element);
            }
        }
        result
    }

    pub fn insert_all(&mut self, other: &TokenSet) -> bool {
        let mut result = false;
        if other.eof {
            result |= !self.eof;
            self.eof = true;
        }
        if other.end_of_nonterminal_extra {
            result |= !self.end_of_nonterminal_extra;
            self.end_of_nonterminal_extra = true;
        }
        result |= self.insert_all_terminals(other);
        result |= self.insert_all_externals(other);
        result
    }
}

impl FromIterator<Symbol> for TokenSet {
    fn from_iter<T: IntoIterator<Item = Symbol>>(iter: T) -> Self {
        let mut result = Self::new();
        for symbol in iter {
            result.insert(symbol);
        }
        result
    }
}

fn choice_helper(result: &mut Vec<Rule>, rule: Rule) {
    match rule.kind {
        RuleType::Choice(elements) => {
            for element in elements {
                choice_helper(result, element);
            }
        }
        _ => {
            if !result.contains(&rule) {
                result.push(rule);
            }
        }
    }
}

impl fmt::Display for Precedence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Precedence::Integer(i) => write!(f, "{}", i),
            Precedence::Name(s) => write!(f, "'{}'", s),
            Precedence::None => write!(f, "none"),
        }
    }
}

impl Default for Precedence {
    fn default() -> Self {
        Precedence::None
    }
}
