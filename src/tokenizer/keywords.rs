use super::token_type::TokenType;
use std::collections::HashMap;
use std::sync::OnceLock;

pub struct Keywords {
    map: HashMap<&'static str, TokenType>,
}

impl Keywords {
    fn new() -> Self {
        let mut map = HashMap::new();

        map.insert("int", TokenType::KwInt);
        map.insert("float", TokenType::KwFloat);
        map.insert("double", TokenType::KwDouble);
        map.insert("char", TokenType::KwChar);
        map.insert("string", TokenType::KwString);
        map.insert("bool", TokenType::KwBool);
        map.insert("if", TokenType::KwIf);
        map.insert("else", TokenType::KwElse);
        map.insert("while", TokenType::KwWhile);
        map.insert("for", TokenType::KwFor);
        map.insert("do", TokenType::KwDo);
        map.insert("switch", TokenType::KwSwitch);
        map.insert("case", TokenType::KwCase);
        map.insert("default", TokenType::KwDefault);
        map.insert("break", TokenType::KwBreak);
        map.insert("continue", TokenType::KwContinue);
        map.insert("return", TokenType::KwReturn);

        map.insert("true", TokenType::KwTrue);
        map.insert("false", TokenType::KwFalse);

        map.insert("class", TokenType::KwClass);
        map.insert("constructor", TokenType::KwConstructor);
        map.insert("new", TokenType::KwNew);

        map.insert("import", TokenType::KwImport);
        map.insert("export", TokenType::KwExport);
        map.insert("module", TokenType::KwModule);
        map.insert("from", TokenType::KwFrom);
        map.insert("as", TokenType::KwAs);

        map.insert("print", TokenType::KwPrint);
        map.insert("type", TokenType::KwType);
        map.insert("struct", TokenType::KwStruct);

        Self { map }
    }

    pub fn get(&self, identifier: &str) -> Option<TokenType> {
        self.map.get(identifier).copied()
    }

    pub fn is_keyword(&self, identifier: &str) -> bool {
        self.map.contains_key(identifier)
    }

    pub fn all_keywords(&self) -> impl Iterator<Item = (&str, TokenType)> + '_ {
        self.map.iter().map(|(&k, &v)| (k, v))
    }
}

static KEYWORDS: OnceLock<Keywords> = OnceLock::new();

pub fn keywords() -> &'static Keywords {
    KEYWORDS.get_or_init(Keywords::new)
}

pub fn get_keyword(identifier: &str) -> Option<TokenType> {
    keywords().get(identifier)
}

pub fn is_keyword(identifier: &str) -> bool {
    keywords().is_keyword(identifier)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords_recognition() {
        assert_eq!(get_keyword("int"), Some(TokenType::KwInt));
        assert_eq!(get_keyword("if"), Some(TokenType::KwIf));
        assert_eq!(get_keyword("class"), Some(TokenType::KwClass));
        assert_eq!(get_keyword("not_a_keyword"), None);
    }

    #[test]
    fn test_keyword_check() {
        assert!(is_keyword("int"));
        assert!(is_keyword("class"));
        assert!(!is_keyword("my_variable"));
    }
}