use super::strings::*;
use super::Parser;

#[derive(Clone)]
pub (crate) enum GrammarToken {
    Name(String),
    OptionalName(String),
    OptionalNameList(String),
    SpecialNameList{text: String, subtype: String},
    SeparatorNameList{text: String, separator: String},
    Plain(String),
    Regex(String),
    RestIsOptional
}

#[derive(Clone)]
pub (crate) struct GrammarForm {
    pub (crate) tokens : Vec<GrammarToken>,
}

#[allow(dead_code)]
pub (crate) fn default_grammar() -> &'static str
{
    include_str!("defaultgrammar.txt")
}

fn minierr(mystr : &str) -> String
{
    mystr.to_string()
}

fn plainerr<T>(mystr : &str) -> Result<T, String>
{
    Err(minierr(mystr))
}

impl GrammarForm
{
    #[allow(clippy::new_ret_no_self)]
    pub (crate) fn new(line : &str, parser : &mut Parser, intoken : bool) -> Result<GrammarForm, String>
    {
        let re = &mut parser.internal_regexes;
        let mut ret = GrammarForm { tokens : Vec::new() };
        let tokens : Vec<&str> = line.split(' ').collect();
        for token in &tokens
        {
            if *token == ""
            {
                continue;
            }
            if *token == ">>?"
            {
                ret.tokens.push(GrammarToken::RestIsOptional);
            }
            else if re.is_exact(r"%.+%$", token)
            {
                let bare = slice(token, 1, -1);
                if intoken && !parser.regex_set.contains(&bare)
                {
                    parser.regex_set.insert(bare.clone());
                    parser.regex_list.push(bare.clone());
                }
                ret.tokens.push(GrammarToken::Regex(bare));
            }
            else if re.is_exact(r"\$.+\$\.\.\.(.)", token)
            {
                let separator = slice(token, -1, token.len() as i64);
                ret.tokens.push(GrammarToken::SeparatorNameList{text: slice(token, 1, -5), separator: separator.clone()});
                
                if re.is_exact(r"[^a-zA-Z0-9_ \t]+", &separator)
                {
                    if !parser.symbol_set.contains(&separator)
                    {
                        parser.symbol_set.insert(separator.clone());
                        parser.symbol_list.push(separator.clone());
                    }
                }
                else
                {
                    return plainerr("error: separator-list separator is not a symbol");
                }
            }
            else if re.is_exact(r"\$.+\$\*", token)
            {
                ret.tokens.push(GrammarToken::OptionalNameList(slice(token, 1, -2)));
            }
            else if re.is_exact(r"\$.+\$\+", token)
            {
                ret.tokens.push(GrammarToken::Name(slice(token, 1, -2)));
                ret.tokens.push(GrammarToken::OptionalNameList(slice(token, 1, -2)));
            }
            else if re.is_exact(r"\$.+\$\+\.\.\(\$.+\$\)", token)
            {
                let captures = re.captures(r"\$(.+)\$\+\.\.\(\$(.+)\$\)", token).unwrap();
                let text = captures.get(1).unwrap().as_str().to_string();
                let subtype = captures.get(2).unwrap().as_str().to_string();
                ret.tokens.push(GrammarToken::SpecialNameList{text, subtype});
            }
            else if re.is_exact(r"\$.+\$\?", token)
            {
                ret.tokens.push(GrammarToken::OptionalName(slice(token, 1, -2)));
            }
            else if re.is_exact(r"\$.+\$", token)
            {
                ret.tokens.push(GrammarToken::Name(slice(token, 1, -1)));
            }
            else if slice(token, 0, 1) == "$" && token.len() > 1
            {
                return plainerr(&format!("error: stray $\n{}", line));
            }
            else
            {
                ret.tokens.push(GrammarToken::Plain(token.to_string()));
                if re.is_exact(r"[a-zA-Z_][a-zA-Z_0-9]*", token)
                {
                    if !parser.text_set.contains(*token)
                    {
                        parser.text_set.insert(token.to_string());
                        parser.text_list.push(token.to_string());
                    }
                }
                else if re.is_exact(r"[^a-zA-Z0-9_]+", token)
                {
                    if !parser.symbol_set.contains(*token)
                    {
                        parser.symbol_set.insert(token.to_string());
                        parser.symbol_list.push(token.to_string());
                    }
                }
                else
                {
                    return plainerr(&format!("error: literal symbol `{}` does not follow the forms [a-zA-Z_][a-zA-Z_0-9]* || [^a-zA-Z0-9_]+\n{}", token, line));
                }
            }
        }
        Ok(ret)
    }
}
