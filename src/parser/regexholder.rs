use std::collections::HashMap;

use regex::{Regex, Captures};

#[derive(Clone)]
pub (crate) struct RegexHolder {
    exact_regexes : HashMap<String, Result<Regex, regex::Error>>,
    regexes : HashMap<String, Result<Regex, regex::Error>>
}

impl RegexHolder {
    pub (crate) fn new() -> RegexHolder
    {
        RegexHolder { exact_regexes: HashMap::new(), regexes : HashMap::new() }
    }
    pub (crate) fn prepare_exact(&mut self, regex_text : &str)
    {
        if !self.exact_regexes.contains_key(regex_text)
        {
            let regex = Regex::new(&format!("^{}$", regex_text));
            self.exact_regexes.insert(regex_text.to_string(), regex);
        }
    }
    #[allow(clippy::wrong_self_convention)]
    pub (crate) fn is_exact(&mut self, regex_text : &str, text : &str) -> bool
    {
        if let Some(regex) = self.exact_regexes.get(regex_text)
        {
            return regex.as_ref().map(|r| r.is_match(text)).unwrap_or(false);
        }
        let regex = Regex::new(&format!("^{}$", regex_text));
        self.exact_regexes.insert(regex_text.to_string(), regex);
        self.is_exact(regex_text, text)
    }
    pub (crate) fn captures<'t>(&mut self, regex_text : &str, text : &'t str) -> Option<Captures<'t>>
    {
        if let Some(regex) = self.exact_regexes.get(regex_text)
        {
            return regex.as_ref().map(|r| r.captures(text)).unwrap_or(None);
        }
        let regex = Regex::new(&format!("^{}$", regex_text));
        self.exact_regexes.insert(regex_text.to_string(), regex);
        self.captures(regex_text, text)
    }
    pub (crate) fn is_exact_immut(& self, regex_text : &str, text : &str) -> Result<bool, String>
    {
        let regex = self.exact_regexes.get(regex_text).ok_or_else(|| "internal error: attempted to use is_exact_immut for a regex that has not yet been cached".to_string())?;
        
        Ok(regex.as_ref().map(|r| r.is_match(text)).unwrap_or(false))
    }
    // regex offsets are bytes:
    // let mystr = "あそこだよっ！";
    // println!("{}", mystr);
    // let re = Regex::new("[あそ]").unwrap();
    // assert!(re.find_at(mystr, 0).unwrap().start() == 0);
    // assert!(re.find_at(mystr, 3).unwrap().start() == 3);
    pub (crate) fn match_at(&mut self, regex_text : &str, text : &str, start : usize) -> Option<String>
    {
        if let Some(regex) = self.regexes.get(regex_text)
        {
            let regex = regex.as_ref().ok()?;
            let my_match = regex.find_at(text, start)?;
            
            if my_match.start() == start
            {
                return Some(my_match.as_str().to_string());
            }
            return None;
        }
        let regex = Regex::new(regex_text);
        self.regexes.insert(regex_text.to_string(), regex);
        self.match_at(regex_text, text, start)
    }
}