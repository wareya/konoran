#![allow(clippy::len_zero)]

use std::collections::{HashMap, HashSet, BTreeSet};
use std::time::Instant;

pub (crate) mod ast;
pub (crate) mod grammar;
pub (crate) mod strings;
pub (crate) mod regexholder;

use {ast::*, grammar::*, strings::*};
use regexholder::RegexHolder;

// For performance reasons (i.e. temporary parse error storage is VERY slow otherwise),
//  we store possible tokens at the point of possible parse errors with a BTreeMap
//  with short strings stored literally as bytes instead of in a String

#[derive(Clone, Debug)]
pub (crate) struct ParseError {
    token : usize, // location of the token that caused the error
    expected : BTreeSet<MiniStr>,
}

impl ParseError {
    pub (crate) fn new(token : usize, text : &str) -> ParseError
    {
        let mut expected = BTreeSet::new();
        expected.insert(MiniStr::from(text));
        ParseError{token, expected}
    }
}

// adds a parse error if it has the same location, otherwise picks the earlier set (old set or new single as a set), produces result in place of left argument
pub (crate) fn build_new_error(myself : &mut Option<ParseError>, token : usize, text : &str)
{
    match myself
    {
        Some(myself) =>
        {
            if token > myself.token
            {
                *myself = ParseError::new(token, text);
            }
            else if token == myself.token
            {
                myself.expected.insert(MiniStr::from(text));
            }
        }
        None => *myself = Some(ParseError::new(token, text))
    }
}

// combines parse errors if they have the same location, otherwise picks the earlier one, produces result in place of left argument
pub (crate) fn build_best_error(myself : &mut Option<ParseError>, other : Option<ParseError>)
{
    match (myself.as_mut(), other)
    {
        (Some(myself), Some(other)) =>
        {
            if other.token > myself.token
            {
                *myself = other;
            }
            else if other.token == myself.token
            {
                for text in other.expected
                {
                    myself.expected.insert(text);
                }
            }
        }
        (None, Some(other)) => *myself = Some(other),
        _ => {}
    }
}

pub (crate) fn build_latest_node(myself : &mut Option<ASTNode>, other : Option<ASTNode>)
{
    match (myself.as_mut(), other)
    {
        (Some(myself), Some(other)) =>
        {
            if other.line > myself.line || (other.line == myself.line && other.position > myself.position)
            {
                *myself = other;
            }
        }
        (None, Some(other)) => *myself = Some(other),
        _ => {}
    }
}

#[derive(Clone)]
pub (crate) struct GrammarPoint {
    pub (crate) name: String,
    pub (crate) forms: Vec<GrammarForm>,
    pub (crate) istoken: bool,
    pub (crate) embed: bool,
    pub (crate) simplify: bool,
    pub (crate) hidden: bool,
    pub (crate) hide_literals: bool,
    pub (crate) precedence : Option<u64> // precedence of left-associative binary operator rules
}

#[derive(Clone)]
/// Provides facilities for turning program text into an AST.
pub struct Parser {
    pub (crate) regex_list : Vec<String>,
    pub (crate) symbol_list : Vec<String>,
    pub (crate) text_list : Vec<String>,
    // token matchers are inserted into both sets and vectors, sets to quickly check for duplicate insertion and vectors are for order
    pub (crate) regex_set : HashSet<String>,
    pub (crate) symbol_set : HashSet<String>,
    pub (crate) text_set : HashSet<String>,
    
    pub (crate) nodetypemap: HashMap<String, GrammarPoint>,
    pub (crate) internal_regexes: RegexHolder,
    inited: bool,
}

fn minierr(mystr : &str) -> String
{
    mystr.to_string()
}

fn plainerr<T>(mystr : &str) -> Result<T, String>
{
    Err(minierr(mystr))
}

type ParseInfo = (Option<ASTNode>, usize, Option<ParseError>, Option<ASTNode>);
type ParseVecInfo = (Option<Vec<ASTNode>>, usize, Option<ParseError>, Option<ASTNode>);

impl Default for Parser {
    fn default() -> Parser
    {
        Parser {
            regex_list: Vec::new(),
            symbol_list: Vec::new(),
            text_list: Vec::new(),
            regex_set: HashSet::new(),
            symbol_set: HashSet::new(),
            text_set: HashSet::new(),
            nodetypemap: HashMap::new(),
            internal_regexes: RegexHolder::new(),
            inited: false
        }
    }
}
impl Parser {
    /// Constructs a new parser with the default grammar.
    #[allow(dead_code)]
    pub fn new_from_default() -> Result<Parser, String>
    {
        let mut parser = Parser::default();
        parser.init(grammar::default_grammar())?;
        Ok(parser)
    }
    /// Constructs a new parser with a custom grammar.
    ///
    /// Only useful if you're eliminating parts of the grammar to restrict the language, or you're going to manually transform custom aspects of the AST into supported AST structures before compilation,
    pub fn new_from_grammar(grammar : &str) -> Result<Parser, String>
    {
        let mut parser = Parser::default();
        parser.init(grammar)?;
        Ok(parser)
    }
    fn init(&mut self, text: &str) -> Result<(), String>
    {
        let start_time = Instant::now();
        
        let mut lines : Vec<_> = vec!("".to_string());
        lines.extend(text.lines().rev().map(|x| x.to_string()));
    
        while lines.len() > 0
        {
            macro_rules! pop { () => { lines.pop().ok_or_else(|| "tried to access past end of program text".to_string()) }; }
            
            let mut line : String = pop!()?;
            if line == "" || line.starts_with("#")
            {
                continue;
            }
            let captures = self.internal_regexes.captures("([a-zA-Z_][a-zA-Z_0-9]*):[ ]*(EMBED)?[ ]*(SIMPLIFY)?[ ]*(HIDDEN)?[ ]*(HIDELITERALS)?[ ]*(TOKEN)?[ ]*(LEFTBINEXPR [0-9]+)?", &line)
                .ok_or_else(|| minierr(&format!("general syntax error\noffending line:\n{}", line)))?;
            let name = captures.get(1).ok_or_else(|| minierr("unreachable error in parser init getting rule name"))?.as_str().to_string();
            let embed = captures.get(2).is_some();
            let simplify = captures.get(3).is_some();
            let hidden = captures.get(4).is_some();
            let hide_literals = captures.get(5).is_some();
            let istoken = captures.get(6).is_some();
            let precedence =
            match captures.get(7)
            {
                Some(x) => Some(slice_to_end(x.as_str(), 12).parse::<u64>().or_else(|_| plainerr("error: LEFTBINEXPR argument must be a positive integer"))?),
                None => None
            };
            // last line is guaranteed to be "" which means we are unable to pop past the end here
            let mut nodetype : GrammarPoint = GrammarPoint{name, forms: Vec::new(), istoken, embed, simplify, hidden, hide_literals, precedence};
            line = pop!()?;
            while line != ""
            {
                if !line.starts_with("#")
                {
                    nodetype.forms.push(GrammarForm::new(&line, self, istoken)?);
                }
                line = pop!()?;
            }
            if self.nodetypemap.contains_key(&nodetype.name)
            {
                return plainerr(&format!("error: node type `{}` declared twice", nodetype.name));
            }
            self.nodetypemap.insert(nodetype.name.clone(), nodetype);
        }
        
        for regex in &self.regex_set
        {
            self.internal_regexes.prepare_exact(&regex);
        }
        
        for tuple in &self.nodetypemap
        {
            for form in &tuple.1.forms
            {
                for token in &form.tokens
                {
                    let name : String =
                    match token
                    {
                        GrammarToken::Name(text) |
                        GrammarToken::OptionalName(text) |
                        GrammarToken::OptionalNameList(text) |
                        GrammarToken::SeparatorNameList{text, ..} => text.clone(),
                        _ => "".to_string()
                    };
                    if name != "" && !self.nodetypemap.contains_key(&name)
                    {
                        return plainerr(&format!("error: node name {} is used without actually defined", name));
                    }
                }
            }
        }
        if !self.nodetypemap.contains_key("program")
        {
            return plainerr("error: grammar does not define name \"program\"");
        }
        
        self.symbol_list.sort_by_key(|text| -(text.len() as i64));
        self.text_list  .sort_by_key(|text| -(text.len() as i64));
        
        self.inited = true;
        
        println!("init took {:?}", Instant::now().duration_since(start_time));
        
        Ok(())
    }
    
    // FIXME: change it to not be line-based; seek to the next newline instead. necessary for things like strings containing newline literals, which should definitely be supported.
    pub (crate) fn tokenize(&mut self, lines : &[String], silent: bool) -> Result<Vec<LexToken>, String>
    {
        let mut ret : Vec<_> = Vec::new();
        let mut linecount = 1;
        
        let mut in_multiline_comment = false;
        
        for line in lines
        {
            let mut offset : usize = 0; // in bytes
            while offset < line.len() // also in bytes
            {
                // check for comments before doing anything else
                if let Some(signal) = line.get(offset..offset+2)
                {
                    if signal == "*/" && in_multiline_comment
                    {
                        in_multiline_comment = false;
                        offset += 2;
                        continue;
                    }
                    else if signal == "/*"
                    {
                        in_multiline_comment = true;
                        offset += 2;
                        continue;
                    }
                    else if signal == "//"
                    {
                        break;
                    }
                }
                if in_multiline_comment
                {
                    offset += 1;
                    continue;
                }
                // check for whitespace before doing any tokens
                if let Some(text) = self.internal_regexes.match_at("[ \r\n\t]+", &line, offset)
                {
                    offset += text.len();
                    continue;
                }
                
                let mut continue_the_while = false;
                for rule in &self.regex_list
                {
                    if let Some(text) = self.internal_regexes.match_at(&rule, &line, offset)
                    {
                        // TODO: fix position everywhere to be codepoints instead of bytes
                        ret.push(LexToken{text : text.clone(), line : linecount, position : offset+1});
                        offset += text.len();
                        continue_the_while = true;
                        break;
                    }
                }
                if continue_the_while { continue; }
                for text in &self.symbol_list
                {
                    if let Some(segment) = line.get(offset..offset+text.len())
                    {
                        if segment == text.as_str()
                        {
                            ret.push(LexToken{text : text.clone(), line : linecount, position : offset+1});
                            offset += text.len();
                            continue_the_while = true;
                            break;
                        }
                    }
                }
                if continue_the_while { continue; }
                for text in &self.text_list
                {
                    if let Some(segment) = line.get(offset..offset+text.len())
                    {
                        if segment == text.as_str()
                        {
                            // don't tokenize the beginnings of names as actual names
                            if offset + text.len() + 1 > line.len() && self.internal_regexes.is_exact(r"[a-zA-Z0-9_]", &slice(&line, (offset+text.len()) as i64, (offset+text.len()+1) as i64))
                            {
                                continue;
                            }
                            ret.push(LexToken{text : text.clone(), line : linecount, position : offset+1});
                            offset += text.len();
                            continue_the_while = true;
                            break;
                        }
                    }
                }
                if continue_the_while { continue; }
                return plainerr(&format!("failed to tokenize program\noffending line:\n{}", line));
            }
            linecount += 1;
        }
        
        if !silent
        {
            //println!("lex took {:?}", Instant::now().duration_since(start_time));
        }
        
        Ok(ret)
    }

    // attempts to parse a token list as a particular form of a grammar point
    fn parse_form(&self, tokens : &[LexToken], index : usize, form : &GrammarForm, formname : Option<&str>) -> Result<ParseVecInfo, String>
    {
        if tokens.len() == 0
        {
            return Ok((None, 0, None, None));
        }
        
        let mut nodes = Vec::new();
        let mut totalconsumed = 0;
        
        let mut latesterror = None;
        let mut latestnode = None;
        
        let mut defaultreturn = (None, 0);
        
        let mut i = 0;
        while i < form.tokens.len()
        {
            let part = &form.tokens[i];
            i += 1;
            match part
            {
                GrammarToken::Name(text) =>
                {
                    let kind = self.nodetypemap.get(text).ok_or_else(|| minierr(&format!("internal error: failed to find node type {} used by some grammar form", text)))?;
                    
                    let (bit, consumed, error, newlatest) = self.parse(&tokens, index+totalconsumed, kind)?;
                    build_latest_node(&mut latestnode, newlatest);
                    build_best_error(&mut latesterror, error);
                    if let Some(node) = bit
                    {
                        nodes.push(node);
                        totalconsumed += consumed;
                    }
                    else
                    {
                        return Ok((defaultreturn.0, defaultreturn.1, latesterror, latestnode));
                    }
                }
                GrammarToken::OptionalName(text) =>
                {
                    let kind = self.nodetypemap.get(text).ok_or_else(|| minierr(&format!("internal error: failed to find node type {} used by some grammar form", text)))?;
                    
                    let (bit, consumed, error, newlatest) = self.parse(&tokens, index+totalconsumed, kind)?;
                    build_latest_node(&mut latestnode, newlatest);
                    build_best_error(&mut latesterror, error);
                    if let Some(node) = bit
                    {
                        nodes.push(node);
                        totalconsumed += consumed;
                    }
                }
                GrammarToken::OptionalNameList(text) =>
                {
                    let kind = self.nodetypemap.get(text).ok_or_else(|| minierr(&format!("internal error: failed to find node type {} used by some grammar form", text)))?;
                    
                    let (mut bit, mut consumed, mut error, mut newlatest) = self.parse(&tokens, index+totalconsumed, kind)?;
                    build_latest_node(&mut latestnode, newlatest);
                    build_best_error(&mut latesterror, error);
                    
                    while let Some(node) = bit
                    {
                        nodes.push(node);
                        totalconsumed += consumed;
                        
                        let tuple = self.parse(&tokens, index+totalconsumed, kind)?;
                        bit = tuple.0;
                        consumed = tuple.1;
                        error = tuple.2;
                        newlatest = tuple.3;
                        
                        build_best_error(&mut latesterror, error);
                        build_latest_node(&mut latestnode, newlatest);
                    }
                }
                GrammarToken::SpecialNameList{text, subtype} =>
                {
                    let kind = self.nodetypemap.get(text).ok_or_else(|| minierr(&format!("internal error: failed to find node type {} used by some grammar form", text)))?;
                    
                    let (mut bit, mut consumed, mut error, mut newlatest) = self.parse(&tokens, index+totalconsumed, kind)?;
                    build_latest_node(&mut latestnode, newlatest);
                    build_best_error(&mut latesterror, error);
                    
                    while let Some(node) = bit
                    {
                        nodes.push(node);
                        totalconsumed += consumed;
                        
                        let tuple = self.parse(&tokens, index+totalconsumed, kind)?;
                        bit = tuple.0;
                        consumed = tuple.1;
                        error = tuple.2;
                        newlatest = tuple.3;
                        
                        build_best_error(&mut latesterror, error);
                        build_latest_node(&mut latestnode, newlatest);
                    }
                    
                    if nodes.len() == 0 || nodes.last().unwrap().child(0).unwrap().text != *subtype
                    {
                        //println!("{:#?}", nodes.last().unwrap());
                        return Ok((defaultreturn.0, defaultreturn.1, latesterror, latestnode));
                    }
                }
                GrammarToken::SeparatorNameList{text, separator} =>
                {
                    let kind = self.nodetypemap.get(text).ok_or_else(|| minierr(&format!("internal error: failed to find node type {} used by some grammar form", text)))?;
                    
                    let (mut bit, mut consumed, mut error, mut newlatest) = self.parse(&tokens, index+totalconsumed, kind)?;
                    build_latest_node(&mut latestnode, newlatest);
                    build_best_error(&mut latesterror, error);
                    if bit.is_none()
                    {
                        return Ok((defaultreturn.0, defaultreturn.1, latesterror, latestnode));
                    }
                    while let Some(node) = bit
                    {
                        nodes.push(node);
                        totalconsumed += consumed;
                        
                        if let Some(check_separator) = tokens.get(index+totalconsumed)
                        {
                            if check_separator.text == *separator
                            {
                                totalconsumed += 1;
                                
                                let tuple = self.parse(&tokens, index+totalconsumed, kind)?;
                                bit = tuple.0;
                                consumed = tuple.1;
                                error = tuple.2;
                                newlatest = tuple.3;
                                
                                build_best_error(&mut latesterror, error);
                                build_latest_node(&mut latestnode, newlatest);
                                
                                // undo separator drain if right-hand rule parse failed
                                if bit.is_none()
                                {
                                    totalconsumed -= 1;
                                }
                                continue;
                            }
                        }
                        break;
                    }
                }
                GrammarToken::Plain(text) =>
                {
                    if let Some(token) = tokens.get(index+totalconsumed)
                    {
                        if token.text == *text
                        {
                            nodes.push(ASTNode{text : token.text.to_string(), line : token.line, position : token.position, children : None});
                            totalconsumed += 1;
                            continue;
                        }
                    }
                    build_new_error(&mut latesterror, index+totalconsumed, &text);
                    //build_latest_node(&mut latestnode, newlatest);
                    return Ok((defaultreturn.0, defaultreturn.1, latesterror, latestnode));
                }
                GrammarToken::Regex(text) =>
                {
                    if let Some(token) = tokens.get(index+totalconsumed)
                    {
                        if self.internal_regexes.is_exact_immut(text, &token.text)?
                        {
                            nodes.push(ASTNode{text : token.text.to_string(), line : token.line, position : token.position, children : None});
                            totalconsumed += 1;
                            continue;
                        }
                    }
                    build_new_error(&mut latesterror, index+totalconsumed, formname.unwrap_or(&text));
                    return Ok((defaultreturn.0, defaultreturn.1, latesterror, latestnode));
                }
                GrammarToken::RestIsOptional =>
                {
                    defaultreturn = (Some(nodes.clone()), totalconsumed);
                }
            }
        }
        
        build_latest_node(&mut latestnode, nodes.last().cloned());
        
        Ok((Some(nodes), totalconsumed, latesterror, latestnode))
    }

    // attempts to parse a token list as each form of a grammar point in order and uses the first valid one
    fn parse(&self, tokens : &[LexToken], index : usize, nodetype : &GrammarPoint) -> Result<ParseInfo, String>
    {
        if tokens.len() == 0
        {
            return Ok((Some(ASTNode{text : "program".to_string(), line : 0, position : 0, children : Some(Vec::new()) }), 0, None, None));
        }
        
        let mut latesterror : Option<ParseError> = None;
        let mut latestnode : Option<ASTNode> = None;
        
        for form in &nodetype.forms
        {
            let sentname = if nodetype.istoken { Some(nodetype.name.as_str()) } else { None };
            let (nodes, consumed, error, newlatest) = self.parse_form(&tokens, index, form, sentname)?;
            build_latest_node(&mut latestnode, newlatest);
            if let Some(nodes) = &nodes
            {
                build_latest_node(&mut latestnode, nodes.last().cloned());
            }
            build_best_error(&mut latesterror, error);
            if let Some(token) = tokens.get(index)
            {
                if let Some(nodes) = nodes
                {
                    return Ok((Some(ASTNode{text : nodetype.name.clone(), line : token.line, position : token.position, children : Some(nodes) }), consumed, latesterror, latestnode));
                }
            }
        }
        Ok((None, 0, latesterror, latestnode))
    }
    fn rotate(ast : &mut ASTNode) -> Result<(), String>
    {
        if !(ast.is_parent() && ast.child_count().unwrap() == 3 && ast.child(2)?.is_parent() && ast.child(2)?.child_count().unwrap() >= 1)
        {
            return plainerr("internal error: attempted to rotate AST node for which the conditions of AST rotation were not satisfied");
        }
        let mut node_holder = dummy_astnode();
        // tree rotation around self, child 0, and child 2
        std::mem::swap(&mut node_holder, ast.child_mut(2)?); // detach right from under left (leaving dummy on left)
        std::mem::swap(ast.child_mut(2)?, node_holder.child_mut(0)?); // move betweener from right to left (leaving dummy on right)
        std::mem::swap(ast, node_holder.child_mut(0)?); // attach left to under right (leaving dummy on root)
        std::mem::swap(ast, &mut node_holder); // attach right to root (leaving dummy on node_holder)
        Ok(())
    }
    fn parse_rotate_associativity_binexpr(&self, ast : &mut ASTNode) -> Result<bool, String>
    {
        let is_rotatable_binexpr = |a : &ASTNode| -> Result<bool, String>
        {
            let rule = self.nodetypemap.get(&a.text);
            Ok(a.is_parent() && a.child_count().unwrap() == 3 && rule.ok_or("Internal error")?.precedence.is_some())
        };
        let compatible_associativity = |a : &ASTNode, b : &ASTNode| -> Result<bool, String>
        {
            let rule_a = self.nodetypemap.get(&a.child(0)?.text);
            let rule_b = self.nodetypemap.get(&b.child(0)?.text);
            Ok(a.is_parent() && b.is_parent() && rule_a.ok_or("Internal error")?.precedence == rule_b.ok_or("Internal error")?.precedence)
        };
        if is_rotatable_binexpr(ast)? && is_rotatable_binexpr(ast.child(2)?)? && compatible_associativity(ast, ast.child(2)?)?
        {
            Parser::rotate(ast)?;
            Ok(true)
        }
        else
        {
            Ok(false)
        }
    }
    fn parse_fix_associativity(&self, ast : &mut ASTNode) -> Result<(), String>
    {
        if ast.is_parent()
        {
            if self.parse_rotate_associativity_binexpr(ast)?
            {
                self.parse_fix_associativity(ast)?;
            }
            else if let Some(children) = &mut ast.children
            {
                for mut child in children
                {
                    self.parse_fix_associativity(&mut child)?;
                }
            }
        }
        Ok(())
    }
    fn parse_tweak_ast(&self, ast : &mut ASTNode) -> Result<(), String>
    {
        if ast.is_parent()
        {
            let mut rule = self.nodetypemap.get(&ast.text).unwrap();
            while rule.simplify && ast.is_parent() && ast.child_count() == Ok(1)
            {
                if let Some(children) = &mut ast.children
                {
                    let mut temp = Vec::new();
                    std::mem::swap(&mut temp, children);
                    let dummy = temp.get_mut(0).ok_or_else(|| minierr("internal error: could not access child that was supposed to be there in expression summarization"))?;
                    std::mem::swap(ast, dummy);
                    rule = self.nodetypemap.get(&ast.text).unwrap()
                }
            }
            
            if let Some(children) = &mut ast.children
            {
                children.retain(|child| !child.is_parent() || !self.nodetypemap.get(&child.text).unwrap().hidden);
                if rule.hide_literals
                {
                    children.retain(|child| child.is_parent());
                }
            
                for mut child in children
                {
                    self.parse_tweak_ast(&mut child)?;
                }
            }
            
            if let Some(children) = &mut ast.children
            {
                let mut i = 0;
                while i < children.len()
                {
                    if children[i].is_parent()
                    {
                        let rule = self.nodetypemap.get(&children[i].text).unwrap();
                        if rule.embed
                        {
                            let child = children.remove(i);
                            children.splice(i..i, child.children.unwrap());
                            continue;
                        }
                    }
                    i += 1;
                }
            }
        }
        Ok(())
    }
    fn parse_tweak_ast_pass_2(&self, ast : &mut ASTNode) -> Result<(), String>
    {
        if ast.is_parent()
        {
            let line = ast.line;
            let position = ast.position;
            if ast.text == "rhunexpr" || ast.text == "funccall" || ast.text == "lvrhunexpr"
            {
                if let Some(children) = &mut ast.children
                {
                    if children.len() <= 1
                    {
                        let mut temp = Vec::new();
                        std::mem::swap(&mut temp, children);
                        let dummy = temp.get_mut(0).ok_or_else(|| minierr("internal error: could not access child that was supposed to be there in expression summarization"))?;
                        std::mem::swap(ast, dummy);
                    }
                    else
                    {
                        for child in children.iter_mut()
                        {
                            if child.text == "rhunexpr_right" || child.text == "rhunexpr_rightlv"
                            {
                                let mut temp = None;
                                std::mem::swap(&mut temp, &mut child.children);
                                if let Some(mut dummy) = temp
                                {
                                    let dummy = dummy.get_mut(0).ok_or_else(|| minierr("internal error: could not access child that was supposed to be there in expression summarization"))?;
                                    std::mem::swap(child, dummy);
                                }
                                else
                                {
                                    return Err(minierr("internal error: could not access child that was supposed to be there in expression summarization"));
                                }
                            }
                        }
                        let ast_right = children.pop().ok_or_else(|| minierr("internal error: failed to access last element of right-hand unary expansion expression"))?;
                        let mut ast_left = ASTNode
                        {
                            text: format!("{}_head", ast_right.text),
                            line,
                            position,
                            children: Some(Vec::new()),
                        };
                        std::mem::swap(&mut ast_left, ast);
                        ast.children = Some(vec!(ast_left, ast_right));
                    }
                }
            }
            
            if let Some(children) = &mut ast.children
            {
                for mut child in children
                {
                    self.parse_tweak_ast_pass_2(&mut child)?;
                }
            }
        }
        Ok(())
    }
    
    pub fn parse_with_root_node_type(&self, tokens : &[LexToken], lines : &[String], silent: bool, root : &str) -> Result<Option<ASTNode>, String>
    {
        let start_time = Instant::now();
        
        if !silent
        {
            println!("parsing...");
        }
        if let Some(program_type) = self.nodetypemap.get(root)
        {
            let (raw_ast, consumed, latesterror, latestnode) = self.parse(&tokens, 0, program_type)?;
            if !silent
            {
                println!("successfully parsed {} out of {} tokens", consumed, tokens.len());
                println!("parse took {:?}", Instant::now().duration_since(start_time));
            }
            
            if consumed != tokens.len() || raw_ast.is_none()
            {
                if let Some(mut error) = latesterror
                {
                    let mut expected : Vec<String> = error.expected.into_iter().map(|x| x.into_string()).collect();
                    expected.sort();
                    let onepast = error.token == tokens.len();
                    if onepast
                    {
                        error.token -= 1;
                    }
                    if expected.len() == 1
                    {
                        if let Some(expect) = expected.get(0)
                        {
                            println!("error: expected `{}`", expect);
                        }
                        else
                        {
                            println!("internal error: failed to grab expected symbol that was supposed to be there while printing parser error");
                        }
                    }
                    else
                    {
                        println!("error: expected one of `{}`", expected.join("`, `"));
                    }
                    if let Some(token) = tokens.get(error.token)
                    {
                        let linenum = token.line;
                        let mut position = token.position;
                        if onepast
                        {
                            position += 1;
                        }
                        if let Some(line) = lines.get(linenum-1)
                        {
                            println!("context on line {}:\n{}\n{}^", linenum, line, " ".repeat(position-1));
                            println!("latest AST node: {:?}", latestnode);
                            println!("latest AST node (pretty):\n{}", latestnode.unwrap().pretty_debug());
                        }
                        else
                        {
                            println!("internal error: failed to grab context text for parse error");
                        }
                        if onepast
                        {
                            println!("note: this is past the end of your program; you probably have an unclosed block delimiter (or something similar) way, way up there somewhere");
                        }
                    }
                    else
                    {
                        println!("internal error: failed to grab context info for parse error; token number {} out of {}", error.token, tokens.len());
                    }
                }
                else
                {
                    println!("error: unexpected or malformed expression");
                    if let Some(token) = tokens.get(consumed)
                    {
                        println!("(line {})\n(position {})", token.line, token.position);
                    }
                    else
                    {
                        println!("internal error: failed to grab context for parse error {:?} {:?}", raw_ast, tokens);
                    }
                }
                
                Ok(None)
            }
            else if let Some(mut ast) = raw_ast
            {
                if !silent
                {
                    println!("fixing associativity...");
                }
                self.parse_fix_associativity(&mut ast)?;
                
                if !silent
                {
                    println!("tweaking AST...");
                }
                self.parse_tweak_ast(&mut ast)?;
                self.parse_tweak_ast_pass_2(&mut ast)?;
                
                if !silent
                {
                    println!("all good!");
                }
                
                Ok(Some(ast))
            }
            else
            {
                plainerr("error: failed to parse")
            }
        }
        else
        {
            Err(format!("error: grammar does not define `{}` node type", root))
        }
    }
    /// Parses a program. If "silent" is set to true, timing and error diagnostics will be printed to stdout.
    ///
    /// If an internal error is encountered during parsing, Err is returned.
    ///
    /// If the parse fails, Ok(None) is returned.
    ///
    /// Otherwise the root node of an AST is returned as Ok(Ok(Node)).
    ///
    /// The returned structure is an AST, not a parse tree.
    /// - Arithmetic expressions have their associativity direction corrected (to be left-recursive; in the grammar, they're right-recursive, with LEFTBINEXPR tags)
    /// - Value expressions with a single child are simplified to just their child
    /// - Statements have their trailing semicolon stripped
    pub fn parse_program(&self, tokens : &[LexToken], lines : &[String], silent: bool) -> Result<Option<ASTNode>, String>
    {
        self.parse_with_root_node_type(tokens, lines, silent, "program")
    }
}
