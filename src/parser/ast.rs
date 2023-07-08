#[derive(Clone)]
#[derive(Debug)]
pub struct LexToken {
    pub (crate) text: String,
    pub (crate) line: usize,
    pub (crate) position: usize,
}

#[derive(Clone)]
#[derive(Debug)]
pub struct ASTNode {
    pub (crate) text: String,
    pub (crate) line: usize,
    pub (crate) position: usize,
    pub (crate) children: Option<Vec<ASTNode>>,
}

impl ASTNode {
    pub (crate) fn pretty_debug(&self) -> String
    {
        let mut ast_debug = format!("{:#?}", self);
        ast_debug = ast_debug.replace("    ", " ");
        ast_debug = ast_debug.split("\n").filter(|x|
               !x.contains(" position:")
            && !x.contains(" line:")
            && !x.contains(" children: None")
            && !x.contains(" ),")
            && !x.contains(" },")
            && !x.contains(" ],")
            && !x.ends_with(" [")
            && !x.ends_with(" (")
            && *x != "}"
            ).map(|x|
                x.replace("{}", "None").replace(" {", ":").replace("children: Some(", "children:")
            ).collect::<Vec<_>>().join("\n");
        ast_debug
    }
}

impl ASTNode {
    #[allow(dead_code)]
    pub (crate) fn last_child(&'_ self) -> Result<&'_ ASTNode, String>
    {
        let count = self.child_count()?;
        self.child(count-1)
    }
    pub (crate) fn child(&'_ self, n : usize) -> Result<&'_ ASTNode, String>
    {
        if let Some(children) = &self.children
        {
            children.get(n).ok_or_else(|| format!("internal error: tried to access child {} (zero-indexed) of ast node that only has {} children", n, children.len()))
        }
        else
        {
            Err(format!("internal error: tried access child of non-parent AST node"))
        }
    }
    pub (crate) fn child_mut(&'_ mut self, n : usize) -> Result<&'_ mut ASTNode, String>
    {
        if let Some(children) = &mut self.children
        {
            let len = children.len();
            children.get_mut(n).ok_or_else(|| format!("internal error: tried to access child {} (zero-indexed) of ast node that only has {} children", n, len))
        }
        else
        {
            Err(format!("internal error: tried access mutable child of non-parent AST node"))
        }
    }
    pub (crate) fn child_slice(&'_ self, start : isize, end : isize) -> Result<&'_[ASTNode], String>
    {
        if let Some(children) = &self.children
        {
            let count = children.len() as isize;
            let u_start = if start <  0 {count - (-start)    } else {start} as usize;
            let u_end   = if end   <= 0 {count - (-end  ) + 1} else {end  } as usize;
            
            children.get(u_start..u_end).ok_or_else(|| format!("internal error: tried to access child range {} to {} (zero-indexed) of ast node that only has {} children", u_start, u_end, count))
        }
        else
        {
            Err(format!("internal error: tried slice children of non-parent AST node"))
        }
    }
    pub (crate) fn get_children(&'_ self) -> Result<&'_[ASTNode], String>
    {
        self.child_slice(0, -1)
    }
    pub (crate) fn child_count(&'_ self) -> Result<usize, String>
    {
        if let Some(children) = &self.children
        {
            return Ok(children.len())
        }
        Err(format!("internal error: tried count children of non-parent AST node"))
    }
    pub (crate) fn is_parent(&'_ self) -> bool
    {
        self.children.is_some()
    }
    #[allow(dead_code)]
    pub (crate) fn get_tokens(&self) -> Vec<String>
    {
        let mut ret = Vec::new();
        self.visit(&mut |node : &ASTNode| -> bool
        {
            if !node.is_parent()
            {
                ret.push(node.text.clone());
            }
            false
        });
        ret
    }
        
    pub (crate) fn visit(&self, f : &mut dyn FnMut(&ASTNode) -> bool)
    {
        if !f(self) && self.is_parent()
        {
            for child in self.get_children().unwrap()
            {
                child.visit(f);
            }
        }
    }
}

pub (crate) fn dummy_astnode() -> ASTNode
{
    ASTNode{text: "".to_string(), line: 0, position: 0, children: None}
}
