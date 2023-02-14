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
            Err(format!("internal error: tried access mutable child of non-parent AST node"))
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
            let count = children.len();
            let u_start = if start <  0 {count - (-start as usize)} else {start as usize};
            let u_end   = if end   <= 0 {count - (-end   as usize)} else {end   as usize};
            
            children.get(u_start..u_end).ok_or_else(|| format!("internal error: tried to access child range {} to {} (zero-indexed) of ast node that only has {} children", u_start, u_end, count))
        }
        else
        {
            Err(format!("internal error: tried slice children of non-parent AST node"))
        }
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
}

pub (crate) fn dummy_astnode() -> ASTNode
{
    ASTNode{text: "".to_string(), line: 0, position: 0, children: None}
}
