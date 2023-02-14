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
    pub (crate) children: Vec<ASTNode>,
    pub (crate) precedence: Option<u64>,
    pub (crate) isparent: bool,
}

impl ASTNode {
    pub (crate) fn last_child(&'_ self) -> Result<&'_ ASTNode, String>
    {
        self.child(self.children.len()-1)
    }
    pub (crate) fn child(&'_ self, n : usize) -> Result<&'_ ASTNode, String>
    {
        self.children.get(n).ok_or_else(|| format!("internal error: tried to access child {} (zero-indexed) of ast node that only has {} children", n, self.children.len()))
    }
    pub (crate) fn child_mut(&'_ mut self, n : usize) -> Result<&'_ mut ASTNode, String>
    {
        let len = self.children.len();
        self.children.get_mut(n).ok_or_else(|| format!("internal error: tried to access child {} (zero-indexed) of ast node that only has {} children", n, len))
    }
    pub (crate) fn child_slice(&'_ self, start : isize, end : isize) -> Result<&'_[ASTNode], String>
    {
        let u_start = if start <  0 {self.children.len() - (-start as usize)} else {start as usize};
        let u_end   = if end   <= 0 {self.children.len() - (-end   as usize)} else {end   as usize};
        
        self.children.get(u_start..u_end).ok_or_else(|| format!("internal error: tried to access child range {} to {} (zero-indexed) of ast node that only has {} children", u_start, u_end, self.children.len()))
    }
}

pub (crate) fn dummy_astnode() -> ASTNode
{
    ASTNode{text: "".to_string(), line: 0, position: 0, isparent: false, children: Vec::new(), precedence: None}
}
