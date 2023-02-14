pub (crate) fn slice_any<T>(collection : &[T], start : i64, end : i64) -> Option<&[T]>
{
    let u_start = if start < 0 {collection.len() - (-start as usize)} else {start as usize};
    let u_end   = if end   < 0 {collection.len() - (-end   as usize)} else {end   as usize};
    
    collection.get(u_start..u_end)
}
pub (crate) fn slice(text : &str, start : i64, end : i64) -> String
{
    slice_any(&text.chars().collect::<Vec<char>>(), start, end).map(|chars| chars.iter().collect()).unwrap_or_else(|| "".to_string())
}

pub (crate) fn slice_any_to_end<T>(collection : &[T], start : i64) -> Option<&[T]>
{
    let u_start = if start < 0 {collection.len() - (-start as usize)} else {start as usize};
    
    collection.get(u_start..)
}
pub (crate) fn slice_to_end(text : &str, start : i64) -> String
{
    slice_any_to_end(&text.chars().collect::<Vec<char>>(), start).map(|chars| chars.iter().collect()).unwrap_or_else(|| "".to_string())
}

pub (crate) fn unescape(text: &str) -> String
{
    let mut ret = String::with_capacity(text.len());
    let mut chars : Vec<_> = text.chars().rev().collect();
    while let Some(c) = chars.pop()
    {
        if c != '\\'
        {
            ret.push(c);
        }
        else if let Some(c2) = chars.pop()
        {
            match c2
            {
                '\\' => ret.push(c),
                'n' => ret.push('\n'),
                'r' => ret.push('\r'),
                't' => ret.push('\t'),
                '"' => ret.push('"'),
                _ => ret.extend(&[c, c2])
            }
        }
    }
    ret
}

pub (crate) fn escape(text: &str) -> String
{
    let mut ret = String::with_capacity(text.len());
    let mut chars : Vec<_> = text.chars().rev().collect();
    while let Some(c) = chars.pop()
    {
        match c
        {
            '\\' => ret.extend(&['\\', '\\']),
            '\n' => ret.extend(&['\\', 'n']),
            '\r' => ret.extend(&['\\', 'r']),
            '\t' => ret.extend(&['\\', 't']),
            '\"' => ret.extend(&['\\', '"']),
            _ => ret.push(c)
        }
    }
    ret
}

fn trim_at_null(mystr : &[u8]) -> &[u8]
{
    let mut nullpos = 0usize;
    while nullpos < mystr.len() && mystr[nullpos] != 0
    {
        nullpos += 1
    }
    &mystr[..nullpos]
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub (crate) enum MiniStr {
    Short([u8; 8]),
    Long(String)
}

impl MiniStr {
    pub (crate) fn from(text : &str) -> MiniStr
    {
        if text.len() <= 8
        {
            let mut ret : [u8; 8] = [0,0,0,0,0,0,0,0];
            for (i, c) in text.bytes().enumerate()
            {
                ret[i] = c;
            }
            return MiniStr::Short(ret);
        }
        MiniStr::Long(text.to_string())
    }
    #[allow(clippy::wrong_self_convention)]
    pub (crate) fn into_string(self) -> String
    {
        match self
        {
            MiniStr::Short(bytes) => std::str::from_utf8(trim_at_null(&bytes)).map(|x| x.to_string()).unwrap_or_else(|_| "<err>".to_string()),
            MiniStr::Long(string) => string
        }
    }
}
