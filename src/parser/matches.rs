// This is a braindead version of the matches! macro in SimonSapin(?)'s rust-std-candidates repository: https://github.com/SimonSapin/rust-std-candidates#the-matches-macro
macro_rules! matches { ( $x:expr , $( $p:pat )|+ ) =>
{
    match $x
    {
        $($p)|+ => true,
        _ => false
    }
} }


macro_rules! match_or_err { ( $expr:expr, $as:pat => $ok:expr, $err:expr ) =>
{
    match $expr
    {
        $as => Ok($ok),
        _ => Err($err)
    }
} }


macro_rules! match_or_none { ( $expr:expr, $as:pat => $ok:expr ) =>
{
    match $expr
    {
        $as => Some($ok),
        _ => None
    }
} }
