
use std::rc::Rc;
use std::cell::RefCell;

pub (crate) trait ReadSeek : std::io::Read + std::io::Seek {}
impl<T> ReadSeek for T where T: std::io::Read + std::io::Seek {}

/// Helper iterator for looping over the lines of a file. Unlike [std::io::BufRead::lines], can be cloned and rewound.
///
/// The underlying file must be utf-8 and have either LF or CRLF newlines, otherwise it may fail to iterate.
///
/// The [FileLines::into_iter()] implementation panics if an existing iterator exists for this FileLines object.
pub struct FileLines
{
    pub (crate) backend : Rc<RefCell<Option<Box<dyn ReadSeek>>>>,
}
/// Iterator of lines over a FileLines object.
pub struct FileLinesIterator
{
    pub (crate) backend : Rc<RefCell<Option<Box<dyn ReadSeek>>>>,
    lock : Option<Box<dyn ReadSeek>>,
}

impl FileLines
{
    /// Creates a cloneable lines iterator.
    pub fn from_seekable<T : std::io::Read + std::io::Seek + 'static>(f : T) -> Self
    {
        Self { backend : Rc::new(RefCell::new(Some(Box::new(f)))) }
    }
}

impl Clone for FileLines
{
    fn clone(&self) -> Self
    {
        Self { backend : Rc::clone(&self.backend) }
    }
}

impl IntoIterator for FileLines
{
    type Item = String;
    type IntoIter = FileLinesIterator;
    /// Panics if an existing iterator exists for this FileLines object.
    fn into_iter(self) -> Self::IntoIter
    {
        // Borrow by cloning the seekable's shared pointer into the iterator object.
        let backend = Rc::clone(&self.backend);
        // Null the seekable and move it into the iterator object. (The iterator object will put it back when it drops or finishes.)
        let lock = backend.take();
        Self::IntoIter { backend, lock }
    }
}

impl FileLinesIterator
{
    fn invalidate(&mut self)
    {
        // Invalidate by placing the original seekable back into the 
        let mut lock = self.lock.take();
        if let Some(ref mut seekable) = lock
        {
            let _ = seekable.rewind();
        }
        self.backend.replace(lock);
        // prevent re-invalidation
        self.backend = Rc::new(RefCell::new(None));
    }
}

impl Drop for FileLinesIterator
{
    fn drop(&mut self)
    {
        self.invalidate()
    }
}

impl Iterator for FileLinesIterator
{
    type Item = String;
    fn next(&mut self) -> Option<String>
    {
        if let Some(lock) = self.lock.as_mut()
        {
            let mut buf = [0u8; 1];
            if let Ok(n) = lock.read(&mut buf)
            {
                if n == 0
                {
                    self.invalidate();
                    return None;
                    //return Some("".to_string());
                }
                let mut bytes = vec!(buf[0]);
                let mut n = lock.read(&mut buf);
                while n.is_ok() && !matches!(n, Ok(0)) && buf[0] != b'\n'
                {
                    bytes.push(buf[0]);
                    n = lock.read(&mut buf);
                }
                if let Some(b'\n') = bytes.last()
                {
                    bytes.pop();
                }
                if let Some(b'\r') = bytes.last()
                {
                    bytes.pop();
                }
                return String::from_utf8(bytes).ok();
            }
            else
            {
                self.invalidate();
                None
            }
        }
        else
        {
            None
        }
    }
}
