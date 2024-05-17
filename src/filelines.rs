
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
        let backend = Rc::clone(&self.backend);
        let lock = backend.take();
        Self::IntoIter { backend, lock }
    }
}

impl Drop for FileLinesIterator
{
    fn drop(&mut self)
    {
        let lock = self.lock.take();
        self.backend.replace(lock);
    }
}

impl Iterator for FileLinesIterator
{
    type Item = String;
    fn next(&mut self) -> Option<String>
    {
        let lock = self.lock.as_mut().unwrap();
        let mut buf = [0u8; 1];
        if let Ok(n) = lock.read(&mut buf)
        {
            if n == 0
            {
                return None
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
            None
        }
    }
}
