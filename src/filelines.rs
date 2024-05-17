//! Helper iterator for looping over the lines of a file. Unlike [std::io::BufRead::lines], can be cloned.
//!
//! Makes it easier to pass files to [crate::compiler::process_program].

use std::rc::Rc;
use std::cell::RefCell;

/// Helper iterator for looping over the lines of a file. Unlike [std::io::BufRead::lines], can be cloned. Cloned copies start iterating from the same place as earlier copies started, but only one active iterator can exist at once.
///
/// The underlying file must be utf-8 and have either LF or CRLF newlines, otherwise it may fail to iterate or return distorted Strings. (However, any returned Strings will be valid Strings, even if distorted.)
///
/// The [FileLines::into_iter()] implementation silently fails if an existing iterator exists for this FileLines object.
pub struct FileLines<T : std::io::Read + std::io::Seek>
{
    pub (crate) backend : Rc<RefCell<Option<T>>>,
    position : u64,
}
/// Iterator of lines over a FileLines object.
pub struct FileLinesIterator<T : std::io::Read + std::io::Seek>
{
    pub (crate) backend : Rc<RefCell<Option<T>>>,
    lock : Option<T>,
    startpos : u64,
}

impl<T : std::io::Read + std::io::Seek> FileLines<T>
{
    /// Creates a cloneable lines iterator.
    pub fn from_seekable(mut f : T) -> Self
    {
        let position = f.stream_position().unwrap();
        Self { backend : Rc::new(RefCell::new(Some(f))), position }
    }
    /// Returns the underlying object and invalidates any other cloned FileLines instances.
    ///
    /// Panics if an associated FileLinesIterator instance exists and is valid.
    pub fn into_inner(self) -> T
    {
        self.backend.take().unwrap()
    }
}

impl<T : std::io::Read + std::io::Seek> Clone for FileLines<T>
{
    fn clone(&self) -> Self
    {
        Self { backend : Rc::clone(&self.backend), position : self.position }
    }
}

impl<T : std::io::Read + std::io::Seek> IntoIterator for FileLines<T>
{
    type Item = String;
    type IntoIter = FileLinesIterator<T>;
    /// Silently fails if an existing iterator exists for this FileLines object.
    fn into_iter(self) -> FileLinesIterator<T>
    {
        // Borrow by cloning the seekable's shared pointer into the iterator object.
        let backend = Rc::clone(&self.backend);
        // Null the seekable and move it into the iterator object. (The iterator object will put it back when it drops or finishes.)
        let lock = backend.take();
        FileLinesIterator { backend, lock, startpos : self.position }
    }
}

impl<T : std::io::Read + std::io::Seek> FileLinesIterator<T>
{
    fn invalidate(&mut self)
    {
        // Invalidate by placing the original seekable back into the 
        let mut lock = self.lock.take();
        if let Some(ref mut seekable) = lock
        {
            let _ = seekable.seek(std::io::SeekFrom::Start(self.startpos));
        }
        self.backend.replace(lock);
        // prevent re-invalidation
        self.backend = Rc::new(RefCell::new(None));
    }
}

impl<T : std::io::Read + std::io::Seek> Drop for FileLinesIterator<T>
{
    fn drop(&mut self)
    {
        self.invalidate()
    }
}

impl<T : std::io::Read + std::io::Seek> Iterator for FileLinesIterator<T>
{
    type Item = String;
    fn next(&mut self) -> Option<String>
    {
        if let Some(lock) = self.lock.as_mut()
        {
            let mut bytes = vec!();
            loop
            {
                let mut buf = [0u8; 1];
                match lock.read(&mut buf)
                {
                    Ok(0) =>
                    {
                        self.invalidate();
                        if let Ok(string) = String::from_utf8(bytes.clone())
                        {
                            return Some(string);
                        }
                        else
                        {
                            return Some(String::from_utf8_lossy(&bytes).to_string());
                        }
                    }
                    Ok(_) =>
                    {
                        bytes.push(buf[0]);
                        if let Some(b'\n') = bytes.last()
                        {
                            bytes.pop();
                            if let Some(b'\r') = bytes.last()
                            {
                                bytes.pop();
                            }
                            if let Ok(string) = String::from_utf8(bytes.clone())
                            {
                                return Some(string);
                            }
                            else
                            {
                                return Some(String::from_utf8_lossy(&bytes).to_string());
                            }
                        }
                    }
                    Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
                    Err(_) =>
                    {
                        self.invalidate();
                        return None;
                    }
                }
            }
        }
        else
        {
            None
        }
    }
}
