use std::{collections::HashMap, rc::Rc};
use crate::{file::File, handle::Handle};

/// Used to **store** a sequence of interned bytes. `Substring` should never be used explicitly, it is just a value to
/// be transformed into either `&[u8]` via `.as_ref()` or into a dynamically allocated string, both of which are
/// responsibilities of the interner to do in `get()` and `get_string()` respectively.
#[derive(Debug, Clone, Hash)]
pub struct Substring(pub(self) Rc<[u8]>);

/// Used to store all string data from the source code in a central location, applying optimizations derived from
/// contiguous memory storage and allocation, as well as prevent uneccesary heap allocations and clones.
#[derive(Debug)]
pub struct Interner {
    /// The file to be interning for.
    file: Handle<File>,
    /// A map from the substring handles to the substring themselves.
    handle_map: HashMap<Handle<Substring>, Substring>,
    /// A map from the underlying byte data to the handles.
    bytes_map: HashMap<Rc<[u8]>, Handle<Substring>>, 
}

impl Interner {
    /// Create a new interner for the provided file.
    pub fn new( file: Handle<File>,) -> Self {
        Self {
            file,
            handle_map: HashMap::new(),
            bytes_map: HashMap::new(),
        }
    }

    /// Intern a sequence of bytes and return a handle to that substring. If an equivalent sequence has already been
    /// interned, then it will just return the pre-existing handle.
    pub fn intern(&mut self, word: &[u8]) -> Handle<Substring> {
        // First check if it already exists
        if let Some(handle) = self.bytes_map.get(word) {
            return *handle;
        }

        // Create an owned Rc<[u8]>
        let substring = Substring(Rc::from(word));

        // Generate a key
        let handle = Handle::<Substring>::new(self.handle_map.len());

        // Insert into both maps
        self.handle_map.insert(handle, substring.clone());
        self.bytes_map.insert(substring.0, handle);

        handle
    }

    /// Returns a reference to the sequence of bytes indexed by the handle.
    pub fn get(&self, handle: Handle<Substring>) -> Option<&[u8]> {
        self.handle_map.get(&handle).map(|rc| rc.0.as_ref())
    }

    /// Returns a dynamically allocated string given be the sequence of bytes index by the handle.
    pub fn get_string(&self, handle: Handle<Substring>) -> Option<String> {
        let bytes = self.handle_map.get(&handle).map(|rc| rc.0.as_ref())?;
        String::from_utf8(bytes.iter().map(|b| *b).collect()).ok()
    }
    
    /// Attempts to find a handle that indexes the provided sequence of bytes.
    /// It will return `None` if no handle exists.
    pub fn resolve(&self, word: &[u8]) -> Option<Handle<Substring>> {
        self.bytes_map.get(word).map(|h| *h)
    }
}