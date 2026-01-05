use crate::handle::{Handle, NullHandleUnderlying};

/// This is a bullshit temporary type.
#[derive(Debug)]
pub struct File;

impl NullHandleUnderlying for File {
    fn null_idx() -> usize {
        0
    }
}
