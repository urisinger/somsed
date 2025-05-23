use std::alloc::{alloc, dealloc, Layout};

/// Allocate memory for `size` bytes and return a pointer to the allocated memory.
/// ## Safety
/// This is marked unsafe becuase it allocates a raw pointer
pub unsafe extern "C" fn malloc(size: usize, allign: usize) -> *mut u8 {
    if size == 0 {
        return std::ptr::null_mut(); // Return null if zero bytes are requested
    }

    // Create a layout for the requested size
    let layout = Layout::from_size_align(size, allign).expect("Invalid layout");
    // Allocate memory and return the pointer
    let ptr = alloc(layout);
    if ptr.is_null() {
        panic!("Memory allocation failed");
    }
    ptr
}

/// Deallocate memory for the given pointer and size.
/// ## Safety
/// This is marked unsafe becuase it frees a raw pointer
pub unsafe extern "C" fn free(ptr: *mut u8, size: usize, allign: usize) {
    if !ptr.is_null() {
        // Create a layout for the size to free
        let layout = Layout::from_size_align(size, allign).expect("Invalid layout");
        dealloc(ptr, layout);
    }
}
