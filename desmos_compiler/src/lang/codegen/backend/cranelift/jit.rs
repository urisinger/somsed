use crate::lang::codegen::backend::jit::{ExplicitFn, ImplicitFn};

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ListLayout {
    pub size: i64,
    pub ptr: *mut u8,
}

pub struct ExplicitFnImpl<T> {
    pub function: unsafe extern "C" fn(f64) -> T,
}

impl<T> ExplicitFnImpl<T> {
    pub unsafe fn from_raw(ptr: *const u8) -> Self {
        let function = std::mem::transmute::<*const u8, unsafe extern "C" fn(f64) -> T>(ptr);
        Self { function }
    }
}

impl<T> ExplicitFn<T> for ExplicitFnImpl<T> {
    fn call(&self, x: f64) -> T {
        unsafe { (self.function)(x) }
    }
}

pub struct ImplicitFnImpl<T> {
    pub function: unsafe extern "C" fn(f64, f64) -> T,
}

impl<T> ImplicitFnImpl<T> {
    pub unsafe fn from_raw(ptr: *const u8) -> Self {
        let function = std::mem::transmute::<*const u8, unsafe extern "C" fn(f64, f64) -> T>(ptr);
        Self { function }
    }
}

impl<T> ImplicitFn<T> for ImplicitFnImpl<T> {
    fn call_implicit(&self, x: f64, y: f64) -> T {
        unsafe { (self.function)(x, y) }
    }
}

pub struct ExplicitListFnImpl {
    pub function: unsafe extern "C" fn(f64) -> ListLayout,
}

impl ExplicitListFnImpl {
    pub unsafe fn from_raw(ptr: *const u8) -> Self {
        let function =
            std::mem::transmute::<*const u8, unsafe extern "C" fn(f64) -> ListLayout>(ptr);
        Self { function }
    }
}

impl<T: Clone> ExplicitFn<Vec<T>> for ExplicitListFnImpl {
    fn call(&self, x: f64) -> Vec<T> {
        let list_layout = unsafe { (self.function)(x) };
        convert_list(&list_layout)
    }
}

impl ImplicitListFnImpl {
    pub unsafe fn from_raw(ptr: *const u8) -> Self {
        let function =
            std::mem::transmute::<*const u8, unsafe extern "C" fn(f64, f64) -> ListLayout>(ptr);
        Self { function }
    }
}

pub struct ImplicitListFnImpl {
    pub function: unsafe extern "C" fn(f64, f64) -> ListLayout,
}

impl<T: Clone> ImplicitFn<Vec<T>> for ImplicitListFnImpl {
    fn call_implicit(&self, x: f64, y: f64) -> Vec<T> {
        let list_layout = unsafe { (self.function)(x, y) };
        convert_list(&list_layout)
    }
}

pub fn convert_list<T: Clone>(list_layout: &ListLayout) -> Vec<T> {
    unsafe {
        if list_layout.ptr.is_null() {
            return Vec::new();
        }

        // Compute the number of elements in the list
        let element_count = list_layout.size as usize;

        // Convert the raw pointer into a slice
        let slice = std::slice::from_raw_parts(list_layout.ptr as *const T, element_count);

        // Clone the slice into a Vec to create a safe wrapper
        slice.to_vec()
    }
}
