use cranelift_module::{FuncOrDataId, Module};

use desmos_compiler::lang::codegen::{
    ir::{IRScalerType, IRType},
    jit::{
        function::{ExplicitFn, ExplicitJitFn, ImplicitFn, ImplicitJitFn, JitValue, PointValue},
        ExecutionEngine,
    },
};

use super::CraneliftBackend;

impl ExecutionEngine for CraneliftBackend {
    type ExplicitNumberFn = ExplicitFnImpl<f64>;
    type ExplicitPointFn = ExplicitFnImpl<PointValue>;
    type ExplicitNumberListFn = ExplicitListFnImpl;
    type ExplicitPointListFn = ExplicitListFnImpl;

    type ImplicitNumberFn = ImplicitFnImpl<f64>;
    type ImplicitPointFn = ImplicitFnImpl<PointValue>;
    type ImplicitNumberListFn = ImplicitListFnImpl;
    type ImplicitPointListFn = ImplicitListFnImpl;

    fn eval(&self, name: &str, ty: &IRType) -> Option<JitValue> {
        let func_id = if let FuncOrDataId::Func(id) = self.module.get_name(name)? {
            id
        } else {
            return None;
        };

        unsafe {
            Some(match ty {
                IRType::Scaler(IRScalerType::Number) => JitValue::Number(std::mem::transmute::<
                    *const u8,
                    unsafe extern "C" fn() -> f64,
                >(
                    self.module.get_finalized_function(func_id),
                )()),

                IRType::Scaler(IRScalerType::Point) => JitValue::Point(std::mem::transmute::<
                    *const u8,
                    unsafe extern "C" fn() -> PointValue,
                >(
                    self.module.get_finalized_function(func_id),
                )()),
                IRType::List(list_t) => match list_t {
                    IRScalerType::Number => {
                        JitValue::NumberList(convert_list(&std::mem::transmute::<
                            *const u8,
                            unsafe extern "C" fn() -> ListLayout,
                        >(
                            self.module.get_finalized_function(func_id),
                        )()))
                    }
                    IRScalerType::Point => {
                        JitValue::PointList(convert_list(&std::mem::transmute::<
                            *const u8,
                            unsafe extern "C" fn() -> ListLayout,
                        >(
                            self.module.get_finalized_function(func_id),
                        )()))
                    }
                },
            })
        }
    }

    fn get_explicit_fn(&self, name: &str, ty: &IRType) -> Option<ExplicitJitFn<Self>> {
        let func_id = if let FuncOrDataId::Func(id) = self.module.get_name(name)? {
            id
        } else {
            return None;
        };

        unsafe {
            Some(match ty {
                IRType::Scaler(IRScalerType::Number) => ExplicitJitFn::Number(
                    ExplicitFnImpl::from_raw(self.module.get_finalized_function(func_id)),
                ),

                IRType::Scaler(IRScalerType::Point) => ExplicitJitFn::Point(
                    ExplicitFnImpl::from_raw(self.module.get_finalized_function(func_id)),
                ),
                IRType::List(list_t) => match list_t {
                    IRScalerType::Number => ExplicitJitFn::NumberList(
                        ExplicitListFnImpl::from_raw(self.module.get_finalized_function(func_id)),
                    ),
                    IRScalerType::Point => ExplicitJitFn::PointList(ExplicitListFnImpl::from_raw(
                        self.module.get_finalized_function(func_id),
                    )),
                },
            })
        }
    }

    fn get_implicit_fn(&self, name: &str, ty: &IRType) -> Option<ImplicitJitFn<Self>> {
        let func_id = if let FuncOrDataId::Func(id) = self.module.get_name(name)? {
            id
        } else {
            return None;
        };

        unsafe {
            Some(match ty {
                IRType::Scaler(IRScalerType::Number) => ImplicitJitFn::Number(
                    ImplicitFnImpl::from_raw(self.module.get_finalized_function(func_id)),
                ),

                IRType::Scaler(IRScalerType::Point) => ImplicitJitFn::Point(
                    ImplicitFnImpl::from_raw(self.module.get_finalized_function(func_id)),
                ),
                IRType::List(list_t) => match list_t {
                    IRScalerType::Number => ImplicitJitFn::NumberList(
                        ImplicitListFnImpl::from_raw(self.module.get_finalized_function(func_id)),
                    ),
                    IRScalerType::Point => ImplicitJitFn::PointList(ImplicitListFnImpl::from_raw(
                        self.module.get_finalized_function(func_id),
                    )),
                },
            })
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ListLayout {
    pub size: i64,
    pub ptr: *mut u8,
}

pub struct ExplicitFnImpl<T> {
    function: unsafe extern "C" fn(f64) -> T,
}

impl<T> ExplicitFnImpl<T> {
    pub(crate) unsafe fn from_raw(ptr: *const u8) -> Self {
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
    function: unsafe extern "C" fn(f64, f64) -> T,
}

impl<T> ImplicitFnImpl<T> {
    pub(crate) unsafe fn from_raw(ptr: *const u8) -> Self {
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
    function: unsafe extern "C" fn(f64) -> ListLayout,
}

impl ExplicitListFnImpl {
    pub(crate) unsafe fn from_raw(ptr: *const u8) -> Self {
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
    pub(crate) unsafe fn from_raw(ptr: *const u8) -> Self {
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
