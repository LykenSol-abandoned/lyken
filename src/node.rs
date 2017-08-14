use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

pub struct Node<T> {
    ptr: Rc<T>,
}

impl<T> Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.ptr
    }
}

impl<T: fmt::Debug> fmt::Debug for Node<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T> Node<T> {
    pub fn new(value: T) -> Self {
        Node { ptr: Rc::new(value) }
    }
}
