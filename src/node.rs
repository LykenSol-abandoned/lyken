use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::Unsize;
use std::ops::{CoerceUnsized, Deref};
use std::rc::{Rc, Weak};

pub struct Node<T: ?Sized> {
    ptr: Rc<T>,
}

impl<T: ?Sized + Unsize<U>, U: ?Sized> CoerceUnsized<Node<U>> for Node<T> {}

impl<T: ?Sized> Clone for Node<T> {
    fn clone(&self) -> Self {
        Node {
            ptr: self.ptr.clone(),
        }
    }
}

impl<T: ?Sized> Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.ptr
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.ptr, &other.ptr)
    }
}

impl<T> Eq for Node<T> {}

impl<T> Hash for Node<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        node_key(self).hash(state)
    }
}

impl<T: fmt::Debug> fmt::Debug for Node<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T> Node<T> {
    pub fn new(value: T) -> Self {
        Node {
            ptr: Rc::new(value),
        }
    }
}

impl Node<Any> {
    pub fn downcast<T: 'static>(&self) -> Option<Node<T>> {
        if self.is::<T>() {
            // HACK replace this with Rc::into_raw when usable with !Sized.
            ::std::mem::forget(self.clone());
            Some(Node {
                ptr: unsafe { Rc::from_raw(&**self as *const Any as *const T) },
            })
        } else {
            None
        }
    }
}

pub struct NodeMap<V> {
    map: Rc<RefCell<HashMap<OpaqueKey, V>>>,
}

type OpaqueKey = *const ();

fn node_key<T: ?Sized>(node: &Node<T>) -> OpaqueKey {
    &**node as *const T as OpaqueKey
}

thread_local!(static NODE_MAPS: RefCell<Vec<Rc<NodeRemove>>> = RefCell::new(vec![]));

trait NodeRemove {
    fn remove_node(&self, key: OpaqueKey);
}

impl<V> NodeRemove for RefCell<HashMap<OpaqueKey, V>> {
    fn remove_node(&self, key: OpaqueKey) {
        let value = self.borrow_mut().remove(&key);
        drop(value);
    }
}

impl<T: ?Sized> Drop for Node<T> {
    fn drop(&mut self) {
        if Rc::strong_count(&self.ptr) == 1 {
            NODE_MAPS.with(|maps| for map in maps.borrow().iter() {
                map.remove_node(node_key(self));
            })
        }
    }
}

impl<V: 'static + Clone> NodeMap<V> {
    pub fn new() -> Self {
        let map = Rc::new(RefCell::new(HashMap::new()));
        NODE_MAPS.with(|maps| maps.borrow_mut().push(map.clone()));
        NodeMap { map }
    }

    pub fn entry<'a, T: ?Sized>(&self, key: &'a Node<T>) -> NodeMapEntry<'a, T, V> {
        NodeMapEntry {
            key,
            map: Rc::downgrade(&self.map),
        }
    }

    pub fn get<T>(&self, key: &Node<T>) -> Option<V> {
        self.entry(key).get()
    }

    pub fn set<T>(&self, key: &Node<T>, value: V) {
        self.entry(key).set(value)
    }
}

pub struct NodeMapEntry<'a, T: ?Sized + 'a, V> {
    key: &'a Node<T>,
    map: Weak<RefCell<HashMap<OpaqueKey, V>>>,
}

impl<'a, T: ?Sized, V: Clone> NodeMapEntry<'a, T, V> {
    pub fn get(&self) -> Option<V> {
        let map = self.map.upgrade().unwrap();
        let value = map.borrow().get(&node_key(self.key)).cloned();
        value
    }

    pub fn set(&self, value: V) {
        let map = self.map.upgrade().unwrap();
        let prev = map.borrow_mut().insert(node_key(self.key), value);
        drop(prev);
    }
}

macro_rules! node_field {
    ($name:ident: $ty:ty) => {
        impl<T: ?Sized> ::node::Node<T> {
            pub fn $name(&self) -> ::node::NodeMapEntry<T, $ty> {
                thread_local!(static MAP: ::node::NodeMap<$ty> = ::node::NodeMap::new());
                MAP.with(|map| map.entry(self))
            }
        }
    }
}
