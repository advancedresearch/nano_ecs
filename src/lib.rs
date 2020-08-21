#![deny(missing_docs)]

//! # Nano-ECS
//! A bare-bones macro-based Entity-Component-System
//!
//! - Maximum 64 components per entity
//! - Stores components sequentially in same array
//! - Masks for enabled/disabled components
//!
//! ```rust
//! use nano_ecs::*;
//!
//! #[derive(Clone)]
//! pub struct Position(pub f32);
//! #[derive(Clone)]
//! pub struct Velocity(pub f32);
//!
//! ecs!{4: Position, Velocity}
//!
//! fn main() {
//!     let mut world = World::new();
//!     world.push(Position(0.0));
//!     world.push((Position(0.0), Velocity(0.0)));
//!     let dt = 1.0;
//!     system!(world, |pos: &mut Position, vel: &Velocity| {
//!         pos.0 = pos.0 + vel.0 * dt;
//!     });
//! }
//! ```
//!
//! ### Design
//!
//! The `ecs!` macro generates a `World` and `Component` object.
//!
//! Can be used with any Rust data structure that implements `Clone`.
//!
//!
//! The order of declared components is used to assign every component an index.
//! This index is used in the mask per entity and to handle slice memory correctly.
//!
//! - All components are stored in one array inside `World`.
//! - All entities have a slice refering to components
//! - All entities have a mask that enable/disable components

/// Creates an Entity-Component-System.
///
/// The first number is how many components are allowed per entity.
/// A lower number reduces compile time.
/// This can be `4, 8, 16, 32, 64`.
///
/// Example: `ecs!{4; Position, Velocity}`
#[macro_export]
macro_rules! ecs{
    ($max_components:tt : $($x:ident),*) => {
        /// Stores a single component.
        #[allow(missing_docs)]
        pub enum Component {
            $($x($x)),*
        }

        /// World storing components and entities.
        pub struct World {
            /// A list of all components.
            pub components: Vec<Component>,
            /// Entities with indices into components.
            pub entities: Vec<(usize, u8)>,
            /// Masks for enabled components per entity.
            masks: Vec<u64>,
        }

        impl World {
            /// Creates a new empty world.
            pub fn new() -> World {
                World {
                    components: vec![],
                    entities: vec![],
                    masks: vec![],
                }
            }

            /// Creates a new empty world with pre-allocated capacity.
            pub fn with_capacity(entities: usize, components: usize) -> World {
                World {
                    components: Vec::with_capacity(components),
                    entities: Vec::with_capacity(entities),
                    masks: Vec::with_capacity(entities),
                }
            }

            /// An iterator for all entities.
            #[inline(always)]
            pub fn all(&self) -> impl Iterator<Item = usize> {0..self.entities.len()}

            /// Gets entity slice of components from id.
            #[inline(always)]
            pub fn entity_slice(&mut self, id: usize) -> &mut [Component] {
                let (at, len) = self.entities[id];
                &mut self.components[at..at + len as usize]
            }

            /// Returns `true` if entity has a component by index.
            #[inline(always)]
            pub fn has_component_index(&self, id: usize, ind: u8) -> bool {
                (self.masks[id] >> ind) & 1 == 1
            }

            /// Returns `true` if entity has a component.
            #[inline(always)]
            pub fn has_component<T>(&self, id: usize) -> bool
                where Component: Ind<T>
            {
                self.has_component_index(id, self.component_index::<T>())
            }

            /// Returns `true` if entity has a specified mask (a set of components).
            #[inline(always)]
            pub fn has_mask(&self, id: usize, mask: u64) -> bool {
                self.masks[id] & mask == mask
            }

            /// Returns the component index of a component.
            #[inline(always)]
            pub fn component_index<T>(&self) -> u8
                where Component: Ind<T>
            {
                <Component as Ind<T>>::ind()
            }

            /// Returns the mask of an entity.
            #[inline(always)]
            pub fn mask_of(&self, id: usize) -> u64 {self.masks[id]}

            /// Enables component for entity.
            ///
            /// This is unsafe because the entity must store the component in memory.
            #[inline(always)]
            pub unsafe fn enable_component<T>(&mut self, id: usize)
                where Component: Ind<T>
            {
                self.masks[id] |= 1 << <Component as Ind<T>>::ind();
            }

            /// Disables component for entity.
            #[inline(always)]
            pub fn disable_component<T>(&mut self, id: usize)
                where Component: Ind<T>
            {
                self.masks[id] &= !(1 << <Component as Ind<T>>::ind());
            }

            /// Disables all components for entity.
            #[inline(always)]
            pub fn disable(&mut self, id: usize) {
                self.masks[id] = 0;
            }
        }

        push_impl!{$max_components}

        ind!{Component, 0, $($x),*}

        $(
            impl<'a> Get<&'a mut $x> for *mut Component {
                unsafe fn get(self) -> Option<&'a mut $x> {
                    if let Component::$x(x) = (&mut *self) {Some(x)} else {None}
                }
            }

            impl<'a> Get<&'a $x> for *mut Component {
                unsafe fn get(self) -> Option<&'a $x> {
                    if let Component::$x(x) = (&*self) {Some(x)} else {None}
                }
            }

            impl From<$x> for Component {
                fn from(x: $x) -> Component {Component::$x(x)}
            }
        )*
    }
}

/// Helper macro for counting size of a tuple.
///
/// This is used to check that every component in a system is uniquely accessed.
#[macro_export]
macro_rules! tup_count(
    () => {0};
    ($x0:ident $(, $y:ident)*) => {1 + tup_count!($($y),*)};
);

/// Generates mask pattern based on a set of components.
#[macro_export]
macro_rules! mask_pat(
    ($($x:ident),*) => {($(1 << <Component as Ind<$x>>::ind())|*)}
);

/// Declares and executes a system.
///
/// Example: `system!(world, |pos: &mut Position| {...});`
///
/// You can also specify the range of entities:
///
/// `system!(world, 0..4, |pos: &mut Position| {...});`
///
/// `system!(world, world.all(), |pos: &mut Position| {...});`
///
/// One or more filters can be added using the `world` object:
///
/// `system!(world, filter: |n| world.has_component::<Velocity>(); |pos: &mut Position| {...})`
///
/// Pre-processes a mask map from component index to entity slice.
/// This is the same for any signature, no matter which order arguments are preserved.
///
/// *Warning! This is unsafe to call nested when accessing same entities more than one.*
#[macro_export]
macro_rules! system(
    ($world:ident, $(filter: |$filter_id:ident| $filter:expr ;)* |$($n:ident: $x:ty),*| $e:expr) => {
        system!($world, 0..$world.entities.len(), $(filter: |$filter_id| $filter ;)* |$($n: $x),*| $e)
    };
    ($world:ident, $iter:expr, $(filter: |$filter_id:ident| $filter:expr ;)* |$($n:ident: $x:ty),*| $e:expr) => {
        let __mask: u64 = ($(1 << <Component as Ind<$x>>::ind())|*);
        let __component_len = __mask.count_ones() as isize;
        assert_eq!(__component_len, tup_count!($($n),*), "Component used twice");
        let mut __mask_map: [isize; 64] = [0; 64];
        let mut __i: isize = 0;
        let mut __bit = 0;
        while __i < __component_len {
            let mut set = false;
            $(
                if <Component as Ind<$x>>::ind() == __bit {
                    __mask_map[<Component as Ind<$x>>::ind() as usize] = __i;
                    set = true;
                }
            )*
            if set {__i += 1}
            __bit += 1;
        }
        for __i in $iter {
            if $world.has_mask(__i, __mask) {
                $(
                    let $filter_id = __i;
                    if !$filter {continue};
                )*
                let slice = $world.entity_slice(__i).as_mut_ptr();
                $(
                    let $n: $x = unsafe{
                        slice.offset(__mask_map[<Component as Ind<$x>>::ind() as usize])
                        .get()
                    }.unwrap();
                )*
                $e
            }
        }
    };
);

/// Enumerates indices of entities only.
///
/// Pre-processes a mask map from component index to entity slice.
/// This is the same for any signature, no matter which order arguments are preserved.
#[macro_export]
macro_rules! entity_ids(
    ($world:ident, $iter:expr, $id:ident, |$($x:ty),*| $e:expr) => {
        let __mask: u64 = ($(1 << <Component as Ind<$x>>::ind())|*);
        let __component_len = __mask.count_ones() as isize;
        let mut __mask_map: [isize; 64] = [0; 64];
        let mut __i: isize = 0;
        let mut __bit = 0;
        while __i < __component_len {
            let mut set = false;
            $(
                if <Component as Ind<$x>>::ind() == __bit {
                    __mask_map[<Component as Ind<$x>>::ind() as usize] = __i;
                    set = true;
                }
            )*
            if set {__i += 1}
            __bit += 1;
        }
        for __i in $iter {
            if $world.has_mask(__i, __mask) {
                let $id = __i;
                $e
            }
        }
    };
    ($world:ident, $id:ident, |$($x:ty),*| $e:expr) => {
        entity_ids!($world, 0..$world.entities.len(), $id, |$($x),*| $e)
    };
);

/// Accesses a single entity.
///
/// Pre-processes a mask map from component index to entity slice.
/// This is the same for any signature, no matter which order arguments are preserved.
///
/// *Warning! This is unsafe to call nested when accessing same entities more than one.*
#[macro_export]
macro_rules! entity(
    ($world:ident, $ind:expr, |$($n:ident: $x:ty),*| $e:expr) => {
        let __mask: u64 = ($(1 << <Component as Ind<$x>>::ind())|*);
        let __component_len = __mask.count_ones() as isize;
        assert_eq!(__component_len, tup_count!($($n),*), "Component used twice");
        let mut __mask_map: [isize; 64] = [0; 64];
        let mut __i: isize = 0;
        let mut __bit = 0;
        while __i < __component_len {
            let mut set = false;
            $(
                if <Component as Ind<$x>>::ind() == __bit {
                    __mask_map[<Component as Ind<$x>>::ind() as usize] = __i;
                    set = true;
                }
            )*
            if set {__i += 1}
            __bit += 1;
        }
        let __i = $ind;
        if $world.has_mask(__i, __mask) {
            let slice = $world.entity_slice(__i).as_mut_ptr();
            $(
                let $n: $x = unsafe{
                    slice.offset(__mask_map[<Component as Ind<$x>>::ind() as usize])
                    .get()
                }.unwrap();
            )*
            $e
        }
    }
);

/// Calls `push` macro with smaller arguments.
#[macro_export]
macro_rules! push_impl {
    (4) => {
        push_impl!{
            x0: T0, x1: T1, x2: T2, x3: T3
        }
    };
    (8) => {
        push_impl!{
            x0: T0, x1: T1, x2: T2, x3: T3, x4: T4, x5: T5,x6: T6, x7: T7
        }
    };
    (16) => {
        push_impl!{
            x0: T0, x1: T1, x2: T2, x3: T3, x4: T4, x5: T5,x6: T6, x7: T7,
            x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15
        }
    };
    (32) => {
        push_impl!{
            x0: T0, x1: T1, x2: T2, x3: T3, x4: T4, x5: T5,x6: T6, x7: T7,
            x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15,
            x16: T16, x17: T17, x18: T18, x19: T19, x20: T20, x21: T21, x22: T22, x23: T23,
            x24: T24, x25: T25, x26: T26, x27: T27, x28: T28, x29: T29, x30: T30, x31: T31
        }
    };
    (64) => {
        push_impl!{
            x0: T0, x1: T1, x2: T2, x3: T3, x4: T4, x5: T5,x6: T6, x7: T7,
            x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15,
            x16: T16, x17: T17, x18: T18, x19: T19, x20: T20, x21: T21, x22: T22, x23: T23,
            x24: T24, x25: T25, x26: T26, x27: T27, x28: T28, x29: T29, x30: T30, x31: T31,
            x32: T32, x33: T33, x34: T34, x35: T35, x36: T36, x37: T37, x38: T38, x39: T39,
            x40: T40, x41: T41, x42: T42, x43: T43, x44: T44, x45: T45, x46: T46, x47: T47,
            x48: T48, x49: T49, x50: T50, x51: T51, x52: T52, x53: T53, x54: T54, x55: T55,
            x56: T56, x57: T57, x58: T58, x59: T59, x60: T60, x61: T61, x62: T62, x63: T63
        }
    };
    ($n:ident : $x:ident) => {
        push!{$n : $x}
    };
    ($n:ident : $x:ident, $($n2:ident : $x2:ident),*) => {
        push!{$n : $x, $($n2 : $x2),*}
        push_impl!{$($n2 : $x2),*}
    };
}

/// Generates `Push` impl for `World`.
#[macro_export]
macro_rules! push{
    ($($n:ident : $x:ident),+) => {
        #[allow(unused_parens)]
        impl<$($x),*> Push<($($x),*)> for World
            where $(Component: From<$x> + Ind<$x>,)*
                  $($x: Clone),*
        {
            fn push(&mut self, ($($n),*): ($($x),*)) -> usize {
                let id = self.entities.len();
                let comp = self.components.len();
                let mask = $(1 << <Component as Ind<$x>>::ind())|+;
                self.masks.push(mask);
                let count = tup_count!($($n),*);
                assert_eq!(mask.count_ones(), count, "Component declared twice");
                let mut i = 0;
                let mut bit = 0;
                while i < count {
                    let mut set = false;
                    $(
                        if <Component as Ind<$x>>::ind() == bit {
                            self.components.push($n.clone().into());
                            set = true;
                        }
                    )*
                    if set {i += 1}
                    bit += 1;
                }
                self.entities.push((comp, count as u8));
                id
            }
        }
    }
}

/// Generates `Ind` impl for `Component`.
#[macro_export]
macro_rules! ind{
    ($c:ident, $id:expr, $x:ident) => {
        impl Ind<&mut $x> for $c {#[inline(always)] fn ind() -> u8 {$id}}
        impl Ind<&$x> for $c {#[inline(always)] fn ind() -> u8 {$id}}
        impl Ind<$x> for $c {#[inline(always)] fn ind() -> u8 {$id}}
    };
    ($c:ident, $id:expr, $x:ident, $($y:ident),+) => {
        impl Ind<&mut $x> for $c {#[inline(always)] fn ind() -> u8 {$id}}
        impl Ind<&$x> for $c {#[inline(always)] fn ind() -> u8 {$id}}
        impl Ind<$x> for $c {#[inline(always)] fn ind() -> u8 {$id}}
        ind!{$c, $id + 1, $($y),+}
    };
}

/// The index of a component type `T` from `Component`.
///
/// This is used to store the components in the declared order.
pub trait Ind<T> {
    /// Returns the component index.
    fn ind() -> u8;
}
/// Gets a component type `T` from a raw pointer of `Component`.
///
/// Implemented for `&mut T` and `&T`.
pub trait Get<T> {
    /// Gets component type.
    ///
    /// This is an unsafe method because the lifetime of the return value is only valid for the scope.
    unsafe fn get(self) -> Option<T>;
}
/// Creates a new entity from a set of components.
pub trait Push<T> {
    /// Pushes/spawns a new entity.
    fn push(&mut self, val: T) -> usize;
}
