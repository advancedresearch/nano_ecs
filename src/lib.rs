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

/// Stores masks efficiently and allows fast iteration.
pub struct MaskStorage {
    /// Stores `(active, initial)` masks.
    pub masks: Vec<(u64, u64)>,
    /// Stores the offsets of the mask.
    pub offsets: Vec<usize>,
}

impl MaskStorage {
    /// Creates a new mask storage.
    pub fn new() -> MaskStorage {
        MaskStorage {masks: vec![], offsets: vec![]}
    }

    /// Gets the next range of entities with active mask pattern.
    pub fn next(&self, mask_pat: u64, mut i: usize, n: usize) -> Option<(usize, usize)> {
        loop {
            if i >= self.masks.len() {return None};
            if self.masks[i].0 & mask_pat == mask_pat {
                if let Some(&next) = self.offsets.get(i + 1) {
                    return Some((self.offsets[i], next));
                } else {
                    return Some((self.offsets[i], n));
                }
            }
            i += 1;
        }
    }

    /// Returns the active mask of component id.
    pub fn mask_of(&self, cid: usize) -> u64 {
        match self.offsets.binary_search(&cid) {
            Ok(ind) => self.masks[ind].0,
            Err(ind) if ind > 0 => self.masks[ind - 1].0,
            Err(_) => panic!("Mask storage offset not including `0`")
        }
    }

    /// Returns the initial mask of entity id.
    pub fn init_mask_of(&self, id: usize) -> u64 {
        match self.offsets.binary_search(&id) {
            Ok(ind) => self.masks[ind].1,
            Err(ind) if ind > 0 => self.masks[ind - 1].1,
            Err(_) => panic!("Mask storage offset not including `0`")
        }
    }

    /// Returns both active and initial mask of entity id.
    pub fn both_masks_of(&self, id: usize) -> (u64, u64) {
        match self.offsets.binary_search(&id) {
            Ok(ind) => self.masks[ind],
            Err(ind) if ind > 0 => self.masks[ind - 1],
            Err(_) => panic!("Mask storage offset not including `0`")
        }
    }

    /// Pushes a new mask.
    pub fn push(&mut self, mask: u64, id: usize) {
        if let Some(&(active, last)) = self.masks.last() {
            if mask == last && active == last {return};
        }
        self.masks.push((mask, mask));
        self.offsets.push(id);
    }

    /// Updates a mask for an entity, returning `Err(swap_id)` to swap components.
    pub fn update(&mut self, mask: u64, id: usize) -> Result<(), usize> {
        let ind = match self.offsets.binary_search(&id) {
            Ok(ind) => ind,
            Err(ind) if ind > 0 => {ind - 1},
            Err(_) => panic!("Mask storage offset not including `0`")
        };
        // No need to update the mask.
        if self.masks[ind].0 == mask {return Ok(())};
        let next_offset = self.offsets.get(ind + 1);
        if let Some(&next) = next_offset {
            if self.masks[ind + 1].0 == mask &&
               self.masks[ind + 1].1 == self.masks[ind].1 {
                // The next range matches the new mask.
                if next - ind == 1 {
                    // The entity is the only in old range.
                    // Just remove it since moving components is not necessary.
                    self.offsets[ind + 1] -= 1;
                    self.offsets.remove(ind);
                } else {
                    // Swap entity slices.
                    self.offsets[ind + 1] -= 1;
                    return Err(next - 1);
                }
                return Ok(());
            }
        }
        if ind > 0 {
            if let Some(&prev) = self.offsets.get(ind - 1) {
                if self.masks[ind - 1].0 == mask &&
                   self.masks[ind - 1].1 == self.masks[ind].1 {
                    // The previous range matches the new mask.
                    if next_offset.is_none() ||
                       *next_offset.unwrap() == ind + 1 {
                        // The entity is the only in old range.
                        // Just remove it since moving components is not necessary.
                        self.offsets.remove(ind);
                        self.offsets[ind - 1] += 1;
                        return Ok(());
                    } else {
                        // Swap entity slices.
                        self.offsets[ind] += 1;
                        return Err(prev + 1);
                    }
                }
            }
        }
        if let Some(&next) = next_offset {
            // A new range must be inserted.
            if next - ind == 1 {
                // The entity is the only in old range.
                // Just update the range.
                self.masks[ind].0 = mask;
            } else {
                // Detect whether to insert before or after old range.
                let j = if id == self.offsets[ind] {
                    self.offsets[ind] += 1;
                    ind
                } else {ind + 1};
                self.masks.insert(j, (mask, self.masks[ind].1));
                self.offsets.insert(j, id);
            }
        } else {
            // The entity is at the end.
            // Add a new mask range.
            self.masks.push((mask, self.masks[ind].1));
            self.offsets.push(id);
        }
        Ok(())
    }
}

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
            /// Masks for ranges of components.
            pub masks: MaskStorage,
        }

        impl World {
            /// Creates a new empty world.
            pub fn new() -> World {
                World {
                    components: vec![],
                    entities: vec![],
                    masks: MaskStorage::new(),
                }
            }

            /// Creates a new empty world with pre-allocated capacity.
            pub fn with_capacity(entities: usize, components: usize) -> World {
                World {
                    components: Vec::with_capacity(components),
                    entities: Vec::with_capacity(entities),
                    masks: MaskStorage::new(),
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
                (self.masks.mask_of(id) >> ind) & 1 == 1
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
                self.masks.mask_of(id) & mask == mask
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
            pub fn mask_of(&self, id: usize) -> u64 {self.masks.mask_of(id)}

            /// Returns the initial mask of an entity.
            #[inline(always)]
            pub fn init_mask_of(&self, id: usize) -> u64 {self.masks.init_mask_of(id)}

            /// Swaps components of two entities.
            ///
            /// This is unsafe because the number of components
            /// and their type are not checked.
            pub unsafe fn unchecked_swap_components(&mut self, id: usize, j: usize) {
                let id_slice = self.entity_slice(id);
                let mut n = id_slice.len();
                let mut id_ptr = id_slice.as_mut_ptr();
                drop(id_slice);
                let mut j_ptr = self.entity_slice(j).as_mut_ptr();
                while n > 0 {
                    std::mem::swap(&mut *id_ptr, &mut *j_ptr);
                    id_ptr = id_ptr.add(1);
                    j_ptr = j_ptr.add(1);
                    n -= 1;
                }
                self.entities.swap(id, j);
            }

            /// Enables component for entity.
            ///
            /// The entity must be pushed with the component active to enable it again.
            /// Returns `true` if successful.
            pub fn enable_component<T>(&mut self, id: usize) -> bool
                where Component: Ind<T>
            {
                let (mut mask, init_mask) = self.masks.both_masks_of(id);
                if init_mask >> <Component as Ind<T>>::ind() & 1 == 1 {
                    mask |= 1 << <Component as Ind<T>>::ind();
                    if let Err(j) = self.masks.update(mask, id) {
                        if j != id {unsafe {self.unchecked_swap_components(id, j)}}
                    }
                    true
                } else {
                    false
                }
            }

            /// Disables component for entity.
            #[inline(always)]
            pub fn disable_component<T>(&mut self, id: usize)
                where Component: Ind<T>
            {
                let mut mask = self.masks.mask_of(id);
                mask &= !(1 << <Component as Ind<T>>::ind());
                if let Err(j) = self.masks.update(mask, id) {
                    if j != id {unsafe {self.unchecked_swap_components(id, j)}}
                }
            }

            /// Disables all components for entity.
            #[inline(always)]
            pub fn disable(&mut self, id: usize) {
                if let Err(j) = self.masks.update(0, id) {
                    if j != id {unsafe {self.unchecked_swap_components(id, j)}}
                }
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

/// Used internally by other macros.
///
/// Builds a mask map from the argument signature.
#[macro_export]
macro_rules! mask_pre(
    ($mask:ident, $mask_map:ident, |$($n:ident: $x:ty),*|) => {
        let $mask: u64 = ($(1 << <Component as Ind<$x>>::ind())|*);
        let __component_len = $mask.count_ones() as isize;
        assert_eq!(__component_len, tup_count!($($n),*), "Component used twice");

        let mut $mask_map: [isize; 64] = [0; 64];
        let mut __i: isize = 0;
        let mut __bit = 0;
        while __i < __component_len {
            let mut set = false;
            $(
                if <Component as Ind<$x>>::ind() == __bit {
                    $mask_map[<Component as Ind<$x>>::ind() as usize] = __i;
                    set = true;
                }
            )*
            if set {__i += 1}
            __bit += 1;
        }
    }
);

/// Declares and executes a system.
///
/// Example: `system!(world, |pos: &mut Position| {...});`
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
        mask_pre!(__mask, __mask_map, |$($n: $x),*|);

        let __n = $world.entities.len();
        let mut __i = 0;
        while let Some((__start, __end)) = $world.masks.next(__mask, __i, __n) {
            let __init_mask = $world.masks.masks[__i].1;
            for __i in __start..__end {
                entity_unchecked_access!($world, __i, __mask, __mask_map, __init_mask,
                    $(filter: |$filter_id| $filter ;)* |$($n : $x,)*| $e);
            }
            __i += 1;
        }
    };
);

/// Same as `system!`, but with entity ids.
///
/// Example: `system_ids!(world, filter: |n| ...; id, |&Position| {...});`
#[macro_export]
macro_rules! system_ids(
    ($world:ident,
     $(filter: |$filter_id:ident| $filter:expr ;)*
     $id:ident,
     |$($n:ident: $x:ty),*| $e:expr) => {
        mask_pre!(__mask, __mask_map, |$($n: $x),*|);

        let __n = $world.entities.len();
        let mut __i = 0;
        while let Some((__start, __end)) = $world.masks.next(__mask, __i, __n) {
            let __init_mask = $world.masks.masks[__i].1;
            for __i in __start..__end {
                let $id = __i;
                entity_unchecked_access!($world, $id, __mask, __mask_map, __init_mask,
                    $(filter: |$filter_id| $filter ;)* |$($n : $x,)*| $e);
            }
            __i += 1;
        }
    };
);

/// Enumerates indices of entities only.
///
/// Pre-processes a mask map from component index to entity slice.
/// This is the same for any signature, no matter which order arguments are preserved.
#[macro_export]
macro_rules! entity_ids(
    ($world:ident, $id:ident, |$($x:ty),*| $e:expr) => {
        let __mask: u64 = ($(1 << <Component as Ind<$x>>::ind())|*);
        let __component_len = __mask.count_ones() as isize;
        assert_eq!(__component_len, tup_count!($($n),*), "Component used twice");

        let __n = $world.entities.len();
        let mut __i = 0;
        while let Some((__start, __end)) = $world.masks.next(__mask, __i, __n) {
            for __i in __start..__end {
                let $id = __i;
                $e
            }
            __i += 1;
        }
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
        mask_pre!(__mask, __mask_map, |$($n: $x),*|);

        let __i = $ind;
        entity_access!($world, __i, __mask, __mask_map, |$($n : $x,)*| $e);
    }
);

/// Accesses an entity.
///
/// This macro is used internally.
///
/// Reuses mask map if the initial mask matches exactly.
/// Otherwise, it loops through list of components in entity slice.
#[macro_export]
macro_rules! entity_access(
    ($world:ident, $i:ident, $__mask:ident, $__mask_map:ident,
     $(filter: |$filter_id:ident| $filter:expr ;)*
    |$($n:ident : $x:ty,)*| $e:expr) => {
        let __entity_init_mask = $world.init_mask_of($i);
        let __entity_mask = $world.mask_of($i);
        if __entity_init_mask & __entity_mask & $__mask == $__mask {
            $(
                let $filter_id = $i;
                if !$filter {continue};
            )*
            let __slice = $world.entity_slice($i).as_mut_ptr();
            $(
                let $n: $x = if __entity_init_mask == $__mask {
                    unsafe {
                        __slice.offset($__mask_map[<Component as Ind<$x>>::ind() as usize])
                        .get()
                    }.unwrap()
                } else {
                    let mut ptr = __slice;
                    loop {
                        if let Some(val) = unsafe {<*mut Component as Get<$x>>::get(ptr)} {
                            break val;
                        } else {
                            unsafe {ptr = ptr.add(1)};
                        }
                    }
                };
            )*
            $e
        }
    }
);

/// Accesses an entity, but without checking active mask.
///
/// This macro is used internally.
///
/// Reuses mask map if the initial mask matches exactly.
/// Otherwise, it loops through list of components in entity slice.
#[macro_export]
macro_rules! entity_unchecked_access(
    ($world:ident, $i:ident, $__mask:ident, $__mask_map:ident,
     $__entity_init_mask:ident,
     $(filter: |$filter_id:ident| $filter:expr ;)*
    |$($n:ident : $x:ty,)*| $e:expr) => {
        $(
            let $filter_id = $i;
            if !$filter {continue};
        )*
        let __slice = $world.entity_slice($i).as_mut_ptr();
        $(
            let $n: $x = if $__entity_init_mask == $__mask {
                unsafe {
                    __slice.offset($__mask_map[<Component as Ind<$x>>::ind() as usize])
                    .get()
                }.unwrap()
            } else {
                let mut ptr = __slice;
                loop {
                    if let Some(val) = unsafe {<*mut Component as Get<$x>>::get(ptr)} {
                        break val;
                    } else {
                        unsafe {ptr = ptr.add(1)};
                    }
                }
            };
        )*
        $e
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
                let mask: u64 = $(1 << <Component as Ind<$x>>::ind())|+;
                self.masks.push(mask, id);
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
