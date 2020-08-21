# Nano-ECS
A bare-bones macro-based Entity-Component-System

- Maximum 64 components per entity
- Stores components sequentially in same array
- Masks for enabled/disabled components

```rust
use nano_ecs::*;

#[derive(Clone)]
pub struct Position(pub f32);
#[derive(Clone)]
pub struct Velocity(pub f32);

ecs!{4: Position, Velocity}

fn main() {
    let mut world = World::new();
    world.push(Position(0.0));
    world.push((Position(0.0), Velocity(0.0)));
    let dt = 1.0;
    system!(world, |pos: &mut Position, vel: &Velocity| {
        pos.0 = pos.0 + vel.0 * dt;
    });
}
```

### Design

The `ecs!` macro generates a `World` and `Component` object.

Can be used with any Rust data structure that implements `Clone`.


The order of declared components is used to assign every component an index.
This index is used in the mask per entity and to handle slice memory correctly.

- All components are stored in one array inside `World`.
- All entities have a slice refering to components
- All entities have a mask that enable/disable components
