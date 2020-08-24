use nano_ecs::*;

#[derive(Clone)]
pub struct Position(pub f32);
#[derive(Clone)]
pub struct Velocity(pub f32);

ecs!{4: Position, Velocity}

fn main() {
    let mut world = World::new();
    world.push((Position(0.0), Velocity(0.0)));
    world.push(Position(1.0));
    system_ids!(world,
        ?|n| !world.has_component::<Velocity>(n);
        id,
    |pos: &Position| {
        println!("{}: {}", id, pos.0);
    });
}
