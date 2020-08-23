use nano_ecs::*;

#[derive(Clone)]
pub struct Position(pub f32);
#[derive(Clone)]
pub struct Velocity(pub f32);

ecs!{4: Position, Velocity}

fn main() {
    let mut world = World::new();
    world.push((Position(0.2), Velocity(0.0)));
    world.push((Position(0.3), Velocity(0.0)));
    world.push(Position(0.0));
    world.push((Position(0.4), Velocity(0.0)));
    world.push(Position(0.1));
    world.push((Position(0.5), Velocity(0.0)));
    world.optimize();
    system!(world, |pos: &Position| {
        println!("{:?}", pos.0);
    });
}
