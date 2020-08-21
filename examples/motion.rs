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
