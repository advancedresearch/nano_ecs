use nano_ecs::*;

#[derive(Clone)]
pub struct Position(pub f32);
#[derive(Clone)]
pub struct Velocity(pub f32);

ecs!{4: Position, Velocity}

fn main() {
    let mut world = World::new();
    world.push(Position(0.0));
    let a = world.push((Position(0.1), Velocity(0.0)));
    world.disable_component::<Position>(a);
    world.enable_component::<Position>(a);
    system!(world, |pos: &mut Position, vel: &Velocity| {
        println!("{:?}", pos.0);
    });

    let mut world = World::new();
    let a = world.push((Position(0.2), Velocity(0.0)));
    world.push(Position(0.0));
    world.disable_component::<Position>(a);
    world.enable_component::<Position>(a);
    system!(world, |pos: &mut Position, vel: &Velocity| {
        println!("{:?}", pos.0);
    });

    let mut world = World::new();
    world.push(Position(0.0));
    let a = world.push((Position(0.3), Velocity(0.0)));
    world.push(Position(0.0));
    world.disable_component::<Position>(a);
    world.enable_component::<Position>(a);
    system!(world, |pos: &mut Position, vel: &Velocity| {
        println!("{:?}", pos.0);
    });

    let mut world = World::new();
    world.push(Position(0.0));
    world.push((Position(0.4), Velocity(0.0)));
    let a = world.push((Position(0.5), Velocity(0.0)));
    world.push(Position(0.0));
    world.disable_component::<Position>(a);
    world.enable_component::<Position>(a);
    system!(world, |pos: &mut Position, vel: &Velocity| {
        println!("{:?}", pos.0);
    });

    let mut world = World::new();
    world.push(Position(0.0));
    let a = world.push((Position(0.6), Velocity(0.0)));
    world.push((Position(0.7), Velocity(0.0)));
    world.push(Position(0.0));
    world.disable_component::<Position>(a);
    world.enable_component::<Position>(a);
    system!(world, |pos: &mut Position, vel: &Velocity| {
        println!("{:?}", pos.0);
    });

    let mut world = World::new();
    world.push(Position(0.0));
    world.push((Position(0.8), Velocity(0.0)));
    let a = world.push((Position(0.9), Velocity(0.0)));
    world.push((Position(1.0), Velocity(0.0)));
    world.push(Position(0.0));
    world.disable_component::<Position>(a);
    world.enable_component::<Position>(a);
    system!(world, |pos: &mut Position, vel: &Velocity| {
        println!("{:?}", pos.0);
    });
}
