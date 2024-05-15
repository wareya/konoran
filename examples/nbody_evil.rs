// an implementation of the infamous nbody benchmark!
// rewritten based on these versions:
// https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/nbody-rust-2.html
// https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/nbody-rust-3.html
// and others

use std::ops::Add;
use std::ops::Sub;
use std::ops::Mul;

#[derive(Clone, Copy)]
struct Vec3
{
    pub x : f64,
    pub y : f64,
    pub z : f64,
}

impl Add for Vec3 {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Vec3{ x : self.x + rhs.x, y : self.y + rhs.y, z : self.z + rhs.z }
    }
}

impl Sub for Vec3 {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Vec3{ x : self.x - rhs.x, y : self.y - rhs.y, z : self.z - rhs.z }
    }
}

impl Mul<f64> for Vec3 {
    type Output = Self;
    fn mul(self, rhs: f64) -> Self {
        Vec3{ x : self.x * rhs, y : self.y * rhs, z : self.z * rhs }
    }
}

#[derive(Clone, Copy)]
struct Body {
    pos: Vec3,
    vel: Vec3,
    mass: f64,
}

fn vec3_sq_length(a : Vec3) -> f64
{
    a.x*a.x + a.y*a.y + a.z*a.z
}
fn vec3_length(a : Vec3) -> f64
{
    vec3_sq_length(a).sqrt()
}
fn advance(bodies : &mut [Body; 5], delta : f64)
{
    for i in 0..5
    {
        let mass = bodies[i].mass;
        let pos = bodies[i].pos;
        let mut vel = bodies[i].vel;
        
        for j in (i+1)..5
        {
            let mass2 = bodies[j].mass;
            let pos2 = bodies[j].pos;
            let mut vel2 = bodies[j].vel;
            
            let pos_diff = pos - pos2;
            let distance_sq = vec3_sq_length(pos_diff);
            let distance = distance_sq.sqrt();
            let mag = delta / (distance * distance_sq);
            
            vel = vel - (pos_diff * (mass2 * mag));
            vel2 = vel2 + (pos_diff * (mass * mag));
            
            bodies[j].vel = vel2;
        }
        
        bodies[i].vel = vel;
        bodies[i].pos = pos + (vel * delta);
    }
}

fn energy(bodies : &[Body; 5]) -> f64
{
    let mut energy = 0.0f64;
    for i in 0..5
    {
        let body = bodies[i];
        let mass = body.mass; 
        let vel = body.vel;
        let pos = body.pos;
        
        energy = energy + mass * vec3_sq_length(vel) * 0.5f64;
        
        for j in (i+1)..5
        {
            let body2 = bodies[j];
            let mass2 = body2.mass; 
            //let vel2 = body2.vel;
            let pos2 = body2.pos;
            
            let pos_diff = pos - pos2;
            let dist = vec3_length(pos_diff);
            energy = energy - mass * mass2 * (1.0f64/dist);
        }
    }
    
    return energy;
}

fn offset_momentum(bodies : &mut [Body; 5])
{
    const PI  : f64= 3.141592653589793f64;
    const SOLAR_MASS  : f64= 4.0f64 * PI * PI;
    for i in 0..5
    {
        let ratio = bodies[i].mass / SOLAR_MASS;
        bodies[0].vel = bodies[0].vel - ((*bodies)[i].vel * ratio);
    }
    return;
}
fn nbody_bench(count : u64)
{
    const PI : f64 = 3.141592653589793f64;
    const SOLAR_MASS : f64 = 4.0f64 * PI * PI;
    const DAYS_PER_YEAR : f64 = 365.24f64;
    
    let mut bodies = [
        Body {
            pos : Vec3 { x : 0.0f64, y : 0.0f64, z : 0.0f64 },
            vel : Vec3 { x : 0.0f64, y : 0.0f64, z : 0.0f64 },
            mass : SOLAR_MASS
        },
        Body {
            pos : Vec3 { x : 4.84143144246472090e+00f64, y : -1.16032004402742839e+00f64, z : -1.03622044471123109e-01f64 },
            vel : Vec3 { x : 1.66007664274403694e-03f64 * DAYS_PER_YEAR, y : 7.69901118419740425e-03f64 * DAYS_PER_YEAR, z : -6.90460016972063023e-05f64 * DAYS_PER_YEAR },
            mass : 9.54791938424326609e-04f64 * SOLAR_MASS
        },
        Body {
            pos : Vec3 { x : 8.34336671824457987e+00f64, y : 4.12479856412430479e+00f64, z : -4.03523417114321381e-01f64 },
            vel : Vec3 { x : -2.76742510726862411e-03f64 * DAYS_PER_YEAR, y : 4.99852801234917238e-03f64 * DAYS_PER_YEAR, z : 2.30417297573763929e-05f64 * DAYS_PER_YEAR },
            mass : 2.85885980666130812e-04f64 * SOLAR_MASS
        },
        Body {
            pos : Vec3 { x : 1.28943695621391310e+01f64, y : -1.51111514016986312e+01f64, z : -2.23307578892655734e-01f64 },
            vel : Vec3 { x : 2.96460137564761618e-03f64 * DAYS_PER_YEAR, y : 2.37847173959480950e-03f64 * DAYS_PER_YEAR, z : -2.96589568540237556e-05f64 * DAYS_PER_YEAR },
            mass : 4.36624404335156298e-05f64 * SOLAR_MASS
        },
        Body {
            pos : Vec3 { x : 1.53796971148509165e+01f64, y : -2.59193146099879641e+01f64, z : 1.79258772950371181e-01f64 },
            vel : Vec3 { x : 2.68067772490389322e-03f64 * DAYS_PER_YEAR, y : 1.62824170038242295e-03f64 * DAYS_PER_YEAR, z : -9.51592254519715870e-05f64 * DAYS_PER_YEAR },
            mass : 5.15138902046611451e-05f64 * SOLAR_MASS
        }
    ];
    
    offset_momentum(&mut bodies);
    
    let start_energy = energy(&bodies);
    println!("{:.9}", start_energy);
    
    for _ in 0..count
    {
        advance(&mut bodies, 0.01f64);
    }
    let end_energy = energy(&bodies);
    println!("{:.9}", end_energy);
    
    return;
}

fn main()
{
    let count = std::env::args_os().nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(50000000u64);
    
    nbody_bench(count);
}