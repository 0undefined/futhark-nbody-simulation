import "lib/github.com/athas/vector/vspace"
import "lib/github.com/diku-dk/cpprandom/random"
import "radixtree"

type real = f32

module v3 = mk_vspace_3d f32

type v3 = v3.vector

module rng_engine = minstd_rand
module rand       = uniform_real_distribution f32 rng_engine

-- min/max values [x|y]
let vy_bound_lower : real = -400
let vx_bound_lower : real = -400
let vy_bound_upper : real =  400
let vx_bound_upper : real =  400

let mass_bound : real = 5000000

-- Constructor for v3
let vec x y z = {x, y, z}

type pointmass = {
  pos: v3,
  vel: v3,
  mass: real
}

let init_solar (seed: i32) (n: i32) : [n]pointmass =
  [{pos=v3.zero, vel=v3.zero, mass=mass_bound}, -- Center
   {pos=vec (-290)     0  0, vel=vec    0  ( 122) 0, mass=mass_bound/25},  -- Big with orbit
   {pos=vec (-267)     0  0, vel=vec    0  ( 219) 0, mass=mass_bound/300}, -- orbit
   {pos=vec (  75)     0  0, vel=vec    0  ( 259) 0, mass=mass_bound/250},
   {pos=vec ( 375) (-375) 0, vel=vec (-69) ( -69) 0, mass=mass_bound/40},
   {pos=vec (   0) (-180) 0, vel=vec (160) (   0) 0, mass=mass_bound/60}
   ]

let init (seed: i32) (n: i32) : []pointmass =
  let v = 9f32
  let rng = rng_engine.split_rng n <| rng_engine.rng_from_seed [seed]
  let bodies = map (\r ->
    -- retarded but it works
    let (r, px) = rand.rand (vx_bound_lower/1.3, vx_bound_upper/1.3) r
    let (r, py) = rand.rand (vy_bound_lower/1.3, vy_bound_upper/1.3) r
    let (r, pz) = rand.rand (vy_bound_lower/1.3, vy_bound_upper/1.3) r

    let (r, vx) = rand.rand (-v, v) r
    let (r, vy) = rand.rand (-v, v) r
    let (r, vz) = rand.rand (-v, v) r

    let pos = vec px py pz
    let vel = vec vx vy vz

    in {pos, vel,
        mass = rand.rand (mass_bound/1000, mass_bound/10) r |> (.2)}
  ) rng
  -- TODO radix-sort by morton codes first
  in [{pos=v3.zero, vel=v3.zero, mass=mass_bound}] ++ bodies

let advance_object_naive (dt: real) (o: pointmass) (a: v3) : pointmass =
  let vel = v3.(o.vel + scale dt a)
  let pos = v3.(o.pos + scale dt vel)
  in {pos, vel, mass=o.mass}

let force (a: pointmass) (b: pointmass) : v3 =
  let G'       = 1f32 -- -6.674e-11
  let r        = v3.(b.pos - a.pos)
  let inv_dist = 1 / v3.norm r -- f32.max 1f32 (v3.norm r)
  let r'       = v3.scale inv_dist r
  in v3.scale (G' * a.mass * b.mass * inv_dist**2) r'

let acceleration (a: pointmass) (f: v3) : v3 =
  v3.scale (1/a.mass) f

let step_naive [n] (dt: real) (speed: real) (os: [n]pointmass) : [n]pointmass =
  let forces = map2 (\i o ->
    scatter (map (\p -> force o p) os) [i] [v3.zero] |> reduce_comm (v3.+) v3.zero
  ) (iota n) os
  let accelerations = map2 acceleration os forces
  in map2 (advance_object_naive dt) os accelerations

let step [n] (dt: real) (speed: real) (os: [n]pointmass) : [n]pointmass =
  -- Gen sparse octree
  --   Foreach node, [8]children*, parent, center of mass, mass
  --   Morton code, https://en.wikipedia.org/wiki/Z-order_curve
  --   if n > 1 then recurse (ie. gen more buckets)
  -- Travers/apply forces
  -- *sparse, so at max we will have 8 children per node
  os

let main : real =
  0
