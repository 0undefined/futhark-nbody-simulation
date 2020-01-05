import "lib/github.com/athas/vector/vspace"
import "lib/github.com/diku-dk/cpprandom/random"
import "radixtree"

module v3 = mk_vspace_3d f32

type v3 = v3.vector

module rng_engine = minstd_rand
module rand       = uniform_real_distribution f32 rng_engine

-- min/max values [x|y]
let vy_bound_lower = -100f32
let vx_bound_lower = -100f32
let vy_bound_upper =  100f32
let vx_bound_upper =  100f32

let mass_bound = 1000.0f32

-- Constructor for v3
let vec x y z = {x, y, z}

type pointmass = {
  pos: v3,
  vel: v3,
  mass: f32
}

let init (seed: i32) (n: i32) : []pointmass =
  let rng = rng_engine.split_rng n <| rng_engine.rng_from_seed [seed]
  let bodies = map (\r ->
    -- retarded but it works
    let (r, px) = rand.rand (vx_bound_lower/2, vx_bound_upper/2) r
    let (r, py) = rand.rand (vy_bound_lower/2, vy_bound_upper/2) r
    let (r, pz) = rand.rand (vy_bound_lower/2, vy_bound_upper/2) r

    let (r, vx) = rand.rand (-2.5, 2.5) r
    let (r, vy) = rand.rand (-2.5, 2.5) r
    let (r, vz) = rand.rand (-2.5, 2.5) r

    let pos = vec px py pz
    let vel = vec vx vy vz

    in {pos, vel,
        mass = rand.rand (0.5, mass_bound/200) r |> (.2)}
  ) rng
  -- TODO radix-sort by morton codes first
  in bodies ++ [{pos=v3.zero, vel=v3.zero, mass=mass_bound}]

let advance_object_naive (dt: f32) (o: pointmass) (a: v3) : pointmass =
  --let dt = dt / 20000
  let acc = v3.scale o.mass a
  let vel = v3.(o.vel + scale dt acc)
  let pos = v3.(o.pos + scale dt vel)
  in {pos, vel, mass=o.mass}

let acceleration (speed: f32) (a: pointmass) (b: pointmass) : v3 =
  let dist = v3.(b.pos - a.pos)
  let dist_sqr = v3.dot dist dist + speed * speed
  let inverse = 1.0f32 / f32.sqrt dist_sqr
  let scalar = a.mass * (inverse**2)
  in v3.scale scalar dist

let step_naive [n] (dt: f32) (speed: f32) (os: [n]pointmass) : [n]pointmass =
  let accelerations =
    let delta_velocity (p : pointmass) = map (acceleration speed p) os |> reduce_comm (v3.+) v3.zero
    in map delta_velocity os
  in map2 (advance_object_naive dt) os accelerations

let step [n] (dt: f32) (speed: f32) (os: [n]pointmass) : [n]pointmass =
  -- Gen sparse octree
  --   Foreach node, [8]children*, parent, center of mass, mass
  --   Morton code, https://en.wikipedia.org/wiki/Z-order_curve
  --   if n > 1 then recurse (ie. gen more buckets)
  -- Travers/apply forces
  -- *sparse, so at max we will have 8 children per node
  os

let main : f32 =
  0f32
