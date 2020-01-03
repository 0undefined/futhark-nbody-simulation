import "lib/github.com/athas/vector/vspace"
import "lib/github.com/diku-dk/cpprandom/random"
import "radixtree"

module v3 = mk_vspace_3d f32

type v3 = v3.vector

module rng_engine = minstd_rand
module rand       = uniform_real_distribution f32 rng_engine

let vy_bound_lower = -100f32
let vx_bound_lower = -100f32
let vy_bound_upper =  100f32
let vx_bound_upper =  100f32

let mass_bound = 5f32

-- Constructor for v3
let vec x y z = {x, y, z}

type pointmass = {
  pos: v3,
  vel: v3,
  mass: f32
}

let init (seed: i32) (n: i32) : [n]pointmass =
  let rng = rng_engine.split_rng n <| rng_engine.rng_from_seed [seed]
  in map (\r ->
    -- retarded but it works
    let (r, px) = rand.rand (vx_bound_lower, vx_bound_upper) r
    let (r, py) = rand.rand (vy_bound_lower, vy_bound_upper) r
    let (r, pz) = rand.rand (vy_bound_lower, vy_bound_upper) r

    let (r, vx) = rand.rand (vx_bound_lower, vx_bound_upper) r
    let (r, vy) = rand.rand (vy_bound_lower, vy_bound_upper) r
    let (r, vz) = rand.rand (vy_bound_lower, vy_bound_upper) r

    let pos = {x=px, y=py, z=pz}
    let vel = {x=vx, y=vy, z=vz}

    in {pos,
        vel,
        mass = rand.rand (0.01, mass_bound) r |> (.2)}
  ) rng

let step [n] (o: [n]pointmass) : [n]pointmass =
  -- Gen sparse octree
  --   Foreach node, [8]children*, parent, center of mass, mass
  --   Morton code, https://en.wikipedia.org/wiki/Z-order_curve
  --   if n > 1 then recurse (ie. gen more buckets)
  -- Travers/apply forces
  -- *sparse, so at max we will have 8 children per node
  o

let main : f32 =
  0f32
