import "lib/github.com/athas/vector/vspace"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/athas/matte/colour"
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

-- return flat index + colour of point
let drawpoint (x: f32) (y: f32) (z: f32) (w: f32) (height: i32) (width: i32) : (i32, i32) =
  -- ignore if out of window bounds
  if   x < vx_bound_lower
    || x > vx_bound_upper
    || y < vy_bound_lower
    || y > vy_bound_upper then
    (-1, 0)
  else
    -- normalize, [0;1]
    let x' = (x - vx_bound_lower) / (vx_bound_upper - vx_bound_lower)
    let y' = (y - vy_bound_lower) / (vy_bound_upper - vy_bound_lower)
    -- project back to height/width
    let x'' = i32.f32 (x' * (f32.i32 width))
    let y'' = i32.f32 (y' * (f32.i32 height))
    -- colour is determined by mass + z-position
    -- red: one thicc boy
    -- bright: one close boy
    let colour_weight = 0.2 + 0.8 * (w / mass_bound)
    let colour_z      = 0.2 + 0.8 * ((z - vy_bound_lower) / (vy_bound_upper - vy_bound_lower))
    let colour        = argb.mix colour_weight argb.red colour_z argb.white

    in (y'' * width + x'', colour)

let render [n] (os: [n]pointmass) (height: i32) (width: i32) : [height][width]i32 =
  let backdrop = argb.black
  let (is, cs) = unzip (map (\o -> drawpoint o.pos.x o.pos.y o.pos.z o.mass height width) os)

  in unflatten height width (scatter (replicate (height * width) backdrop) is cs)

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
