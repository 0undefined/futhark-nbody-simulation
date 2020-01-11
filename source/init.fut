import "lib/github.com/diku-dk/cpprandom/random"
import "types"

module rng_engine = minstd_rand
module rand       = uniform_real_distribution f32 rng_engine

let init_solar (_: i32) (n: i32) : [n]pointmass =
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
