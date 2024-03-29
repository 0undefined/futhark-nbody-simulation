import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/sorts/radix_sort"
import "types"
import "BHtree"

module rng_engine = minstd_rand
module rand       = uniform_real_distribution f64 rng_engine


let init_rand (seed: u64) (n: i64) : [n]pointmass =
  let rng = rng_engine.split_rng n <| rng_engine.rng_from_seed [i32.u64 seed]
  let bodies = map (\r ->
    -- retarded but it works
    let (r, px) = rand.rand (vx_bound_lower/1.3, vx_bound_upper/1.3) r
    let (r, py) = rand.rand (vy_bound_lower/1.3, vy_bound_upper/1.3) r
    let (r, pz) = rand.rand (vy_bound_lower/1.3, vy_bound_upper/1.3) r

    let (r, vx) = rand.rand (-initial_vel, initial_vel) r
    let (r, vy) = rand.rand (-initial_vel, initial_vel) r
    let (r, vz) = rand.rand (-initial_vel, initial_vel) r

    let pos = vec px py pz
    let vel = vec vx vy vz

    in {pos, vel,
        mass = rand.rand (mass_bound/1000, mass_bound/10) r |> (.1)}
  ) rng
  in bodies


let init_circle (seed: u64) (n: i64) : [n]pointmass =
  let c = vec 0 0 0
  let filter_fun (bs: []pointmass) = filter (\b ->
    let {x, y, z} = b.pos
    in (x-c.x)**2 + (y-c.y)**2 + (z-c.z)**2 - (vx_bound_upper - vx_bound_lower |> (/4f64))**2f64 < 0f64
  ) bs

  let bodies = loop b=(init_rand seed n |> filter_fun)
  while length b < n do
    b++(init_rand (seed + u64.i64 (length b)) n |> filter_fun)

  in bodies[:n]


-- Arguments are solely for compatability, `_n` must be equal to 5
let init_solar (_seed: u64) (_n: i64) : [6]pointmass =
  [{pos=vec (-290)     0  0, vel=vec    0  ( 122) 0, mass=mass_bound/25},  -- Big with orbit
   {pos=vec (-267)     0  0, vel=vec    0  ( 219) 0, mass=mass_bound/300}, -- orbit
   {pos=vec (  75)     0  0, vel=vec    0  ( 259) 0, mass=mass_bound/250},
   {pos=vec ( 375) (-375) 0, vel=vec (-69) ( -69) 0, mass=mass_bound/40},
   {pos=vec (   0) (-180) 0, vel=vec (160) (   0) 0, mass=mass_bound/60},
   {pos=v3.zero, vel=v3.zero, mass=mass_bound}]


let init_heavy_center
    (seed: u64)
    (n: i64)
    (init_func: u64 -> i64 -> [n]pointmass) : []pointmass =
  [{pos=v3.zero, vel=v3.zero, mass=mass_bound}] ++ init_func seed n


let init_fast (seed: u64) (n: i64) (init_func: u64 -> i64 -> []pointmass) : [n]pointmass =
  -- Create pointmasses
  let bodies = init_func seed n
  -- Create BHTree / Sort
  let sort = (\kf ks -> radix_sort_by_key kf u64.num_bits (u64.get_bit) ks)
  let (tree, _, _) = mk_BH_tree sort bodies
  in (tree.L :> [n]pointmass)
