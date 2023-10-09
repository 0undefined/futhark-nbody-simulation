-- speed test BH, varying theta
-- ==
-- entry: random_test
-- input {1024i64 0.0f64 1i64 1f64 1f64}
-- input {1024i64 0.1f64 1i64 1f64 1f64}
-- input {1024i64 0.2f64 1i64 1f64 1f64}
-- input {1024i64 0.3f64 1i64 1f64 1f64}
-- input {1024i64 0.4f64 1i64 1f64 1f64}
-- input {1024i64 0.5f64 1i64 1f64 1f64}
-- input {1024i64 0.6f64 1i64 1f64 1f64}
-- input {1024i64 0.7f64 1i64 1f64 1f64}
-- input {1024i64 0.8f64 1i64 1f64 1f64}
-- input {1024i64 0.9f64 1i64 1f64 1f64}
-- input {1024i64 1.0f64 1i64 1f64 1f64}


-- speed test BH, varying number of bodies
-- ==
-- entry: random_test
-- input {10i64 0.5f64 1i64 1f64 1f64}
-- input {100i64 0.5f64 1i64 1f64 1f64}
-- input {1000i64 0.5f64 1i64 1f64 1f64}
-- input {10000i64 0.5f64 1i64 1f64 1f64}
-- input {100000i64 0.5f64 1i64 1f64 1f64}
-- input {1000000i64 0.5f64 1i64 1f64 1f64}
-- input {10000000i64 0.5f64 1i64 1f64 1f64}


-- speed test naive, varying number of bodies
-- ==
-- entry: random_test_naive
-- input {10i64 0.5f64 1i64 1f64 1f64}
-- input {100i64 0.5f64 1i64 1f64 1f64}
-- input {1000i64 0.5f64 1i64 1f64 1f64}
-- input {10000i64 0.5f64 1i64 1f64 1f64}
-- input {100000i64 0.5f64 1i64 1f64 1f64}
-- input {1000000i64 0.5f64 1i64 1f64 1f64}
-- input {10000000i64 0.5f64 1i64 1f64 1f64}

import "lib/github.com/diku-dk/sorts/radix_sort"
import "radixtree"
import "types"
import "BHtree"
import "init"


let advance_object (dt: real) (o: pointmass) (a: v3) : pointmass =
  let vel = v3.(o.vel + scale dt a)
  let pos = v3.(o.pos + scale dt vel)
  in {pos, vel, mass=o.mass}


let force (a: pointmass) (b: pointmass) : v3 =
  let G     = 1f64 -- 6.674e-11
  let r     = v3.(b.pos - a.pos)
  let invr  = 1.0f64 / (v3.norm r + G) -- f64.sqrt rsqr
  in v3.scale (b.mass * invr * invr * invr) r


let step_naive [n] (dt: real) (speed: f64) (os: [n]pointmass) : [n]pointmass =
  let apply_forces (o: pointmass) = map (force o) os |> reduce_comm (v3.+) v3.zero
  --let accelerations = map2 acceleration os forces
  let forces = map apply_forces os
  in map2 (advance_object (speed*dt)) os forces


let step [n] (theta: f64) (dt: real) (speed: f64) (os: [n]pointmass) : [n]pointmass =
  -- We can assume that the bodies are almost sorted, therefore use a sorting
  -- algorithm with best case on a (nearly|pre) sorted array
  let sort = (\kf ks -> radix_sort_by_key kf u64.num_bits (u64.get_bit) ks)
  let (bh_tree, min, max) = mk_BH_tree sort os

  -- Traverse/apply forces
  let forces = map2 (\leaf idx ->
    let threshold = threshold_denormalized min max leaf.pos theta
    let op        = cool_op idx leaf
    in BH_fold threshold op v3.zero bh_tree
  ) bh_tree.L (iota n) |> trace

  -- Apply accelerations, and update velocity and position
  --let accelerations = map2 acceleration bh_tree.L forces
  in map2 (advance_object (speed*dt)) bh_tree.L forces


let wrap_body (p: (real, real, real)) (v: (real, real, real)) (mass: real) : pointmass =
  {pos=vec p.0 p.1 p.2, vel=vec v.0 v.1 v.2, mass}


let unwrap_body (p : pointmass) =
  ((p.pos.x, p.pos.y, p.pos.z), (p.vel.x, p.vel.y, p.vel.z), p.mass)


entry main [n]
      (theta:     f64)
      (steps:     i64)
      (dt:        f64)
      (speed:     f64)
      (xps:    [n]f64)
      (yps:    [n]f64)
      (zps:    [n]f64)
      (xvs:    [n]f64)
      (yvs:    [n]f64)
      (zvs:    [n]f64)
      (masses: [n]f64) : ([n]f64, [n]f64, [n]f64, [n]f64, [n]f64, [n]f64, [n]f64) =

  let bodies : [n]pointmass = map3 wrap_body (zip3 xps yps zps) (zip3 xvs yvs zvs) masses

  let res = loop bodies for _i < steps do step theta dt speed bodies

  let (final_pos, final_vel, final_mass) = map unwrap_body (res) |> unzip3

  let (xps', yps', zps') = unzip3 final_pos
  let (xvs', yvs', zvs') = unzip3 final_vel
  in (xps', yps', zps', xvs', yvs', zvs', final_mass)


entry random_test (n: i64)
                (theta: f64)
                (steps: i64)
                (dt: f64)
                (speed: f64) =
  let bodies = init_rand 0 n
  in loop bodies for _i < steps do step theta dt speed bodies

entry random_test_naive (n: i64)
                (theta: f64)
                (steps: i64)
                (dt: f64)
                (speed: f64) =
  let bodies = init_rand 0 n
  in loop bodies for _i < steps do step_naive dt speed bodies
