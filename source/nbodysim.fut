-- ==
-- entry: main
-- input @ data/1024-bodies-bin.in
-- input @ data/2048-bodies-bin.in
-- input @ data/4096-bodies-bin.in
-- input @ data/8192-bodies-bin.in
-- input @ data/16384-bodies-bin.in
-- input @ data/32768-bodies-bin.in
-- input @ data/65536-bodies-bin.in
-- input @ data/131072-bodies-bin.in
-- input @ data/262144-bodies-bin.in
-- input @ data/524288-bodies-bin.in
-- input @ data/1048576-bodies-bin.in
-- input @ data/2097152-bodies-bin.in
-- input @ data/4194304-bodies-bin.in
-- input @ data/8388608-bodies-bin.in
-- input @ data/16777216-bodies-bin.in
-- input @ data/33554432-bodies-bin.in
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
  let G     = 1f32 -- 6.674e-11
  let r     = v3.(b.pos - a.pos)
  let invr  = 1.0f32 / (v3.norm r + G) -- f32.sqrt rsqr
  in v3.scale (b.mass * invr * invr * invr) r


let step_naive [n] (dt: real) (speed: f32) (os: [n]pointmass) : [n]pointmass =
  let apply_forces (o: pointmass) = map (force o) os |> reduce_comm (v3.+) v3.zero
  --let accelerations = map2 acceleration os forces
  let forces = map apply_forces os
  in map2 (advance_object (dt)) os forces


let step [n] (dt: real) (speed: f32) (os: [n]pointmass) : [n]pointmass =
  -- We can assume that the bodies are almost sorted, therefore use a sorting
  -- algorithm with best case on a (nearly|pre) sorted array
  --let sort = (\kf ks -> bubble_sort_by_key kf (<) ks)
  let sort = (\kf ks -> radix_sort_by_key kf u32.num_bits (u32.get_bit) ks)
  let (bh_tree, min, max) = mk_BH_tree sort os

  -- Traverse/apply forces
  let forces = map2 (\leaf idx ->
    let threshold = threshold_denormalized min max leaf.pos 0.5
    let op        = cool_op idx leaf
    in BH_fold threshold op v3.zero bh_tree
  ) bh_tree.L (iota n)
  --let forces = replicate n v3.zero

  -- Apply accelerations, and update velocity and position
  --let accelerations = map2 acceleration bh_tree.L forces
  in map2 (advance_object (speed*dt)) bh_tree.L forces


let wrap_body (p: (real, real, real)) (v: (real, real, real)) (mass: real) : pointmass =
  {pos=vec p.1 p.2 p.3, vel=vec v.1 v.2 v.3, mass}


let unwrap_body (p : pointmass) =
  ((p.pos.x, p.pos.y, p.pos.z), (p.vel.x, p.vel.y, p.vel.z), p.mass)


let main [n]
      (steps:     i32)
      (dt:        f32)
      (epsilon:   f32)
      (xps:    [n]f32)
      (yps:    [n]f32)
      (zps:    [n]f32)
      (xvs:    [n]f32)
      (yvs:    [n]f32)
      (zvs:    [n]f32)
      (masses: [n]f32) : ([n]f32, [n]f32, [n]f32, [n]f32, [n]f32, [n]f32, [n]f32) =
  let bodies : [n]pointmass = map3 wrap_body (zip3 xps yps zps) (zip3 xvs yvs zvs) masses
  --init_circle 0 n
  let res = loop bodies for _i < steps do step_naive dt epsilon bodies
  let (final_pos, final_vel, final_mass) = map unwrap_body (res) |> unzip3
  let (xps', yps', zps') = unzip3 final_pos
  let (xvs', yvs', zvs') = unzip3 final_vel
  in (xps', yps', zps', xvs', yvs', zvs', final_mass)
