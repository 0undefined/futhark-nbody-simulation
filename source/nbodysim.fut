-- ==
-- entry: main
-- input { 5000i32 10i32 } auto output
import "lib/github.com/diku-dk/sorts/bubble_sort"
import "radixtree"
import "types"
import "BHtree"
import "init"


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


let acceleration (a: pointmass) (f: v3) : v3 = v3.scale (1/a.mass) f


let step_naive [n] (dt: real) (speed: f32) (os: [n]pointmass) : [n]pointmass =
  let forces = trace(map2 (\i o ->
    scatter (map (\p -> force o p) os) [i] [v3.zero] |> reduce_comm (v3.+) v3.zero
  ) (iota n) os)
  let accelerations = map2 acceleration os forces
  in map2 (advance_object_naive (speed*dt)) os accelerations


let step [n] (dt: real) (speed: f32) (os: [n]pointmass) : [n]pointmass =
  -- We can assume that the bodies are almost sorted, therefore use a sorting
  -- algorithm with best case on a (nearly|pre) sorted array
  let sort = (\kf ks -> bubble_sort_by_key kf (<=) ks)
  let (bh_tree, _, _) = mk_BH_tree sort os

  -- Traverse/apply forces
  let forces = map2 (\leaf idx ->
    let threshold = cool_threshold leaf.pos ((vx_bound_upper - vx_bound_lower) / 12)
    let op        = cool_op idx leaf
    in BH_fold threshold op v3.zero bh_tree
  ) bh_tree.L (iota n)

  -- Apply accelerations, and update velocity and position
  let accelerations = map2 acceleration bh_tree.L forces
  in map2 (advance_object_naive (speed*dt)) bh_tree.L accelerations


let main (n: i32) (steps: i32) : real =
  let bodies : [n]pointmass = init_fast 0 5 init_solar
  let (res, _) = loop (bodies, i) = (bodies, 0) while i < steps do
    (step 0.1 0.1 bodies, i + 1)
  in map (\r -> r.mass) res |> reduce (+) 0
