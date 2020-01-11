import "radixtree"
import "types"

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

let step_naive [n] (dt: real) (_: real) (os: [n]pointmass) : [n]pointmass =
  let forces = map2 (\i o ->
    scatter (map (\p -> force o p) os) [i] [v3.zero] |> reduce_comm (v3.+) v3.zero
  ) (iota n) os
  let accelerations = map2 acceleration os forces
  in map2 (advance_object_naive dt) os accelerations

let step [n] (dt: real) (_: real) (os: [n]pointmass) : [n]pointmass =
  -- Gen sparse octree
  --   Foreach node, [8]children*, parent, center of mass, mass
  --   Morton code, https://en.wikipedia.org/wiki/Z-order_curve
  --   if n > 1 then recurse (ie. gen more buckets)
  -- Travers/apply forces
  -- *sparse, so at max we will have 8 children per node
  os

let main : real =
  0
