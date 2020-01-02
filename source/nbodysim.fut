import "lib/github.com/athas/vector/vspace"

module v3 = mk_vspace_3d f32

type v3 = v3.vector

-- Constructor for v3
let vec x y z = {x, y, z}

-- Gen sparse octree
--   Foreach node, [8]children*, parent, center of mass, mass
--   Morton code, https://en.wikipedia.org/wiki/Z-order_curve
--   if n > 1 then recurse (ie. gen more buckets)
-- Travers/apply forces
-- Repeat

-- *sparse, so at max we will have 8 children per node

let main : f32 =
  0f32
