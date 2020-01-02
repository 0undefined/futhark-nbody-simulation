import "lib/github.com/athas/vector/vspace"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/athas/matte/colour"

module v3 = mk_vspace_3d f32

type v3 = v3.vector

--let rng = minstd_rand.rng_from_seed [0]
--module r = uniform_float_distribution f32 minstd_rand


-- Constructor for v3
let vec x y z = {x, y, z}

type pointmass = {
  pos: v3,
  vel: v3,
  mass: f32
}


let render (objects: []pointmass) (h: i32) (w: i32): [h][w]argb.colour =
  tabulate_2d h w (\_ -> argb.black)


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
