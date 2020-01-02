import "lib/github.com/athas/vector/vspace"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/athas/matte/colour"

module v3 = mk_vspace_3d f32

type v3 = v3.vector

--let rng = minstd_rand.rng_from_seed [0]
--module r = uniform_float_distribution f32 minstd_rand

-- Creating Morton codes, taken from
-- https://github.com/athas/raytracingthenextweekinfuthark/blob/master/bvh.fut
-- | Expands a 10-bit integer into 30 bits by inserting 2 zeros after
-- each bit.
-- let expand_bits (v: u32) : u32 =
--   let v = (v * 0x00010001) & 0xFF0000FF
--   let v = (v * 0x00000101) & 0x0F00F00F
--   let v = (v * 0x00000011) & 0xC30C30C3
--   let v = (v * 0x00000005) & 0x49249249
--   in v
-- let morton_3D {x,y,z} : u32 =
--   let x = f32.min (f32.max(x * 1024) 0) 1023
--   let y = f32.min (f32.max(y * 1024) 0) 1023
--   let z = f32.min (f32.max(z * 1024) 0) 1023
--   let xx = expand_bits (u32.f32 x)
--   let yy = expand_bits (u32.f32 y)
--   let zz = expand_bits (u32.f32 z)
--   in xx * 4 + yy * 2 + zz


-- Constructor for v3
let vec x y z = {x, y, z}

type pointmass = {
  pos: v3,
  vel: v3,
  mass: f32
}

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
