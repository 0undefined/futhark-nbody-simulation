-- Types and constants
import "lib/github.com/athas/vector/vspace"

type real = f32

module v3 = mk_vspace_3d f32

type v3 = v3.vector

-- Constructor for v3
let vec x y z = {x, y, z}

type pointmass = {
  pos: v3,
  vel: v3,
  mass: real
}

-- min/max values [x|y]
let vy_bound_lower : real = -600
let vx_bound_lower : real = -600
let vy_bound_upper : real =  600
let vx_bound_upper : real =  600

let mass_bound : real = 5000000
