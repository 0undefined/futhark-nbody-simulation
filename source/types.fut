-- Types and constants
import "lib/github.com/athas/vector/vspace"
module real = f32
module v3 = mk_vspace_3d real
type v3 = v3.vector
type real = v3.real

-- Constructor for v3
let vec x y z = {x, y, z}

type pointmass = {
  pos: v3,
  vel: v3,
  mass: real
}

type ptr = #leaf i32 | #inner i32

type octNode = {isLeaf: bool, level: u8, parentIdx: u32, morton: u32,
		childIdx: []u32}

-- min/max values [x|y]
let vy_bound_lower : real = -600
let vx_bound_lower : real = -600
let vy_bound_upper : real =  600
let vx_bound_upper : real =  600

let mass_bound : real = 5000000

let initial_vel = 9f32
