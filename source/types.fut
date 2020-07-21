-- Types and constants
import "lib/github.com/athas/vector/vspace"

module real = f32
module v3   = mk_vspace_3d real

type v3   = v3.vector
type real = v3.real

type pointmass = {pos: v3, vel: v3, mass: real}

type ptr = #leaf i32 | #inner i32

type radix_inner = {left: ptr, right: ptr, parent: i32, delta: u8}

type bh_inner  = {pos: v3, mass: real, left: ptr, right: ptr, parent: i32, delta: u8}
type~ bh [n] = {L: [n]pointmass, I: []bh_inner}

-- Constructor for v3
let vec x y z = {x, y, z}

-- min/max values [x|y]
let vy_bound_lower : real = -600
let vx_bound_lower : real = -600
let vy_bound_upper : real =  600
let vx_bound_upper : real =  600

let mass_bound : real = 5000000

let initial_vel = 9f32
