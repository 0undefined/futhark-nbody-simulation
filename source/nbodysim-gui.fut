import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"
import "init"     -- Initialization functions
import "nbodysim" -- Simulation functions

module v2 = mk_vspace_2d f32
type   v2 = v2.vector

type text_content = (
  i32,f32,
  f32,f32,f32,f32,f32, -- vel, pos{x,y,z}, mass
  f32,f32,f32,f32,f32,
  f32,f32,f32,f32,f32,
  f32,f32,f32,f32,f32,
  f32,f32,f32,f32,f32,
  f32,f32,f32,f32,f32,
  i32)

let step_fun = step 0.2


-- return flat index + colour of point
let drawpoint (x: f32) (y: f32) (z: f32) (w: f32) (height: i32) (width: i32) : (i32, u32) =
  -- ignore if out of window bounds
  if   x < vx_bound_lower
    || x > vx_bound_upper
    || y < vy_bound_lower
    || y > vy_bound_upper then
    (-1, 0)
  else
    let pos = {x, y}
    -- normalize, [0;1]
    let pos_norm = (v2.-) pos      {x=vx_bound_lower, y=vy_bound_lower}
    let pos_norm = (v2./) pos_norm {x=vx_bound_upper - vx_bound_lower,
                                    y=vy_bound_upper - vy_bound_lower}
    -- project back to height/width
    let pos' = (v2.*) pos_norm {x=f32.i32 width, y=f32.i32 height}
    -- colour is determined by mass + z-position
    -- red: one thicc boy
    -- bright: one close boy
    let colour_weight = 0.2 + 0.8 * (w / mass_bound)
    let colour_z      = 0.2 + 0.8 * f32.sqrt (z**2 / (vy_bound_upper**2 + vy_bound_lower**2))
    let colour        = argb.mix colour_weight argb.red colour_z argb.white

    in ((i32.f32 pos'.y) * width + (i32.f32 pos'.x), colour)


let render [n] (os: [n]pointmass) (height: i32) (width: i32) : [height][width]u32 =
  let backdrop = argb.black
  let (is, cs) = unzip (map (\o -> drawpoint o.pos.x o.pos.y o.pos.z o.mass height width) os)

  in unflatten height width (scatter (replicate (height * width) backdrop) is cs)


module lys: lys with text_content = text_content = {
  type~ state = {
    objects: []pointmass,
    speed: f32,
    height: i32,
    width: i32,
    paused: bool
  }


  let init (seed: u32) (height: i32) (width: i32) : state =
    {objects = init_solar seed 6, speed = 1f32, height, width, paused = true}


  let resize (height: i32) (width: i32) (s: state) =
    s with height  = height
      with width   = width
      with objects = s.objects


  let keydown (k: i32) (s: state) =
    if      k == SDLK_SPACE then s with paused  = !s.paused
    else if k == SDLK_n     then s with objects = step_fun 0.1 s.speed s.objects
    else if k == SDLK_j     then s with speed = s.speed - 0.05
    else if k == SDLK_k     then s with speed = s.speed + 0.05
    else s


  let grab_mouse = false
  let mouse _ _ _ s = s
  let wheel _ s = s


  let event (e: event) (s: state) =
    match e
    case #step dt -> s with objects = if s.paused then s.objects else step_fun dt s.speed s.objects
    case #keydown {key} -> keydown key s
    case _ -> s


  let render (s: state) =
    let backdrop = argb.black
    let (is, cs) = unzip (map (\o -> drawpoint o.pos.x o.pos.y o.pos.z o.mass s.height s.width) s.objects)

    in unflatten s.height s.width (scatter (replicate (s.height * s.width) backdrop) is cs)

  type text_content = text_content
  let text_format   = \() -> "FPS: %d\nspeed: %.2f
  v: %.2f x: %.2f y: %.2f z: %.2f m: %.2f
  v: %.2f x: %.2f y: %.2f z: %.2f m: %.2f
  v: %.2f x: %.2f y: %.2f z: %.2f m: %.2f
  v: %.2f x: %.2f y: %.2f z: %2.f m: %.2f
  v: %.2f x: %.2f y: %.2f z: %2.f m: %.2f
  v: %.2f x: %.2f y: %.2f z: %2.f m: %.2f\n%[\nPaused|]"
  let text_colour _ = argb.white


  let text_content fps (s: state) = (
    t32 fps, s.speed,
    v3.norm s.objects[0].vel, s.objects[0].pos.x, s.objects[0].pos.y, s.objects[0].pos.z, s.objects[0].mass,
    v3.norm s.objects[1].vel, s.objects[1].pos.x, s.objects[1].pos.y, s.objects[1].pos.z, s.objects[1].mass,
    v3.norm s.objects[2].vel, s.objects[2].pos.x, s.objects[2].pos.y, s.objects[2].pos.z, s.objects[2].mass,
    v3.norm s.objects[3].vel, s.objects[3].pos.x, s.objects[3].pos.y, s.objects[3].pos.z, s.objects[3].mass,
    v3.norm s.objects[4].vel, s.objects[4].pos.x, s.objects[4].pos.y, s.objects[4].pos.z, s.objects[4].mass,
    v3.norm s.objects[5].vel, s.objects[5].pos.x, s.objects[5].pos.y, s.objects[5].pos.z, s.objects[5].mass,
    if s.paused then 0i32 else 1i32)
}
