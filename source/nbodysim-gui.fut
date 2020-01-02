import "nbodysim"
import "lib/github.com/diku-dk/lys/lys"

type text_content = (i32)

module lys: lys with text_content = text_content = {
  type state = {
    objects: []pointmass,
    height: i32,
    width: i32
  }

  let init (seed: i32) (height: i32) (width: i32) : state =
    {objects = init seed 12, height, width}

  let resize (height: i32) (width: i32) (s: state) =
    s with height = height
      with width  = width
      with objects = s.objects

  let keydown (k: i32) (s: state) =
    s

  let grab_mouse = false
  let mouse _ _ _ s = s
  let wheel _ s = s

  let step (s: state) =
    s with objects = step s.objects

  let event (e: event) (s: state) =
    match e
    case #step _ -> step s
    case #keydown {key} -> keydown key s
    case _ -> s

  let render (s: state) =
    let backdrop = argb.black
    let (is, cs) = unzip (map (\o -> drawpoint o.pos.x o.pos.y o.pos.z o.mass s.height s.width) s.objects)

    in unflatten s.height s.width (scatter (replicate (s.height * s.width) backdrop) is cs)

  type text_content = text_content
  let text_format   = "FPS: %d"
  let text_colour _ = argb.white
  let text_content fps (s: state) = (t32 fps)
}
