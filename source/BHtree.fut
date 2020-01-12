-- A Barns-Hut implementation
import "types"
import "radixtree"

let highest = {x=real.highest, y=real.highest, z=real.highest}
let lowest  = {x=real.lowest,  y=real.lowest,  z=real.lowest}


let mk_BH_tree [n]
    (sort_by_key: (pointmass -> u32) ->  []pointmass -> []pointmass)
    (pms: [n]pointmass) =
  let centers   = map (.pos) pms
  let min       = reduce_comm (v3.map2 real.min) highest centers
  let max       = reduce_comm (v3.map2 real.max) lowest  centers
  let normalize = (v3.-min) >-> (v3./(max v3.-min))

  let morton    = (.pos) >-> normalize >-> morton30bit
  let pms       = sort_by_key morton pms

  let empty_inner {left, right, parent, delta} = {pos=v3.zero, mass=0, left, right, parent, delta}
  let inners    = map empty_inner (mk_radix_tree (map morton pms))
  let get_pointmass inners ptr =
    match ptr
    case #leaf  i -> unsafe (pms[i].pos,    pms[i].mass)
    case #inner i -> unsafe (inners[i].pos, inners[i].mass)

  let update inners {pos=_, mass=_, left, right, parent, delta} =
    let avgpos p1 m1 p2 m2 =
      v3.map (/m1+m2) ((v3.scale m1 p1) v3.+ (v3.scale m2 p2))
    let (pl, ml) = get_pointmass inners left
    let (pr, mr) = get_pointmass inners right
    in {pos=avgpos pl ml pr mr, mass=ml + mr,
        left, right, parent, delta}

  let depth     = t32 (f32.log2 (r32 n)) + 2
  let inners = loop inners for _i < depth do
                 map (update inners) inners
  in ({L = pms, I = inners}, min, max)


let BH_fold [n] 'a 'b
    (threshold: u8 -> v3 -> bool)
    (op: b -> i32 -> pointmass -> b)
    (init: b)
    (t: bh [n]) =
  (.1) <|
  loop (acc, cur, prev) = (init, 0, #inner (-1))
  while cur != -1 do
  let node       = unsafe t.I[cur]
  let from_left  = prev == node.left
  let from_right = prev == node.right
  let rec_child : #rec ptr | #norec =
    -- Did we return from left node?
    if from_left
    then #rec node.right
    -- First encounter and in this BB?
    else if !from_right && threshold node.delta node.pos
    then #rec node.left
    else #norec
  in match rec_child
    case #norec ->
      let inner = unsafe t.I[cur]
      let pointmass = {pos=inner.pos, mass=inner.mass, vel=v3.zero}
      in (op acc cur pointmass, node.parent, #inner cur)
    case #rec ptr -> match ptr
      case #inner i -> (acc, i, #inner cur)
      case #leaf i  -> (op acc i (unsafe t.L[i]), cur, ptr)


let force (a: pointmass) (b: pointmass) : v3 =
  let G'       = 1f32 -- -6.674e-11
  let r        = v3.(b.pos - a.pos)
  let inv_dist = 1 / v3.norm r -- f32.max 1f32 (v3.norm r)
  let r'       = v3.scale inv_dist r
  in v3.scale (G' * a.mass * b.mass * inv_dist**2) r'


let cool_op (self_idx : i32) (self : pointmass) (accumulated_F : v3) (i : i32) (other : pointmass) : v3 =
  if self_idx == i then accumulated_F else (v3.+) accumulated_F (force self other)


let cool_threshold (self_pos : v3) (theta : real) (delta : u8) (other_pos : v3) : bool =
  (f32.u8 delta) / v3.(norm (self_pos - other_pos)) < theta
