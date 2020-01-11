import "types"
-- Creating Morton codes, taken from
-- https://github.com/athas/raytracingthenextweekinfuthark/blob/master/bvh.fut
-- | Expands a 10-bit integer into 30 bits by inserting 2 zeros after
-- each bit.
let expand_bits (v: u32) : u32 =
  let v = (v * 0x00010001) & 0xFF0000FF
  let v = (v * 0x00000101) & 0x0F00F00F
  let v = (v * 0x00000011) & 0xC30C30C3
  let v = (v * 0x00000005) & 0x49249249
  in v

-- let morton_3D {x,y,z} : u32 =
--   let x = f32.min (f32.max(x * 1024) 0) 1023
--   let y = f32.min (f32.max(y * 1024) 0) 1023
--   let z = f32.min (f32.max(z * 1024) 0) 1023
--   let xx = expand_bits (u32.f32 x)
--   let yy = expand_bits (u32.f32 y)
--   let zz = expand_bits (u32.f32 z)
--   in xx * 4 + yy * 2 + zz

let morton30bit {x, y, z} =
    let clamp10bit : f32 -> f32 =
      (*) 1024 >-> f32.max 0 >-> f32.min 1023
    let expand : f32 -> u32 =
      u32.f32 >-> (*) 0x00010001 >-> (&) 0xFF0000FF
              >-> (*) 0x00000101 >-> (&) 0x0F00F00F
              >-> (*) 0x00000011 >-> (&) 0xC30C30C3
              >-> (*) 0x00000005 >-> (&) 0x49249249
    let x' = (clamp10bit >-> expand) x
    let y' = (clamp10bit >-> expand) y
    let z' = (clamp10bit >-> expand) z
    in x' * 4 + y' * 2 + z'

let normalize (max : v3) (min : v3) (v : v3) =
  v3.map2 (/) ((v3.-) v min) ((v3.-) max min)

-- let normalize {x,y,z} = {x=(x-x_min)/(x_max-x_min),
--                            y=(y-y_min)/(y_max-y_min),
--                            z=(z-z_min)/(z_max-z_min)}

local let div_rounding_up x y : i32 = (x + y - 1) / y

type ptr = #leaf i32 | #inner i32

type inner = {left:ptr, right:ptr, parent: i32, delta: u8}

-- Creating Radix tree taken from
-- https://github.com/athas/raytracingthenextweekinfuthark/blob/master/radixtree.fut
-- | `L` must be sorted.
let mk_radix_tree [n] (L: [n]u32) : []inner =

  let delta (i, j) = if j >= 0 && j < n
                     then let Li = unsafe L[i]
                          let Lj = unsafe L[j]
                          -- Handle duplicates by using index as
                          -- tiebreaker if necessary.
                          in if Li == Lj
                             then 32 + u32.clz (u32.i32 i ^ u32.i32 j)
                             else u32.clz (Li ^ Lj)
                     else -1

  let node (i: i32) =
    -- Determine direction of range.
    let d = i32.sgn (delta(i,i+1) - delta(i,i-1))

    -- Compute upper bound for the length of the range.
    let delta_min = delta(i,i-d)
    let l_max = loop l_max = 2
                while delta(i, i+l_max*d) > delta_min do
                  l_max * 2

    -- Find the other end using binary search.
    let (l, _) = loop (l, t) = (0, l_max/2)
                 while t > 0 do
                   if delta(i, i+(l+t)*d) > delta_min
                   then (l + t, t/2)
                   else (l, t/2)
    let j = i + l * d

    -- Find the split position using binary search.
    let delta_node = delta(i, j)
    let (s, _) = loop (s, q) = (0, 1)
                 while q <= l do
                 let t = l `div_rounding_up` (q*2)
                 in if delta(i, i+(s+t)*d) > delta_node
                    then (s+t, q*2)
                    else (s, q*2)
    let gamma = i + s*d + i32.min d 0

    -- Output child pointers
    let (left, set_left_parent) =
      if i32.min i j == gamma
      then (#leaf gamma, -1)
      else (#inner gamma, gamma)

    let (right, set_right_parent) =
      if i32.max i j == gamma + 1
      then (#leaf (gamma+1), -1)
      else (#inner (gamma+1), gamma+1)

    in ({left, right}, (set_left_parent, i), (set_right_parent, i), u8.i32 delta_node)

  let (inners, parents_a, parents_b, delta_nodes) = tabulate (n-1) node |> unzip4
  let k = (n-1)*2
  let parents = scatter (replicate (n-1) (-1))
                        (map (.1) parents_a ++ map (.1) parents_b : [k]i32)
                        (map (.2) parents_a ++ map (.2) parents_b : [k]i32)

  in map3 (\{left, right} parent delta_node -> {left, right, parent, delta=delta_node}) inners parents delta_nodes

--let liste = [[0.1,0.1,0.1],[0.2,0.2,0.2],[0.3,0.3,0.3],[0.4,0.4,0.4],[0.5,0.5,0.5]]
-- let main =
--   let liste = [[1.0,1.0,1.0],[2.0,2.0,2.0],[3.0,3.0,3.0],[4.0,4.0,4.0],[5.0,5.0,5.0]]
--   let mortonList = map (\lst -> let norm = normalize {x=1.0,y=1.0,z=1.0} {x=5.0,y=5.0,z=5.0} {x=lst[0],y=lst[1],z=lst[2]}
-- 				in morton30bit norm) liste
--   let _ = map (\i -> trace(i)) mortonList
--   let x = trace(mk_radix_tree mortonList)
--   in x
