import "types"

local def div_rounding_up x y : i64 = (x + y - 1) / y

def normalize (max: v3) (min: v3) (v: v3) =
  v3.map2 (/) ((v3.-) v min) ((v3.-) max min)


-- Creating Morton codes, taken from
-- https://github.com/athas/raytracingthenextweekinfuthark/blob/master/bvh.fut
-- | Expands a 10-bit integer into 30 bits by inserting 2 zeros after
-- each bit.
def expand_bits (v: u64) : u64 =
  let v = (v * 0x00010001) & 0xFF0000FF
  let v = (v * 0x00000101) & 0x0F00F00F
  let v = (v * 0x00000011) & 0xC30C30C3
  let v = (v * 0x00000005) & 0x49249249
  in v


def morton30bit {x, y, z} =
    let clamp10bit : f64 -> f64 =
      (*) 1024 >-> f64.max 0 >-> f64.min 1023
    let expand : f64 -> u64 =
      u64.f64 >-> (*) 0x00010001 >-> (&) 0xFF0000FF
              >-> (*) 0x00000101 >-> (&) 0x0F00F00F
              >-> (*) 0x00000011 >-> (&) 0xC30C30C3
              >-> (*) 0x00000005 >-> (&) 0x49249249
    let x' = (clamp10bit >-> expand) x
    let y' = (clamp10bit >-> expand) y
    let z' = (clamp10bit >-> expand) z
    in x' * 4 + y' * 2 + z'


-- Creating Radix tree taken from
-- https://github.com/athas/raytracingthenextweekinfuthark/blob/master/radixtree.fut
-- | `L` must be sorted.
def mk_radix_tree [n] (L: [n]u64) : []radix_inner =

  let delta (i, j) = if j >= 0 && j < n
                     then let Li = #[unsafe] L[i]
                          let Lj = #[unsafe] L[j]
                          -- Handle duplicates by using index as
                          -- tiebreaker if necessary.
                          in if Li == Lj
                             then 32 + u64.clz (u64.i64 i ^ u64.i64 j)
                             else u64.clz (Li ^ Lj)

                     else -1

  let node (i: i64) =
    -- Determine direction of range.
    let d = i64.sgn ( i64.i32 (delta(i,i+1i64) - delta(i,i-1i64)))

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
    let delta_node = i64.i32 ( delta(i, j) )
    let (s, _) = loop (s, q) = (0, 1)
                 while q <= l do
                 let t = l `div_rounding_up` (q*2)
                 in if i64.i32 (delta(i, i+(s+t)*d)) > delta_node
                    then (s+t, q*2)
                    else (s, q*2)
    let gamma = i + s*d + i64.min d 0

    -- Output child pointers
    let (left, set_left_parent) =
      if i64.min i j == gamma
      then (#leaf gamma, -1)
      else (#inner gamma, gamma)

    let (right, set_right_parent) =
      if i64.max i j == gamma + 1
      then (#leaf (gamma+1), -1)
      else (#inner (gamma+1), gamma+1)

    in ({left, right}, (set_left_parent, i), (set_right_parent, i), u8.i64 delta_node)

  let (inners, parents_a, parents_b, delta_nodes) = tabulate (n-1) node |> unzip4
  let k = (n-1)*2
  let parents = scatter (replicate (n-1) (-1))
                        (map (.0) parents_a ++ map (.0) parents_b :> [k]i64)
                        (map (.1) parents_a ++ map (.1) parents_b :> [k]i64)
  in map3 (\{left, right} parent delta_node -> {
    left, right, parent, delta=delta_node
  }) inners parents delta_nodes
