import "radixtree"

let mk_octree [n] (max: v3) (min: v3) (ps: [n]v3) =
  let mortons = map (normalize max min >-> morton30bit) ps
  let rtree = mk_radix_tree mortons
  let ndiv3 parent child = let child = match child
                                       case #leaf _ -> 32
                                       case #inner i -> rtree[i].delta
			   in child / 3 - parent / 3
  let sizes = map (\{left, right, delta, parent=_} ->
                     u32.u8 (ndiv3 delta left) +
		      u32.u8 (ndiv3 delta right))
                  rtree
  let sizes_inc = scan (+) 0 <| [0] ++ sizes
  --let idxArr = iota (last sizes_inc + 1u32)
  in sizes_inc
  -- let parents = map (\i -> if i==0 then -1 else sizes_exc[i - 1] ) <| iota n
  -- in parents
  -- let is_leaf = map (\p -> ) parents

  let main : [][]f32 =
  let liste = [[ 1.0,  1.0,  1.0],
               [ 2.0,  2.0,  2.0],
               [ 3.0,  3.0,  3.0],
               [ 4.0,  4.0,  4.0],
               [ 5.0,  5.0,  5.0],
               [ 6.0,  6.0,  6.0],
               [70.0, 70.0, 70.0]]
  let liste1 = map (\lst -> {x=lst[0], y=lst[1], z=lst[2]}) liste
  let mortonList = map (\lst -> let norm = normalize {x=1.0, y=1.0, z=1.0}
						     {x=70.0, y=70.0, z=70.0}
						     {x=lst[0], y=lst[1], z=lst[2]}
 				in morton30bit norm) liste
  let _ = trace(mk_radix_tree mortonList)
  let _ = trace(mk_octree {x=70.0, y=70.0, z=70.0} {x=1.0, y=1.0, z=1.0} liste1)
  --let _ = trace (scan (+) 0 (mk_octree {x=7.0, y=7.0, z=7.0} {x=1.0, y=1.0, z=1.0} liste1))
  in liste
