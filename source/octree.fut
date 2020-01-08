import "radixtree"

let mk_octree [n] (max : v3) (min : v3) (ps : [n]v3) =
  let mortons = map (normalize max min >-> morton30bit) ps
  let rtree = mk_radix_tree mortons
  let ndiv3 parent child = let child = match child
                                       case #leaf _ -> parent -- ndiv3 = 0 in this case
                                       case #inner i -> rtree[i].delta
                             child / 3 - parent / 3
  let sizes = map (\{left, right, delta, parent} ->
                     u32.u8 (ndiv3 delta left +
                             ndiv3 delta right))
                  rtree
  let sizes_inc = scan (+) 0 sizes
  let sizes_exc =  (copy >-> rotate (-1) >-> \as -> scatter as [0] [0]) sizes_inc
  let parents = map (\i -> if 0 then -1 else sizes_exc[i - 1] ) iota n
  let is_leaf = map (\p -> ) parents
