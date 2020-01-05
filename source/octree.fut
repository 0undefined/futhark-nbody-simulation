import "radixtree"

let mk_octree (max : []v3) (min : []v3) (ps : []v3) =
  let mortons = map (normalize max min >-> morton30bit) ps
  let rtree = mk_radix_tree mortons
  let ndiv3 parent child = child / 3 - parent / 3
  let sizes = map (\{left, right, delta, parent} ->
                     {parent,
                      size = u32.u8 (ndiv3 delta rtree[left].delta +
                                     ndiv3 delta tree[right].delta)}) rtree
  let 
