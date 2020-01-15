import "BHtree"
import "lib/github.com/diku-dk/sorts/radix_sort.fut"


let vec x y z : v3 = {x, y, z}

let pm pos = {pos, vel = v3.zero, mass = 0}

let pms = {pm (vec 0 0 0), pm (vec 1 1 1)}

local sort = (\kf ks -> radix_sort_by_key )

let t pms = mk_BH_tree sort pms

let full_fold t =
  let thres = (\a b -> true)
  in map (\p i ->
             let op = cool_op i p
             in BH_fold thres op v3.zero t) t.L (iota (length t.L))
