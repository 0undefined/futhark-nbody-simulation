import "BHtree"
import "nbodysim"
import "lib/github.com/diku-dk/sorts/radix_sort"



let RMSE [n] (vs : [n]v3) (us : [n]v3) : real =
  let mse = curry (uncurry (v3.-) >-> (\v -> (v3.*) v v) >-> (\{x, y, z} -> x + y + z))
  in map2 mse vs us |> (reduce_comm (real.+) 0 >-> (/(n*3 |> real.i32)) >-> real.sqrt)

let simulate [n] (pms : [n]pointmass) theta : real =

  let sort = (\kf ks -> radix_sort_by_key kf u32.num_bits (u32.get_bit) ks)
  let (bh_tree, min, max) = mk_BH_tree sort pms
  let forces = map2 (\leaf idx ->
                       let threshold = threshold_denormalized min max leaf.pos theta
                       let op        = cool_op idx leaf
                       in BH_fold threshold op v3.zero bh_tree
                    ) bh_tree.L (iota n)

  let apply_forces (o: pointmass) = map (force o) bh_tree.L |> reduce_comm (v3.+) v3.zero
  let forces' = map apply_forces bh_tree.L
  in RMSE forces forces'




let find_anyNaN (t : bh) : bh =
  let NaNvec {x, y, z} : bool =
    real.isnan x && real.isnan x real.isnan y && real.isnan z
  let NaNtree {L, I} : bool =
    let NaNL pms : bool =
      let NaNpm {pos, vel, mass} : bool =
        NaNvec pos && NaNvec vel && real.isnan mass
      in map NaNpm pms
    let NaNI inner =
      let NaNi {pos, mass, left=_, right=_, delta=_} =
        NaNvec pos && real.isnan
      in map NaNi inner
    in NaNL L && NaNI I
  in NaNtree t

entry configBeforeTommorow pms =
  let sort = (\kf ks -> radix_sort_by_key kf u32.num_bits (u32.get_bit) ks)
  in (.2) <| loop (cur, prev) = (mk_BH_tree sort pms, mk_BH_tree sort pms) while !(find_anyNaN cur) do
             let prev = cur
             let forces = map2 (\leaf idx ->
                               let threshold = threshold_denormalized min max leaf.pos 0.5
                               let op        = cool_op idx leaf
                               in BH_fold threshold op v3.zero cur
                               ) cur.L (iota n)
             let 


let main [n]
  (xps:    [n]f32)
  (yps:    [n]f32)
  (zps:    [n]f32)
  (xvs:    [n]f32)
  (yvs:    [n]f32)
  (zvs:    [n]f32)
  (masses: [n]f32) =
  let bodies : []pointmass = map3 wrap_body (zip3 xps yps zps) (zip3 xvs yvs zvs) masses
  in simulate bodies 0.5