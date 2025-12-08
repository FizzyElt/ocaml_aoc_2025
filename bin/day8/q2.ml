open Lib

module Junction = struct
  type t = int * int * int

  let make x y z : t = (x, y, z)

  let distance (a : t) (b : t) =
      let (a_x, a_y, a_z) = a in
      let (b_x, b_y, b_z) = b in
      let diff_x = a_x - b_x in
      let diff_y = a_y - b_y in
      let diff_z = a_z - b_z in
      (diff_x * diff_x) + (diff_y * diff_y) + (diff_z * diff_z)
  ;;
  let equal (a : t) (b : t) =
      let (a_x, a_y, a_z) = a in
      let (b_x, b_y, b_z) = b in
      a_x = b_x && a_y = b_y && a_z = b_z
  ;;
end

let insert_junction
      (groups : Junction.t list array)
      (pair : Junction.t * Junction.t)
  : Junction.t list array
  =
    let (a, b) = pair in
    let group_idx_a =
        groups |> CCArray.find_index (CCList.exists (Junction.equal a))
    in
    let group_idx_b =
        groups |> CCArray.find_index (CCList.exists (Junction.equal b))
    in
    match (group_idx_a, group_idx_b) with
    (* in same group *)
    | (Some idx_a, Some idx_b) when idx_a = idx_b -> groups
    (* in different group *)
    | (Some idx_a, Some idx_b) ->
      let group_a = groups.(idx_a) in
      let group_b = groups.(idx_b) in
      groups.(idx_a) <- group_a @ group_b |> CCList.uniq ~eq:Junction.equal;
      groups.(idx_b) <- [];
      CCArray.filter
        (function
          | [] -> false
          | _ -> true )
        groups
    | (Some idx_a, None) ->
      let group_a = groups.(idx_a) in
      groups.(idx_a) <- b :: group_a;
      groups
    | (None, Some idx_b) ->
      let group_b = groups.(idx_b) in
      groups.(idx_b) <- a :: group_b;
      groups
    | (None, None) -> CCArray.append groups [| [ a; b ] |]
;;

let group_junctions (pairs : (Junction.t * Junction.t * int) list) (total : int)
  =
    let rec loop pairs (groups : Junction.t list array) =
        match pairs with
        | [] -> 0
        | pair :: rest ->
          let (junction_a, junction_b, _) = pair in
          let new_groups = insert_junction groups (junction_a, junction_b) in
          if
            CCArray.length new_groups = 1
            && CCList.length new_groups.(0) = total
          then (
            let (a_x, _, _) = junction_a in
            let (b_x, _, _) = junction_b in
            a_x * b_x
          ) else
            loop rest new_groups
    in

    loop pairs [||]
;;

let all_distances (arr : Junction.t array)
  : (Junction.t * Junction.t * int) list
  =
    let len = CCArray.length arr in
    let res = ref [] in
    for i = 0 to len - 2 do
      for j = i + 1 to len - 1 do
        let a = arr.(i) in
        let b = arr.(j) in
        let distance = Junction.distance a b in
        res := (a, b, distance) :: !res
      done
    done;
    CCList.sort (fun (_, _, dis_a) (_, _, dis_b) -> dis_a - dis_b) !res
;;

let parse_line s =
    let coords =
        CCString.split_on_char ',' s |> CCList.map CCInt.of_string_exn
    in
    match coords with
    | [ x; y; z ] -> Junction.make x y z
    | _ -> failwith "invalid coords"
;;

let () =
    let coords =
        File.read_list_of_line parse_line Sys.argv.(1) |> CCArray.of_list
    in
    let sorted_distances = all_distances coords in

    let res = group_junctions sorted_distances (CCArray.length coords) in

    print_int res;
    ()
;;
