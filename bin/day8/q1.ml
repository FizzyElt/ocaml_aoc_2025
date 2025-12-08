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

let insert_junctions
      (groups : Junction.t list array)
      (connection : Junction.t * Junction.t)
  : Junction.t list array
  =
    let (node_a, node_b) = connection in

    let group_idx_a =
        groups |> CCArray.find_index (CCList.exists (Junction.equal node_a))
    in
    let group_idx_b =
        groups |> CCArray.find_index (CCList.exists (Junction.equal node_b))
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
      groups.(idx_a) <- node_b :: group_a;
      groups
    | (None, Some idx_b) ->
      let group_b = groups.(idx_b) in
      groups.(idx_b) <- node_a :: group_b;
      groups
    | (None, None) -> CCArray.append groups [| [ node_a; node_b ] |]
;;

let group_connections (connections : (Junction.t * Junction.t) list) =
    let rec loop
              (connections : (Junction.t * Junction.t) list)
              (groups : Junction.t list array)
      =
        match connections with
        | [] -> groups
        | connection :: tail ->
          let (junction_a, junction_b) = connection in
          let new_groups = insert_junctions groups (junction_a, junction_b) in
          loop tail new_groups
    in

    loop connections [||]
;;

let all_distances (junctions : Junction.t array)
  : (Junction.t * Junction.t) list
  =
    let len = CCArray.length junctions in
    let res = ref [] in
    for i = 0 to len - 2 do
      for j = i + 1 to len - 1 do
        let a = junctions.(i) in
        let b = junctions.(j) in
        let distance = Junction.distance a b in
        res := (a, b, distance) :: !res
      done
    done;
    !res
    |> CCList.sort (fun (_, _, dis_a) (_, _, dis_b) -> dis_a - dis_b)
    |> CCList.map (fun (junction_a, junction_b, _) -> (junction_a, junction_b))
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
    let take_count = 1000 in
    let sorted_distances = all_distances coords |> CCList.take take_count in

    let groups = group_connections sorted_distances |> CCArray.to_list in
    let res =
        groups
        |> CCList.map CCList.length
        |> CCList.sort (fun a b -> b - a)
        |> CCList.take 3
        |> CCList.fold_left ( * ) 1
    in

    print_int res;
    ()
;;
