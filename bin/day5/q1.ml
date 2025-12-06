open Lib
let check_fresh_id (ranges : (int * int) list) (id : int) =
    CCList.exists (fun (low, high) -> low <= id && id <= high) ranges
;;

let total_fresh_id (ranges : (int * int) list) (ids : int list) =
    let reducer acc id =
        if check_fresh_id ranges id then
          acc + 1
        else
          acc
    in
    CCList.fold_left reducer 0 ids
;;

let parse_file (s : string) =
    let parse_range_str (str : string) =
        Scanf.sscanf str "%d-%d" (fun low high -> (low, high))
    in

    match CCString.split ~by:"\n\n" s with
    | [ ranges; ids ] -> begin
      let id_ranges =
          CCString.split_on_char '\n' ranges |> CCList.map parse_range_str
      in
      let ids =
          CCString.split_on_char '\n' ids |> CCList.map CCInt.of_string_exn
      in
      (id_ranges, ids)
    end
    | _ -> failwith "valid input"
;;

let () =
    let file = File.read_file Sys.argv.(1) in
    let (id_ranges, ids) = parse_file (String.trim file) in
    let total = total_fresh_id id_ranges ids in
    print_int total
;;
