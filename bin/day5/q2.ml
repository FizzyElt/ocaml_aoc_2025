open Lib

let merge_ranges (ranges : (int * int) list) =
    let reducer acc range =
        match acc with
        | [] -> [ range ]
        | prev :: tail -> begin
          let (low, high) = range in
          let (prev_low, prev_high) = prev in
          if low <= prev_high then
            (CCInt.min prev_low low, CCInt.max prev_high high) :: tail
          else
            range :: prev :: tail
        end
    in

    ranges
    |> CCList.sort (fun (low_a, _) (low_b, _) -> low_a - low_b)
    |> CCList.fold_left reducer []
;;

let total_fresh_id (ranges : (int * int) list) =
    merge_ranges ranges
    |> CCList.fold_left (fun acc (low, high) -> high - low + 1 + acc) 0
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
    let (id_ranges, _ids) = parse_file (String.trim file) in
    let total = total_fresh_id id_ranges in
    print_int total
;;
