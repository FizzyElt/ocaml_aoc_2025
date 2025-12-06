open Lib

let next_point p diff =
    let next_p = p + diff in
    let next_p_mod_100 = next_p mod 100 in
    let times =
        if diff >= 0 then
          next_p / 100
        else if p = 0 then
          diff / -100
        else if -diff >= p then
          (next_p / -100) + 1
        else
          0
    in

    let next_p =
        if next_p_mod_100 < 0 then
          next_p_mod_100 + 100
        else
          next_p_mod_100
    in

    (next_p, times)
;;

let parse_line s =
    let (dir, distance) = Scanf.sscanf s "%c%d" (fun c dis -> (c, dis)) in

    match dir with
    | 'L' -> -distance
    | 'R' -> distance
    | _ -> failwith "invalid direction"
;;

let () =
    let l = Sys.argv.(1) |> File.read_list_of_line parse_line in

    let res =
        CCList.fold_left
          (fun (p, count) r ->
             let (next_p, times) = next_point p r in
             (next_p, count + times) )
          (50, 0)
          l
        |> snd
    in

    print_int res
;;
