open Lib

let next_point p r =
    let next_p = (p + r) mod 100 in

    if next_p < 0 then
      next_p + 100
    else if next_p > 0 then
      next_p
    else
      0
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
             let next_p = next_point p r in
             let count =
                 if next_p = 0 then
                   count + 1
                 else
                   count
             in
             (p, count) )
          (50, 0)
          l
        |> snd
    in
    print_int res
;;
