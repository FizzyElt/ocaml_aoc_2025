open Lib
let is_twice ns =
    let len = CCString.length ns in
    let mid_len = len / 2 in

    let front = CCString.sub ns 0 mid_len in
    let back = CCString.sub ns mid_len mid_len in

    front = back
;;

let sum_of_invalid_id (first, last) =
    let sum = ref 0 in
    for id = first to last do
      let ns = CCInt.to_string id in
      if CCString.length ns mod 2 = 0 && is_twice ns then sum := !sum + id
    done;

    !sum
;;

let parse_input s =
    let res =
        s
        |> CCString.split_on_char ','
        |> CCList.map (fun s ->
          Scanf.sscanf s "%d-%d" (fun first last -> (first, last)) )
    in
    res
;;

let () =
    let file_string = File.read_file Sys.argv.(1) in
    let l = parse_input file_string in
    let res = CCList.fold_left (fun acc x -> sum_of_invalid_id x + acc) 0 l in
    print_int res
;;
