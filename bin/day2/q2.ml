open Lib

let is_invalid n =
    let ns = CCInt.to_string n in
    let len = CCString.length ns in
    let half = len / 2 in

    let rec loop sub_l =
        if sub_l = 0 then
          false
        else if len mod sub_l = 0 then begin
          let sub_s = CCString.sub ns 0 sub_l in
          let repeated = CCString.repeat sub_s (len / sub_l) in
          if repeated = ns then
            true
          else
            loop (sub_l - 1)
        end else
          loop (sub_l - 1)
    in
    loop half
;;

let sum_of_invalid_id (first, last) =
    let sum = ref 0 in
    for id = first to last do
      if is_invalid id then sum := !sum + id
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
