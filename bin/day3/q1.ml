open Lib

let find_largest_num nl =
    let rec find_max_battery n nl rest_len rest_l =
        match nl with
        | [] -> (n, rest_l)
        | nl when CCList.length nl = rest_len -> (n, rest_l)
        | x :: tail when x > n -> find_max_battery x tail rest_len tail
        | _ :: tail -> find_max_battery n tail rest_len rest_l
    in

    let (first, next_l) = find_max_battery 0 nl 1 nl in
    let (second, _) = find_max_battery 0 next_l 0 next_l in

    (first * 10) + second
;;

let parse_line s =
    let zero_code = CCChar.code '0' in
    CCString.to_list s |> CCList.map (fun c -> CCChar.code c - zero_code)
;;

let () =
    let nll = File.read_list_of_line parse_line Sys.argv.(1) in
    let res = nll |> CCList.map find_largest_num |> CCList.fold_left ( + ) 0 in
    print_int res
;;
