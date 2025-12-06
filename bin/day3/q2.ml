open Lib

let find_largest_jolts (n : int) (batteries : int list) : int =
    let rec find_max_battery n nl rest_len rest_l =
        match nl with
        | [] -> (n, rest_l)
        | nl when CCList.length nl = rest_len -> (n, rest_l)
        | x :: tail when x > n -> find_max_battery x tail rest_len tail
        | _ :: tail -> find_max_battery n tail rest_len rest_l
    in

    let reducer ((r, ll) : int list * int list) (rest_len : int) =
        let (n, next_l) = find_max_battery 0 ll rest_len ll in
        (n :: r, next_l)
    in

    CCList.init n (fun i -> n - 1 - i)
    |> CCList.fold_left reducer ([], batteries)
    |> fst
    |> CCList.foldi (fun acc i n -> acc + (n * CCInt.pow 10 i)) 0
;;

let parse_line (s : string) : int list =
    let zero_code = CCChar.code '0' in
    CCString.to_list s |> CCList.map (fun c -> CCChar.code c - zero_code)
;;

let () =
    let nll = File.read_list_of_line parse_line Sys.argv.(1) in
    let res =
        nll |> CCList.map (find_largest_jolts 12) |> CCList.fold_left ( + ) 0
    in
    print_int res
;;
