open Lib

let flip_matrix (mat : 'a array array) =
    let rows = CCArray.length mat in
    let cols = CCArray.length mat.(0) in

    CCArray.init_matrix cols rows (fun c r -> mat.(r).(c))
;;

let solve_problem
      (str_l : string list)
      (operator : int -> int -> int)
      (init : int)
  =
    let rec loop l acc =
        match l with
        | "" :: tail -> (acc, tail)
        | n :: tail -> loop tail (operator (CCInt.of_string_exn n) acc)
        | [] -> (acc, [])
    in

    loop str_l init
;;

let rec total (l : string list) (operators : string list) (acc : int) =
    match operators with
    | "*" :: tail ->
      let (p_total, rest) = solve_problem l ( * ) 1 in
      total rest tail (acc + p_total)
    | "+" :: tail ->
      let (p_total, rest) = solve_problem l ( + ) 0 in
      total rest tail (acc + p_total)
    | [] -> acc
    | _ -> failwith "invalid operator"
;;

let () =
    let (operators, lines) =
        File.read_file Sys.argv.(1)
        |> CCString.trim
        |> CCString.split_on_char '\n'
        |> CCList.rev
        |> CCList.hd_tl
    in
    let operators =
        operators
        |> CCString.split_on_char ' '
        |> CCList.filter (fun o -> o <> "")
    in
    let num_str_list =
        lines
        |> CCList.rev
        |> CCList.map CCString.to_array
        |> CCArray.of_list
        |> flip_matrix
        |> CCArray.to_list
        |> CCList.map (CCFun.compose CCString.of_array CCString.trim)
    in

    let res = total num_str_list operators 0 in

    print_int res
;;
