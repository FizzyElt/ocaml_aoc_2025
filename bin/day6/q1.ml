open Lib
let parse_line s =
    CCString.split_on_char ' ' s |> CCList.filter (fun s -> s <> "")
;;

let flip_matrix (mat : 'a array array) =
    let rows = CCArray.length mat in
    let cols = CCArray.length mat.(0) in

    CCArray.init_matrix cols rows (fun c r -> mat.(r).(c))
;;

let total (matrix : 'a array array) (operators : string array) =
    CCArray.fold2
      (fun acc nums opt ->
         let res =
             match opt with
             | "*" -> CCArray.fold ( * ) 1 nums
             | "+" -> CCArray.fold ( + ) 0 nums
             | _ -> 0
         in
         acc + res )
      0
      matrix
      operators
;;

let () =
    let lines = File.read_list_of_line parse_line Sys.argv.(1) |> CCList.rev in
    let (operators, num_matrix) = CCList.hd_tl lines in
    let num_matrix =
        num_matrix
        |> CCList.map
             (CCFun.compose (CCList.map CCInt.of_string_exn) CCArray.of_list)
        |> CCArray.of_list
        |> flip_matrix
    in
    let operators = CCArray.of_list operators in
    let res = total num_matrix operators in
    print_int res;
    ()
;;
