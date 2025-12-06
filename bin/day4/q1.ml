open Lib

let is_paper (map : char array array) ((row, col) : int * int) =
    Matrix.get_safe map row col
    |> CCOption.map (fun c -> c = '@')
    |> CCOption.get_or ~default:false
;;

let dirs =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
;;

let near_papers (map : char array array) ((row, col) : int * int) =
    dirs
    |> CCList.map (fun (offset_r, offset_c) ->
      is_paper map (row + offset_r, col + offset_c) )
    |> CCList.filter Fun.id
    |> CCList.length
;;

let total_paper map =
    Matrix.foldi
      (fun acc (row, col) ch ->
         if ch = '@' && near_papers map (row, col) < 4 then
           acc + 1
         else
           acc )
      0
      map
;;

let parse_line s = CCString.to_array s

let () =
    let map =
        File.read_list_of_line parse_line Sys.argv.(1) |> CCArray.of_list
    in
    let res = total_paper map in
    print_int res
;;
