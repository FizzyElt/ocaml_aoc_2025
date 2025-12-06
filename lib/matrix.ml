let get_safe (matrix : 'a array array) r c =
    try Some matrix.(r).(c) with
    | Invalid_argument _ -> None
;;

let set (matrix : 'a array array) r c v = matrix.(r).(c) <- v

let fold (f : 'b -> 'a -> 'b) (init : 'b) (matrix : 'a array array) =
    let col_reducer acc row = CCArray.fold f acc row in
    matrix |> CCArray.fold col_reducer init
;;

let foldi (f : 'b -> int * int -> 'a -> 'b) (init : 'b) (matrix : 'a array array)
  =
    let col_reducer acc r row =
        CCArray.foldi (fun acc c v -> f acc (r, c) v) acc row
    in
    matrix |> CCArray.foldi col_reducer init
;;

let of_list (mat_list : 'a list list) : 'a array array =
    mat_list |> CCList.map CCArray.of_list |> CCArray.of_list
;;

let to_list (mat : 'a array array) : 'a list list =
    mat |> CCArray.map CCArray.to_list |> CCArray.to_list
;;
