open Lib

type coord = int * int

let get_area (a : coord) (b : coord) =
    let (a_r, a_c) = a in
    let (b_r, b_c) = b in
    (CCInt.abs (a_r - b_r) + 1) * (CCInt.abs (a_c - b_c) + 1)
;;

let find_max_area (coords : coord array) =
    let max_area = ref 0 in
    let len = CCArray.length coords in
    for i = 0 to len - 2 do
      for j = i + 1 to len - 1 do
        begin
          let a = coords.(i) in
          let b = coords.(j) in
          let area = get_area a b in
          if area > !max_area then max_area := area
        end
      done
    done;

    !max_area
;;

let parse_line (s : string) : coord = Scanf.sscanf s "%d,%d" (fun r c -> (r, c))

let () =
    let coords =
        File.read_list_of_line parse_line Sys.argv.(1) |> CCArray.of_list
    in

    let res = find_max_area coords in
    print_int res;
    ()
;;
