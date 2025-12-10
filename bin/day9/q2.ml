open Lib

let parse_line s = Scanf.sscanf s "%d,%d" (fun x y -> (x, y))

let reds = File.read_list_of_line parse_line Sys.argv.(1)

let area (x1, y1) (x2, y2) =
    (CCInt.abs (x2 - x1) + 1) * (CCInt.abs (y2 - y1) + 1)
;;

let min_max a b = (CCInt.min a b, CCInt.max a b)

let crosses_axis (min_a, max_a) (min_b, max_b) (a, b1) (_, b2) =
    a > min_a && a < max_a && CCInt.min b1 b2 < max_b && CCInt.max b1 b2 > min_b
;;

let edgeCrossesRect rangeX rangeY (((x1, y1) as e1), ((x2, y2) as e2)) =
    crosses_axis rangeX rangeY e1 e2
    || crosses_axis rangeY rangeX (y1, x1) (y2, x2)
;;

let rectValid edges ((x1, y1), (x2, y2)) =
    let f = edgeCrossesRect (min_max x1 x2) (min_max y1 y2) in
    edges |> CCArray.for_all (fun e -> not (f e))
;;

let pairwise points =
    let rec loop points acc =
        match points with
        | [] | [ _ ] -> acc
        | a :: b :: tail -> loop (b :: tail) ((a, b) :: acc)
    in
    loop points [] |> CCList.rev
;;

let edges =
    reds
    @ [ CCList.last_opt reds |> CCOption.get_exn_or "not found lasted";
        CCList.hd reds
      ]
    |> pairwise
    |> CCArray.of_list
;;

let areas =
    begin
      let points = reds |> CCArray.of_list in
      let len = CCArray.length points in
      let areas = ref [] in

      for i = 0 to len - 2 do
        for j = i + 1 to len - 1 do
          let a = points.(i) in
          let b = points.(j) in

          areas := (area a b, a, b) :: !areas
        done
      done;
      !areas |> CCList.sort (fun (a, _, _) (b, _, _) -> b - a)
    end
;;

let rec find_max_area areas =
    match areas with
    | [] -> 0
    | (area, p1, p2) :: rest ->
      if rectValid edges (p1, p2) then
        area
      else
        find_max_area rest
;;

let res = find_max_area areas;;

print_int res
