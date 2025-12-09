open Lib

type coord = int * int

let get_area (a : coord) (b : coord) =
    let (a_x, a_y) = a in
    let (b_x, b_y) = b in
    (CCInt.abs (a_x - b_x) + 1) * (CCInt.abs (a_y - b_y) + 1)
;;

let divide_two ((x, y) : coord) : coord = (x / 2, y / 2)

let coord_to_string ((x, y) : coord) = Printf.sprintf "(%d, %d)" x y

let get_center (a : coord) (b : coord) : coord =
    let (a_x, a_y) = a in
    let (b_x, b_y) = b in
    ((a_x + b_x) / 2, (a_y + b_y) / 2)
;;

let get_vertical_lines (coords : coord array) =
    print_endline "vertical lines";
    coords
    |> CCArray.to_list
    |> CCList.sort (fun (a_x, a_y) (b_x, b_y) ->
      let x_ord = a_x - b_x in
      if x_ord = 0 then
        a_y - b_y
      else
        x_ord )
    (* |> Util.tap (fun l ->
      l
      |> CCList.iter (fun (x, y) -> Printf.printf "(%d, %d)\n" (x / 2) (y / 2)) ) *)
    |> CCList.chunks 2
;;

let get_horizontal_lines (coords : coord array) =
    print_endline "horizontal lines";
    coords
    |> CCArray.to_list
    |> CCList.sort (fun (a_x, a_y) (b_x, b_y) ->
      let y_ord = a_y - b_y in
      if y_ord = 0 then
        a_x - b_x
      else
        y_ord )
    (* |> Util.tap (fun l ->
      l
      |> CCList.iter (fun (x, y) -> Printf.printf "(%d, %d)\n" (x / 2) (y / 2)) ) *)
    |> CCList.chunks 2
;;

let is_line
      (horizontal_lines : coord list list)
      (vertical_lines : coord list list)
      ((a_x, a_y) : coord)
      ((b_x, b_y) : coord)
  =
    if a_x = b_x then begin
      let min_y = CCInt.min a_y b_y in
      let max_y = CCInt.max a_y b_y in
      vertical_lines
      |> CCList.exists (function
        | [ (x, start_y); (_, end_y) ] ->
          x = a_x && start_y = min_y && end_y = max_y
        | _ -> failwith "invalid case" )
    end else if a_y = b_y then begin
      let min_x = CCInt.min a_x b_x in
      let max_x = CCInt.max a_x b_x in
      horizontal_lines
      |> CCList.exists (function
        | [ (start_x, y); (end_x, _) ] ->
          y = a_y && start_x = min_x && end_x = max_x
        | _ -> failwith "invalid case" )
    end else
      false
;;

let is_in_polygon (coord : coord) (vertical_lines : coord list list) =
    let (x, y) = coord in
    let rec loop lines cross_count =
        match lines with
        | [] -> cross_count
        | [ (start_x, start_y); (_end_x, end_y) ] :: rest ->
          if start_x > x && start_y <= y && y <= end_y then
            loop rest (cross_count + 1)
          else
            loop rest cross_count
        | _ -> failwith "invalid case"
    in
    let cross_count = loop vertical_lines 0 in

    cross_count mod 2 = 1
;;

let check_rect
      ((a_x, a_y) : coord)
      ((b_x, b_y) : coord)
      (vertical_lines : coord list list)
      (horizontal_lines : coord list list)
  =
    if is_line horizontal_lines vertical_lines (a_x, a_y) (b_x, b_y) then
      true
    else begin
      let max_x = CCInt.max a_x b_x in
      let min_x = CCInt.min a_x b_x in
      let max_y = CCInt.max a_y b_y in
      let min_y = CCInt.min a_y b_y in

      let in_polygon = ref true in

      (* top line *)
      for i = min_x + 1 to max_x - 1 do
        if !in_polygon then
          in_polygon
          := !in_polygon && is_in_polygon (i, min_y + 1) vertical_lines
      done;

      (* bottom line *)
      for i = min_x + 1 to max_x - 1 do
        if !in_polygon then
          in_polygon
          := !in_polygon && is_in_polygon (i, max_y - 1) vertical_lines
      done;

      (* left line *)
      for i = min_y + 1 to max_y - 1 do
        if !in_polygon then
          in_polygon
          := !in_polygon && is_in_polygon (min_x + 1, i) vertical_lines
      done;

      (* right line *)
      for i = min_y + 1 to max_y - 1 do
        if !in_polygon then
          in_polygon
          := !in_polygon && is_in_polygon (max_x - 1 + 1, i) vertical_lines
      done;

      !in_polygon
    end
;;

let find_max_area
      (coords : coord array)
      (vertical_lines : coord list list)
      (horizontal_lines : coord list list)
  =
    let max_area = ref 0 in
    let len = CCArray.length coords in
    for i = 0 to len - 2 do
      for j = i + 1 to len - 1 do
        begin
          let a = coords.(i) in
          let b = coords.(j) in
          if check_rect a b vertical_lines horizontal_lines then (
            let origin_a = divide_two a in
            let origin_b = divide_two b in

            let area = get_area origin_a origin_b in
            (* Printf.printf
              "%s  %s  %d\n"
              (coord_to_string origin_a)
              (coord_to_string origin_b)
              area; *)
            if area > !max_area then max_area := area
          )
        end
      done
    done;

    !max_area
;;

let parse_line (s : string) : coord =
    Scanf.sscanf s "%d,%d" (fun x y -> (x * 2, y * 2))
;;

let () =
    let coords =
        File.read_list_of_line parse_line Sys.argv.(1) |> CCArray.of_list
    in
    let vertical_lines = get_vertical_lines coords in
    let horizontal_lines = get_horizontal_lines coords in
    print_int (CCList.length vertical_lines);
    print_newline ();
    print_int (CCList.length horizontal_lines);
    print_newline ();
    let res = find_max_area coords vertical_lines horizontal_lines in

    (* let check =
        check_rect (9 * 2, 5 * 2) (2 * 2, 3 * 2) horizontal_lines vertical_lines
    in
    Printf.printf "(9,5) (2,3) %b\n" check; *)
    print_int res;
    ()
;;
