open Lib

let (shape_units, regions) =
    begin
      let content = File.read_file Sys.argv.(1) in
      let parts =
          content
          |> CCString.split ~by:"\n\n"
          |> CCList.filter (fun s -> s <> "")
      in

      let (shapes_str_list, regions_str) =
          match CCList.rev parts with
          | [] -> ([], "") (* Empty file *)
          | regions_block :: reversed_shapes_blocks ->
            let shapes_str_list =
                CCList.rev reversed_shapes_blocks
                |> CCList.map String.trim
                |> CCList.filter (fun s -> s <> "")
            in
            (shapes_str_list, regions_block)
      in

      let shape_units =
          List.map
            (CCString.fold
               (fun acc c ->
                  if c = '#' then
                    acc + 1
                  else
                    acc )
               0 )
            shapes_str_list
      in

      let regions =
          regions_str
          |> CCString.split_on_char '\n'
          |> CCList.filter (fun s -> CCString.trim s <> "")
          |> CCList.map (fun region_line ->
            match CCString.split_on_char ':' region_line with
            | [ dimensions_str; quantities_str ] -> begin
              let dimensions_str = CCString.trim dimensions_str in
              let quantities_str = CCString.trim quantities_str in
              let area =
                  CCString.split_on_char 'x' dimensions_str
                  |> CCList.fold_left
                       (fun acc s -> CCInt.of_string_exn s * acc)
                       1
              in

              let quantities =
                  CCString.split_on_char ' ' quantities_str
                  |> CCList.filter (fun s -> CCString.trim s <> "")
                  |> CCList.map (fun s -> CCInt.of_string_exn s)
              in

              (area, quantities)
            end
            | _ -> failwith "fail" )
      in
      (shape_units, regions)
    end
;;

let count_region (area, quantities) =
    let best_case =
        CCList.fold_left2
          (fun acc unit quantity -> acc + (unit * quantity))
          0
          shape_units
          quantities
    in
    if area >= best_case then
      1
    else
      0
;;

let res =
    CCList.fold_left (fun total region -> total + count_region region) 0 regions
;;

print_int res
