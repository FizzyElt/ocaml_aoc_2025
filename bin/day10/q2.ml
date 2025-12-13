open Lib
open Util.Infix

type machine =
  { target : int;
    buttons : int list;
    joltage : float list
  }

let parse_lights (s : string) : int =
    s
    |> CCString.to_list
    |> CCList.filter (fun c -> c = '#' || c = '.')
    |> CCList.foldi
         (fun acc i light ->
            if light = '#' then
              (1 lsl i) + acc
            else
              acc )
         0
;;

let parse_button (s : string) : int =
    CCString.sub s 1 (CCString.length s - 2)
    |> CCString.split_on_char ','
    |> CCList.map (fun b -> CCInt.of_string_exn b)
    |> CCList.fold_left (fun acc i -> (1 lsl i) + acc) 0
;;

let parse_joltage (s : string) : float list =
    CCString.sub s 1 (CCString.length s - 2)
    |> CCString.split_on_char ','
    |> CCList.map (CCInt.of_string_exn >> CCFloat.of_int)
;;

let parse_line (s : string) : machine =
    let parts = CCString.split_on_char ' ' s in

    let (lights, tail) = CCList.hd_tl parts in
    let (buttons, joltage_str_l) =
        CCList.take_drop (CCList.length tail - 1) tail
    in
    let target = parse_lights (CCString.trim lights) in
    let buttons = CCList.map parse_button buttons in

    let joltage = CCList.hd joltage_str_l |> parse_joltage in

    { target; buttons; joltage }
;;

let machines = File.read_list_of_line parse_line Sys.argv.(1)

let solve_machine_joltage machine =
    let open Lp in
    let num_buttons = List.length machine.buttons in
    let v =
        CCArray.init num_buttons (fun i ->
          var ~integer:true (Printf.sprintf "button%d" i) )
    in

    let sum indices =
        CCList.fold_left (fun acc i -> acc ++ v.(i)) (c 0.0) indices
    in

    let obj = minimize (sum (CCList.init num_buttons (fun i -> i))) in

    let constraints =
        CCList.mapi
          (fun counter_idx target_val ->
             let buttons_indices =
                 machine.buttons
                 |> CCList.mapi (fun btn_idx btn_mask ->
                   if btn_mask land (1 lsl counter_idx) > 0 then
                     Some (num_buttons - btn_idx - 1)
                   else
                     None )
                 |> CCList.filter_map (fun v -> v)
             in
             sum buttons_indices =~ c target_val )
          machine.joltage
    in
    let problem = make obj constraints in
    match Lp_glpk.solve problem with
    | Ok (obj_val, _) -> obj_val
    | Error msg ->
      Printf.eprintf "LP solver error: %s\n" msg;
      0.0
;;

let res =
    machines
    |> CCList.fold_left
         (fun acc machine -> acc +. solve_machine_joltage machine)
         0.0
    |> CCInt64.of_float
;;

print_endline (CCInt64.to_string res)
