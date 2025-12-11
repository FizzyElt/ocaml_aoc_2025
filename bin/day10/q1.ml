open Lib

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

let parse_line (s : string) : int * int list =
    let parts = CCString.split_on_char ' ' s in

    let (lights, tail) = CCList.hd_tl parts in
    let (buttons, _) = CCList.take_drop (CCList.length tail - 1) tail in
    let lights = parse_lights (CCString.trim lights) in
    let buttons = CCList.map parse_button buttons in

    (lights, buttons)
;;

let machines = File.read_list_of_line parse_line Sys.argv.(1)

module LightSet = CCSet.Make (Int)

let press_button_count ((target, buttons) : int * int list) : int =
    let queue = Queue.create () in
    Queue.push (0, 0) queue;

    let reachable = ref LightSet.empty in

    let res = ref None in

    while (not (Queue.is_empty queue)) && CCOption.is_none !res do
      let (x, n) = Queue.take queue in
      if x = target then
        res := Some n
      else if LightSet.mem x !reachable then
        ()
      else begin
        reachable := LightSet.add x !reachable;
        buttons
        |> CCList.iter (fun btn ->
          let toggled = x lxor btn in
          Queue.push (toggled, n + 1) queue )
      end
    done;

    CCOption.get_exn_or "impossible" !res
;;

let res =
    machines
    |> CCList.map (fun machine -> press_button_count machine)
    |> CCList.fold_left ( + ) 0
;;

print_int res
