(*open Base*)
open Core.Std
open Sys
open Unix
open Result

let backlight_dir = "/sys/class/backlight"
let backlight_device_dir dev = String.concat ?sep:(Some "/") [backlight_dir; dev]
let brightness_file dev = String.concat ?sep:(Some "/") [backlight_device_dir dev; "brightness"]
let max_brightness_file dev = String.concat ?sep:(Some "/") [backlight_device_dir dev; "max_brightness"]

let read_file file =
  try (Ok (In_channel.with_file file ~f:(fun ic -> In_channel.input_line ic)))
  with Sys_error err -> Error err
                          
let write_file file value =
  try (Ok (Out_channel.with_file file ~f:(fun oc -> Out_channel.output_string oc value)))
  with Sys_error err -> Error err

let read_brightness dev =
  match brightness_file dev |> read_file with
  | Error e -> Error e
  | Ok None -> Error "Brightness file was empty"
  | Ok (Some x) ->
    try Ok (Caml.Pervasives.int_of_string x)
    with Failure err -> Error "Backlight value was not an int?"

let read_max_brightness dev =
  match max_brightness_file dev |> read_file with
  | Error e -> Error e
  | Ok None -> Error "Brightness file was empty"
  | Ok (Some x) ->
    try Ok (Caml.Pervasives.int_of_string x)
    with Failure err -> Error "Backlight max value was not an int?"

let write_brightness dev value =
  try
    let int_value = Caml.Pervasives.string_of_int value in
    write_file (brightness_file dev) int_value
  with Failure err -> Error err
  
let rec get_first_directory_entry dirhandle =
  let entry = readdir_opt dirhandle in
  match entry with
  | None -> Error "Could not find any directory entries"
  | Some e ->
    match String.is_prefix e "." with
    | true -> get_first_directory_entry dirhandle
    | false -> Ok e

let get_first_backlight_device =
  let dirhandle = opendir backlight_dir in
  let dev = get_first_directory_entry dirhandle in
  closedir dirhandle;
  dev

let get_all_backlight_devices =
  let dirhandle = opendir backlight_dir in
  closedir dirhandle

let get_backlight device =
  match device with
  | Some d ->
     (match read_brightness d with
     | Ok b -> printf "%d\n" b
     | Error err -> printf "%s\n" err)
  | _ -> printf "lol\n"

let set_backlight device value =
  match device with
  | None -> printf "Could not acquire a backlight device\n"
  | Some d ->
    (match write_brightness d value with
     | Ok _ -> ()
     | Error e -> printf "%s\n" e)
                    
let inc_backlight device value =
  match device with
  | None -> printf "Could not acquire a backlight device\n"
  | Some d ->
    (match read_brightness d with
     | Ok cv ->
       (match write_brightness d (cv + value) with
        | Ok _ -> ()
        | Error e -> printf "%s\n" e)
     | Error e -> printf "%s\n" e)
                     
let rec dec_backlight device value =
  match device with
  | None -> printf "Could not acquire a backlight device\n"
  | Some d ->
    (match read_brightness d with
     | Ok cv ->
       (match write_brightness d (cv - value) with
        | Ok _ -> ()
        | Error e -> printf "%s\n" e)
     | Error e -> printf "%s\n" e)
                        
let () =
  let rec command_parse_options m dev nval =
    try
      match dev with
      | None ->
        (match get_first_backlight_device with
         | Ok d -> m (Some d) nval
         | Error err -> (fun () -> printf "%s\n" err))
      | Some _ -> m dev nval
    with Failure err -> (fun () -> printf "%s\n" err)
  in
  let command_get =
    Command.basic
      ~summary: "Get the current backlight value"
      Command.Spec.(
        step
          (fun m dev ->
             match dev with
             | None ->
               (match get_first_backlight_device with
                | Ok d -> m (Some d)
                | Error err -> m None)
             | Some _ -> m dev
          )
        +> flag "-dev" (optional string) ~doc: "Backlight device to use")
      (fun dev () -> get_backlight dev) in
  let command_list =
    Command.basic
      ~summary: "List the available backlight devices"
      Command.Spec.(empty)
      (fun () -> printf "lol\n") in
  let command_set =
    Command.basic
      ~summary: "Set the backlight value"
      Command.Spec.(
        step command_parse_options
        +> flag "-dev" (optional string) ~doc:"Backlight device to use"
        +> anon ("val" %: int))
      (fun dev new_val () -> set_backlight dev new_val) in
  let command_inc =
    Command.basic
      ~summary: "Increase the backlight value"
      Command.Spec.(
        step command_parse_options
        +> flag "-dev" (optional string) ~doc:"Backlight device to use"
        +> anon ("val" %: int))
      (fun dev new_val () -> inc_backlight dev new_val) in
  let command_dec =
    Command.basic
      ~summary: "Decrease the backlight value"
      Command.Spec.(
        step command_parse_options
        +> flag "-dev" (optional string) ~doc:"Backlight device to use"
        +> anon ("val" %: int))
      (fun dev new_val () -> dec_backlight dev new_val) in
  let commands = [ "get", command_get
                 ; "list", command_list
                 ; "set", command_set
                 ; "inc", command_inc
                 ; "dec", command_dec ] in
  Command.group ~summary:"Foo" ~readme:(fun() -> "I POOP ON YOUR FACE") commands
  |> Command.run ~version:"0.0.1" ~build_info:"lol"



