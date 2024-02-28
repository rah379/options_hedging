let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;


(*let list_of_file = read_file "Options_To_Hedge/options.txt"*)

let rec print_list (s : string list) : unit = 
  match s with
  | [] -> ()
  | h :: t -> (print_endline h); print_list t

let rec float_list_to_string_list (f : float list) : string list = 
  match f with
  | h :: t -> (string_of_float h) :: float_list_to_string_list t
  | [] -> []


let print_full_option (s : string) : unit = 
  let init_op = Option.set_init_vals (read_file s) in
    let init_op_floats = Option.list_of_option init_op in let 
      str_list = float_list_to_string_list init_op_floats in 
        print_list str_list


let main () = (print_full_option "Options_To_Hedge/options.txt")

let () = main ()
