(*open Options*)

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

(*let read_one_option (channel : in_channel)  = print_char((input_char channel)) *)

let list_of_file = read_file "Options_To_Hedge/options.txt"

let rec print_list (s : string list) : unit = 
  match s with
  | [] -> ()
  | h :: t -> (print_endline h); print_list t


let () = print_list (read_file "Options_To_Hedge/options.txt"); 
print_string (List.nth list_of_file 0)