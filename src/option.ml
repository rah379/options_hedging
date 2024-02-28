type t = float list

let empty = [] 
let s0 = -1.0
let u = -1.0
let d = -1.0
let r = -1.0

let rec set_init_vals (s : string list) : float list = 
  match s with
  | h :: t -> (try float_of_string h with _ -> 3. ) :: set_init_vals t
  | [] -> []

let list_of_option (a : t) : float list = a