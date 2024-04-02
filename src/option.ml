type option_tree = 
| Leaf
| Node of (float ref * float ref) * option_tree ref * option_tree ref

type t = option_tree

type selling = 
| American
| European

type types_of_option = 
| Call
| Put
| Function


type option_values = {selling : selling; type_option : types_of_option; s0 : float;
                       u : float; d : float; r : float; n : int; k : float; p : float; q : float}
let empty = Leaf

let init_tree (s0 : 'a) = Node ((ref s0, ref (-1.0)), ref Leaf, ref Leaf)

let set_p_q (vals : option_values) : option_values = 
  let p_prime = (1.0 +. vals.r -. vals.d) /. (vals.u -. vals.d)
      in let q_prime = 1.0 -. p_prime in 
        {vals with p = p_prime; q = q_prime}


let rec set_head (prev_value : 'a) (n : int) (values : option_values): option_tree = if n = 0 then Leaf 
else
  let curr_val = prev_value *. values.u in
  Node ((ref (curr_val), ref (-1.0)), ref (set_tail curr_val (n-1) values), ref (set_head curr_val (n-1) values))
and set_tail (prev_value : 'a) (n : int) (values : option_values): option_tree = if n = 0 then Leaf 
else
  let curr_val = prev_value *. values.d in
  Node ((ref (prev_value *. values.d), ref (-1.0)), ref (set_tail curr_val (n-1) values), ref (set_head curr_val (n-1) values))

let set_s_vals (init : t) (values : option_values): t = match init with
| Leaf -> failwith "this shouldn't be happening"
| Node ((s0, _), t, h) -> t := set_tail !s0 values.n values; h := set_head !s0 values.n values; 
  init


let rec set_v_vals_european_call (s_vals : t) (values : option_values) : unit = match s_vals with
| Leaf -> failwith "shouldn't get here"
| Node ((s0, v0), t, h) -> match !t, !h with 
  | Leaf, Leaf -> v0 := Float.max 0. (!s0 -. values.k)
  | _, _ -> set_v_vals_european_call !t values; set_v_vals_european_call !h values; 
  match !t, !h with 
  | Leaf, Leaf -> failwith "wrong"
  | Node ((_, vT), _, _), Node ((_, vH), _, _) -> v0 := (1. /. (1. +. values.r)) *. (values.p *. (!vH) +. values.q *. (!vT))
  | _, _ -> failwith "wrong"

let set_v_vals_european_put (s_vals : t) (values : option_values) : unit = match s_vals with
| Leaf -> failwith "shouldn't get here"
| Node ((s0, v0), t, h) -> match !t, !h with 
  | Leaf, Leaf -> v0 := Float.max 0. (values.k -. !s0)
  | _, _ -> set_v_vals_european_call !t values; set_v_vals_european_call !h values; 
  match !t, !h with 
  | Leaf, Leaf -> failwith "wrong"
  | Node ((_, vT), _, _), Node ((_, vH), _, _) -> v0 := (1. /. (1. +. values.r)) *. (values.p *. (!vH) +. values.q *. (!vT))
  | _, _ -> failwith "wrong"

let set_v_vals_american_call (s_vals : t) (values : option_values) : unit = match s_vals, values with 
| _, _ -> failwith "unimplemented"

let set_v_vals_american_put (s_vals : t) (values : option_values) : unit = match s_vals, values with
| _, _ -> failwith "Unimplemented"

let set_v_vals_american (s_vals : t) (values : option_values) : t = match values.type_option with
| Call -> set_v_vals_american_call s_vals values; s_vals
| Put -> set_v_vals_american_put s_vals values; s_vals
| Function -> failwith "Unimplemented"

let set_v_vals_european (s_vals : t) (values : option_values) : t = match values.type_option with
| Call -> set_v_vals_european_call s_vals values; s_vals
| Put -> set_v_vals_european_put s_vals values; s_vals
| Function -> failwith "Unimplemented"

let set_v_vals (s_vals : t) (values : option_values) : t = match values.selling with
| American -> set_v_vals_american s_vals values
| European -> set_v_vals_european s_vals values
