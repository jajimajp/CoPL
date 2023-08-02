type 'a env = (string * 'a) list

let empty = []

let extend id value env =
  (id, value)::env

let rec assoc id env =
  match env with
    [] -> failwith ("Not bound:" ^ id)
  | (k, v)::rest ->
      if id = k then v
      else assoc id rest

let to_list env = env

