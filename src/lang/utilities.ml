(** Utilities *)

(** [in_order x] incrementally builds a map from [int] to small
    [int]s. The map depends on the order in which the integers
    of the domain are observed. *)
let in_order =
  let h = Hashtbl.create 13 in
  let c = ref (-1) in
  fun (x : int) ->
  try
    Hashtbl.find h x
  with Not_found ->
    incr c;
    Hashtbl.add h x !c;
    !c

(** [fresh_name_generator constructor] returns a fresh name generator
    for identifier build from [constructor] applied to a string. *)
let fresh_name_generator constructor =
  let c = ref 0 in
  fun p -> incr c; constructor (Printf.sprintf "%s_%d" p !c)
