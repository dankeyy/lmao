type lmao =
  | Variable of string
  | Abstraction of string * lmao
  | Application of lmao * lmao (*[@warning "-37"]*)


let rec interpret expr =
  match expr with
  | Variable _ -> expr

  | Abstraction _ -> expr

  | Application (Abstraction (x, body), arg) ->
      substitute x arg body (* this is the beta reduction entrypoint basically *)

  | Application (f, arg) ->
      let f' = interpret f in (* here we try to reduce *)
      interpret (Application (f', arg)) (* and retry with the reduced *)


and substitute target replacement body =
  match body with
  | Variable v when v = target ->
    replacement

  | Variable v ->
    Variable v

  (* hopefully careful capture avoidance stuff *)
  | Abstraction (bound, body_expr) when bound <> target && not (free_in bound replacement) ->
    (* if safe to recursively replace within the lambody: *)
    Abstraction (bound, substitute target replacement body_expr)

  (* rename to avoid collisions *)
  | Abstraction (bound, body_expr) ->
    let fresh = fresh_var bound [target; bound] in
    let body_renamed = substitute bound (Variable fresh) body_expr in
    Abstraction (fresh, substitute target replacement body_renamed)

  (* and application is easy because we can just recurse into both ends *)
  | Application (e1, e2) ->
    Application (substitute target replacement e1, substitute target replacement e2)


and fresh_var base used =
  let rec loop n =
    let candidate = if n = 0 then base else base ^ string_of_int n in
    if List.mem candidate used then loop (n + 1) else candidate
  in
  loop 0


and free_in v expr =
  match expr with
  | Variable y -> y = v
  | Abstraction (y, body) -> y <> v && free_in v body
  | Application (e1, e2) -> free_in v e1 || free_in v e2



(* printing util *)
let rec show_lmao_expr = function
  | Variable x -> x
  | Abstraction (x, e) -> "(λ" ^ x ^ "." ^ show_lmao_expr e ^ ")"
  | Application (e1, e2) -> "(" ^ show_lmao_expr e1 ^ " " ^ show_lmao_expr e2 ^ ")"

let string_of_l_exp e = show_lmao_expr e


let id = Abstraction ("x", Variable "x")

(* if all goes well the this should reduce to id *)
let test = Application (id, Abstraction ("y", Variable "y")) (* (λx.x)(λy.y) *)

let () =
  Printf.printf "Result: %s\n" (string_of_l_exp (interpret test))

(* let () = *)
(*   let id = Abstraction ("x", Variable "x") in *)
(*   let ap = Application (id, Variable "x") in *)
(*   print_endline (string_of_l_exp ap) *)
