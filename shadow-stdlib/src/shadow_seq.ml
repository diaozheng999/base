(** Recreating 4.08's Seq module in 4.06.1
    See: https://ocaml.org/releases/4.08/htmlman/libref/Seq.html *)

type 'a t = unit -> 'a node

and 'a node =
  | Nil
  | Cons of 'a * 'a t

let empty () = Nil

let return a () = Cons (a, empty)

let rec map f seq () =
  match seq () with
    | Nil -> Nil
    | Cons (e, next) -> Cons (f e, map f next)

let rec filter f seq () =
  match seq () with
    | Nil -> Nil
    | Cons (e, next) when f e -> Cons (e, filter f next)
    | Cons (_, next) -> filter f next ()

let rec filter_map f seq () =
  match seq () with
    | Nil -> Nil
    | Cons (e, next) ->
      match f e with
        | Some r -> Cons (r, filter_map f next)
        | None -> filter_map f next ()


let rec flat_map f seq () =
  let rec join seq e () =
    match seq () with
      | Nil -> e ()
      | Cons (node, next) -> Cons (node, join next e)
  in
  match seq () with
    | Nil -> Nil
    | Cons (e, next) -> join (f e) (flat_map f next) ()

let rec fold_left f a seq =
  match seq () with
    | Nil -> a
    | Cons (e, next) -> fold_left f (f a e) next

let rec iter f seq =
  match seq () with
    | Nil -> ()
    | Cons (e, next) -> f e; iter f next
