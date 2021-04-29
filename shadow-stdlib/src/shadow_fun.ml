external id : 'a -> 'a = "%identity"

exception Finally_raised of exn

let const_uncurried a = fun [@bs] _ -> a

let const a =
  let uncurried = const_uncurried a in
  fun v -> (uncurried v [@bs])

let flip_uncurried f = fun [@bs] b a -> (f a b [@bs])

let flip f a b =
  let flipped = flip_uncurried (fun [@bs] a b -> f a b) in
  (flipped a b [@bs])
(** Recreating 4.08's Fun module in 4.06.1
    See: https://ocaml.org/releases/4.08/htmlman/libref/Fun.html *)

let negate_uncurried f = fun [@bs] x -> not (f x [@bs])

let negate f =
  let negated = negate_uncurried (fun [@bs] x -> f x) in
  fun x -> (negated x [@bs])

let protect_uncurried ~finally work =
  let result =
    try work () [@bs]
    with exn ->
      let () = try finally () [@bs] with exn -> raise (Finally_raised exn) in
      raise exn
  in
  try
    let () = (finally () [@bs]) in
    result
  with exn -> raise (Finally_raised exn)

let protect ~finally work =
  protect_uncurried
    ~finally:(fun [@bs] () -> finally ())
    (fun [@bs] () -> work ())
