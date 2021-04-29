open! Import

module T = struct
  type t = Nothing

  let unreachable_code = function
    | (_ : t) -> [%raw "void 0"]
  ;;

  let all = []
  let hash_fold_t _ t = unreachable_code t
  let hash = unreachable_code
  let compare a _ = unreachable_code a
  let sexp_of_t = unreachable_code
  let t_of_sexp sexp = Sexplib.Conv_error.empty_type "Base.Nothing.t" sexp
  let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) = { untyped = Union [] }
  let to_string = unreachable_code
  let of_string (_ : string) = failwith "Base.Nothing.of_string: not supported"
end

include T

include Identifiable.Make (struct
    include T

    let module_name = "Base.Nothing"
  end)
