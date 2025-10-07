open! Base
include Ppx_string_runtime_intf.Definitions

(* The behavior of [finish_one] in the global and local versions of [For_string] below
   differ in a way that isn't actually compatible with allocation polymorphism. What's
   different between them is not {i where} we would like the result to be allocated, but
   {i whether} we would like to allocate at all. In other words: in order to ensure that a
   value is on the heap, we want to move it to the heap only if we don't know that it's on
   the heap already.

   While we might be hesitant to supply a function like [globalize_if_local] for
   templating over in client code, we use it here only to implement the runtime modules
   for two different ppxs, and they need not actually be mode- or allocation-polymorphic.
   So our use of [ppx_template] here is just a trick for convenience.
*)

[%%template
let[@mode local] [@alloc heap] globalize_if_local_to_heap : string @ local -> string =
  String.globalize
;;

let[@mode global] [@alloc heap] globalize_if_local_to_heap t = t
let[@mode local] [@alloc stack] globalize_if_local_to_heap t = t]

[%%template
[@@@alloc a_out @ m_out = (heap_global, stack_local)]
[@@@alloc a_in @ m_in = (a_out @ m_out, stack_local)]

module [@alloc a_in a_out] For_string : sig
  include
    S
    [@alloc a_in a_out]
    with type t := string
     and type length := int
     and type conversion := string @@ portable
end = struct
  let empty = ""
  let of_string t = t
  let convert t = t
  let concat list = (String.concat [@alloc a_out]) ~sep:"" list [@exclave_if_stack a_out]

  let finish_one : string @ m_in -> string @ m_out =
    fun t ->
    (globalize_if_local_to_heap [@mode m_in] [@alloc a_out]) t [@exclave_if_stack a_out]
  ;;

  let pad t ~len =
    let n = String.length t in
    if n >= len
    then t
    else
      (String.append [@alloc a_in])
        ((String.make [@alloc a_in]) (len - n) ' ')
        t [@exclave_if_stack a_in]
  ;;
end]
