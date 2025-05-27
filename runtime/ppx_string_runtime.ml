open! Base
include Ppx_string_runtime_intf.Definitions

[%%template
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
let globalize_if_local t = t
let[@mode local] globalize_if_local t = String.globalize t

module For_string = struct
  let empty = ""
  let of_string t = t
  let convert t = t
  let concat list = String.concat ~sep:"" list
  let finish_one = (globalize_if_local [@mode m])

  let pad t ~len =
    let n = String.length t in
    if n >= len then t else String.make (len - n) ' ' ^ t
  ;;
end
[@@mode m = (global, local)]]
