open Stdlib
open StdLabels

module type S = Ppx_string_runtime_intf.S

module For_string = struct
  let empty = ""
  let of_string t = t
  let convert t = t
  let concat list = String.concat ~sep:"" list

  let pad t ~len =
    let n = String.length t in
    if n >= len then t else String.make (len - n) ' ' ^ t
  ;;

  external identity : string -> string = "%identity"
end
