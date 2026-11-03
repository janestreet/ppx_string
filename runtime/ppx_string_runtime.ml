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

module Bytes = struct
  include Stdlib.BytesLabels

  external unsafe_create_local
    :  int
    -> bytes
    = "Ppx_string_runtime_unsafe_create_local_bytes"
  [@@noalloc]

  let[@zero_alloc] create_local len =
    if len > Stdlib.Sys.max_string_length then invalid_arg "Bytes.create_local";
    unsafe_create_local len
  ;;

  let make n c =
    let t = create n in
    unsafe_fill t ~pos:0 ~len:n c;
    t
  ;;
end

module String = struct
  include Stdlib.StringLabels

  external globalize : string -> string = "caml_obj_dup"

  let ensure_ge (x : int) y = if x >= y then x else invalid_arg "String.concat"

  let rec sum_lengths acc seplen = function
    | [] -> acc
    | hd :: [] -> length hd + acc
    | hd :: tl -> sum_lengths (ensure_ge (length hd + seplen + acc) acc) seplen tl
  ;;

  let rec unsafe_blits ~dst ~dst_pos ~sep ~sep_len = function
    | [] -> ()
    | hd :: [] ->
      Bytes.unsafe_blit_string ~src:hd ~src_pos:0 ~dst ~dst_pos ~len:(length hd)
    | hd :: tl ->
      Bytes.unsafe_blit_string ~src:hd ~src_pos:0 ~dst ~dst_pos ~len:(length hd);
      Bytes.unsafe_blit_string
        ~src:sep
        ~src_pos:0
        ~dst
        ~dst_pos:(dst_pos + length hd)
        ~len:sep_len;
      unsafe_blits ~dst ~dst_pos:(dst_pos + length hd + sep_len) ~sep ~sep_len tl
  ;;

  include (
  struct
    let smart_globalize a = globalize a
  end :
  sig
    val smart_globalize : string -> string
  end)

  let concat ?(sep = "") l =
    match l with
    | [] -> ""
    | [ x ] -> smart_globalize x
    | l ->
      let sep_len = length sep in
      let dst = Bytes.create (sum_lengths 0 sep_len l) in
      unsafe_blits ~dst ~dst_pos:0 ~sep ~sep_len l;
      Bytes.unsafe_to_string dst
  ;;

  let append s1 s2 =
    let l1 = length s1
    and l2 = length s2 in
    let s = Bytes.create (l1 + l2) in
    Bytes.unsafe_blit_string ~src:s1 ~src_pos:0 ~dst:s ~dst_pos:0 ~len:l1;
    Bytes.unsafe_blit_string ~src:s2 ~src_pos:0 ~dst:s ~dst_pos:l1 ~len:l2;
    Bytes.unsafe_to_string s
  ;;

  let make n c = Bytes.unsafe_to_string (Bytes.make n c)
end

let globalize_if_local_to_heap t = t

module For_string : sig
  include S with type t := string and type length := int and type conversion := string
end = struct
  let empty = ""
  let of_string t = t
  let convert t = t
  let concat list = String.concat ~sep:"" list
  let finish_one : string -> string = fun t -> globalize_if_local_to_heap t

  let pad t ~len =
    let n = String.length t in
    if n >= len then t else String.append (String.make (len - n) ' ') t
  ;;
end
