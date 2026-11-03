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
    -> bytes @ local
    @@ portable
    = "Ppx_string_runtime_unsafe_create_local_bytes"
  [@@noalloc]

  let[@zero_alloc] create_local len = exclave_
    if len > Stdlib.Sys.max_string_length then invalid_arg "Bytes.create_local";
    unsafe_create_local len
  ;;

  let%template[@alloc stack] create x = exclave_ create_local x [@@zero_alloc]

  let%template[@alloc a = (heap, stack)] make n c =
    (let t = (create [@alloc a]) n in
     unsafe_fill t ~pos:0 ~len:n c;
     t)
    [@exclave_if_stack a]
  ;;
end

module String = struct
  include Stdlib.StringLabels

  external globalize : string @ local -> string @@ portable = "%obj_dup"

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

  include%template (
  struct
    let[@alloc m = heap] smart_globalize a = globalize a
    let[@alloc m = stack] smart_globalize a = a
  end :
  sig
  @@ portable
    val smart_globalize : string @ local -> string @ m
    [@@alloc __ @ m = (heap_global, stack_local)]
  end)

  let%template[@alloc a = (heap, stack)] concat ?(sep = "") (l @ local) =
    match[@exclave_if_stack a] l with
    | [] -> ""
    | [ x ] -> (smart_globalize [@alloc a]) x
    | l ->
      let sep_len = length sep in
      let dst = (Bytes.create [@alloc a]) (sum_lengths 0 sep_len l) in
      unsafe_blits ~dst ~dst_pos:0 ~sep ~sep_len l;
      Bytes.unsafe_to_string dst
  ;;

  let%template[@alloc a = (heap, stack)] append (s1 @ local) (s2 @ local) =
    (let l1 = length s1
     and l2 = length s2 in
     let s = (Bytes.create [@alloc a]) (l1 + l2) in
     Bytes.unsafe_blit_string ~src:s1 ~src_pos:0 ~dst:s ~dst_pos:0 ~len:l1;
     Bytes.unsafe_blit_string ~src:s2 ~src_pos:0 ~dst:s ~dst_pos:l1 ~len:l2;
     Bytes.unsafe_to_string s)
    [@exclave_if_stack a]
  ;;

  let%template[@alloc a = (heap, stack)] make n c =
    Bytes.unsafe_to_string ((Bytes.make [@alloc a]) n c) [@exclave_if_stack a]
  ;;
end

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
