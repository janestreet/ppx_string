open! Base

module Definitions = struct
  (** Signature for runtime implementations of Ppx_string's backend.

      May be used for derived ppxes using different types or modified behavior. Types [t],
      [conversion], and [length] should be erased using destructive substitution, i.e.
      [:=]. Otherwise they introduce new aliases for the types in question, and error
      messages or Merlin may start referring to them. *)
  module type%template S = sig
    (** Result type of interpolation, and of interpolated [%{values}]. *)
    type t

    (** Result type of %[{converted#String}] interpolated values. This will often be
        either [string] or [t], depending on what is convenient for the configured ppx. *)
    type conversion

    (** Type of length values for %[{padding#:8}]. *)
    type length

    (** Empty string. *)
    val empty : t

    (** Literal string. *)
    val of_string : string -> t

    (** Finish a conversion to [t]. *)
    val convert : conversion @ m -> t @ m

    (** Pad to some minimum length. *)
    val pad : t @ m -> len:length @ m -> t @ m

    (** Combine multiple values in order. *)
    val concat : t list @ m -> t

    (** Called in place of [concat] when there is just one component, an interpolated
        part, in the interpolation. This function has two purposes:

        1. Ensure an argument has type [t] in expanded code without calling [concat] and
           without needing the type [t] to be exported explicitly for a type annotation.
           See note above about destructive substitution.
        2. Globalize [t] when using the version of the interface designed for extensions
           that consume [local]s but produce [global] concatenation results. *)
    val finish_one : t @ m -> t
  end
  [@@mode m = (global, local)]
end

module type%template Ppx_string_runtime = sig @@ portable
  include module type of struct
    include Definitions
  end

  module For_string :
    S [@mode m] with type t := string and type length := int and type conversion := string
  [@@mode m = (global, local)]
end
