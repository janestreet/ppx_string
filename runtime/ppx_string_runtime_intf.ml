(** Signature for runtime implementations of Ppx_string's backend.

    May be used for derived ppxes using different types or modified behavior. Types [t],
    [conversion], and [length] should be erased using destructive substitution, i.e. [:=].
    Otherwise they introduce new aliases for the types in question, and error messages or
    Merlin may start referring to them. *)
module type S = sig
  (** Result type of interpolation, and of interpolated %{values}. *)
  type t

  (** Result type of %{converted#String} interpolated values. This will often be either
      [string] or [t], depending on what is convenient for the configured ppx. *)
  type conversion

  (** Type of length values for %{padding#:8}. *)
  type length

  (** Empty string. *)
  val empty : t

  (** Literal string. *)
  val of_string : string -> t

  (** Finish a conversion to [t]. *)
  val convert : conversion -> t

  (** Combine multiple values in order. *)
  val concat : t list -> t

  (** Pad to some minimum length. *)
  val pad : t -> len:length -> t

  (** Identity function.

      Used for ensuring an argument has type [t] in expanded code, without needing the
      type [t] to be exported explicitly for a type annotation. See note above about
      destructive substitution. *)
  external identity : t -> t = "%identity"
end

module type Ppx_string_runtime = sig
  module type S = S

  module For_string :
    S with type t := string and type length := int and type conversion := string
end
