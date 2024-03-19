open! Base
open Ppxlib

module Definitions = struct
  (** Used to configure different instances of this ppx. May be used, for example, to add
      preprocessing, or to interpolate a different string-like type. *)
  module Config = struct
    type t =
      { fully_qualified_runtime_module : Longident.t
          (** Where to find an implementation of [Ppx_string_runtime.S]. The implementation of
          [[%string]] is at [Ldot (Lident "Ppx_string_runtime", "For_string")] *)
      ; conversion_function_name : string
          (** Conversion function implied by ["%{expr#Module}"], e.g. ["to_string"]. *)
      ; preprocess_before_parsing : (string -> string) option
          (** Preprocessing to apply before parsing the string for interpolation. If [None],
          source locations can be computed precisely based on the result of parsing. *)
      }
  end

  module Part = struct
    module Interpreted = struct
      type t =
        { loc_start : position
        ; value : expression
        ; module_path : longident_loc option
        ; pad_length : expression option
        ; loc_end : position
        ; interpreted_string : string
            (** [interpreted_string] is the string of the interpreted part. (e.g. in the
            example %{foo#Foo}, the string is "foo#Foo") *)
        }
    end

    type t =
      | Literal of string loc
      | Interpreted of Interpreted.t
  end

  module Parse_result = struct
    type t =
      { parts : Part.t list
      ; locations_are_precise : bool
      }
  end
end

module type Ppx_string = sig
  include module type of struct
    include Definitions
  end

  (** Parse a string to find interpolated substrings. *)
  val parse
    :  config:Config.t
    -> string_loc:location
    -> delimiter:string option
    -> string
    -> Parse_result.t

  (** Interpret an interpolated string as an expression, including %{conversions#String}
      and %{padding#:8}. *)
  val interpret : config:Config.t -> Part.Interpreted.t -> expression

  (** Combines [parse], [interpret], and concatenation to expand an interpolated string to
      an expression implementing it. *)
  val expand
    :  config:Config.t
    -> expr_loc:location
    -> string_loc:location
    -> string:string
    -> delimiter:string option
    -> expression

  (** Construct an [Extension.t] implementing the configured interpolation ppx. *)
  val extension : name:string -> config:Config.t -> Extension.t

  (** Configuration for [[%string]]: string type and conversion type are [string], length
      type is [int], and no preprocessing. *)
  val config_for_string : Config.t
end
