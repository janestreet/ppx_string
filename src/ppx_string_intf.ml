open! Base
open Ppxlib

module type Ppx_string = sig
  module Part : sig
    module Interpreted : sig
      type t =
        { loc_start : position
        ; value : expression
        ; module_path : longident_loc option
        ; pad_length : expression option
        ; loc_end : position
        ; interpreted_string : string
            (** [interpreted_string] is the string of the interpreted part. (e.g. in the example
            %{foo#Foo}, the string is "foo#Foo") *)
        }

      val to_expression : t -> expression
    end

    type t =
      | Literal of label loc
      | Interpreted of Interpreted.t
  end

  module Parse_result : sig
    type t =
      { parts : Part.t list
      ; locations_are_precise : bool
      }
  end

  val parse_parts
    :  string_loc:location
    -> delimiter:label option
    -> label
    -> Parse_result.t

  val expand
    :  expr_loc:location
    -> string_loc:location
    -> string:label
    -> delimiter:label option
    -> expression
end
