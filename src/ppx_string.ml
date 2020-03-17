open Base
open Ppxlib
open Ast_builder.Default

let parse_expression ~loc_start string =
  let lexbuf = Lexing.from_string string in
  lexbuf.lex_abs_pos <- loc_start.pos_cnum;
  lexbuf.lex_curr_p <- loc_start;
  Ppxlib.Parse.expression lexbuf
;;

let parse_expression_shifted ~loc ~shift string =
  (* These shifts are not correct in the presence of newlines in the string literal and
     escapes (1 character in ast string does not correspond to +1 columns in the source),
     but we're still closer than without correcting. *)
  parse_expression
    ~loc_start:{ loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + shift }
    string
;;

let explode_str ({ txt = s; loc } : string Located.t) =
  let push_substr acc start end_ =
    if start = end_
    then acc
    else estring ~loc (String.sub s ~pos:start ~len:(end_ - start)) :: acc
  in
  let rec loop acc pos =
    match String.substr_index ~pos s ~pattern:"%{" with
    | None -> List.rev (push_substr acc pos (String.length s))
    | Some start ->
      let acc = push_substr acc pos start in
      let pos = start + 2 in
      let end_ =
        match String.index_from s pos '}' with
        | None -> Location.raise_errorf ~loc "unterminated %%{"
        | Some end_ -> end_
      in
      let string_expr =
        let unparsed_subexpr = String.sub s ~pos ~len:(end_ - pos) in
        match String.rsplit2 ~on:'#' unparsed_subexpr with
        | None -> parse_expression_shifted ~loc ~shift:pos unparsed_subexpr
        | Some (arg, conversion_module) ->
          let arg = parse_expression_shifted ~loc ~shift:pos arg
          and conversion_module =
            parse_expression_shifted
              ~loc
              ~shift:(pos + String.length arg + 1)
              (conversion_module ^ ".to_string")
          in
          eapply ~loc conversion_module [ arg ]
      in
      loop (string_expr :: acc) (end_ + 1)
  in
  loop [] 0
;;

let concat ~loc = function
  | [] -> estring ~loc ""
  | [ x ] -> x
  | _ :: _ :: _ as l ->
    eapply ~loc (evar ~loc "Stdlib.String.concat") [ estring ~loc ""; elist ~loc l ]
;;

let () =
  Ppxlib.Driver.register_transformation
    "ppx_string"
    ~extensions:
      [ Extension.declare
          "ppx_string.string"
          Extension.Context.expression
          Ast_pattern.(pstr (pstr_eval (estring __') nil ^:: nil))
          (fun ~loc:_ ~path:_ sym ->
             Merlin_helpers.hide_expression (concat ~loc:sym.loc (explode_str sym)))
      ]
;;
