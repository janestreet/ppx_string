open Base
open Ppxlib
open Ast_builder.Default
include Ppx_string_intf.Definitions

module Where = struct
  type t =
    | Imprecise of Location.t
    | Precise of { mutable position : position }

  let is_precise = function
    | Imprecise _ -> false
    | Precise _ -> true
  ;;

  let advance position char =
    let pos_cnum = position.pos_cnum + 1 in
    match char with
    | '\n' ->
      { position with pos_lnum = position.pos_lnum + 1; pos_bol = pos_cnum; pos_cnum }
    | _ -> { position with pos_cnum }
  ;;

  let skip t string =
    match t with
    | Imprecise _ -> ()
    | Precise at ->
      for pos = 0 to String.length string - 1 do
        at.position <- advance at.position string.[pos]
      done
  ;;

  let loc_start = function
    | Imprecise loc -> loc.loc_start
    | Precise { position } -> position
  ;;

  let loc_end = function
    | Imprecise loc -> loc.loc_end
    | Precise { position } -> position
  ;;

  let skip_with_loc t string =
    let loc_start = loc_start t in
    skip t string;
    let loc_end = loc_end t in
    { loc_ghost = true; loc_start; loc_end }
  ;;

  let has_escapes ~loc ~string ~delimiter =
    match delimiter with
    | Some _ -> false
    | None ->
      let unescaped_len = 1 + String.length string + 1 in
      let actual_len = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
      unescaped_len <> actual_len
  ;;

  let literal_prefix ~delimiter =
    match delimiter with
    | None -> "\""
    | Some id -> Printf.sprintf "{%s|" id
  ;;

  let create ~loc ~string ~delimiter ~preprocess_before_parsing =
    if Option.is_some preprocess_before_parsing || has_escapes ~loc ~string ~delimiter
    then Imprecise { loc with loc_ghost = true }
    else (
      let t = Precise { position = loc.loc_start } in
      skip t (literal_prefix ~delimiter);
      t)
  ;;
end

let dot id name = pexp_ident ~loc:id.loc { id with txt = Ldot (id.txt, name) }

let config_expr ~(config : Config.t) ~loc name =
  dot { loc; txt = config.fully_qualified_runtime_module } name
;;

let interpret
  ~(config : Config.t)
  ({ loc_start; value; module_path; pad_length; loc_end; interpreted_string = _ } :
    Part.Interpreted.t)
  =
  let loc = { loc_ghost = true; loc_start; loc_end } in
  let unpadded =
    match module_path with
    | None -> value
    | Some fn ->
      [%expr
        [%e config_expr ~config ~loc "convert"]
          ([%e dot fn config.conversion_function_name] [%e value])]
  in
  match pad_length with
  | None -> unpadded
  | Some len -> [%expr [%e config_expr ~config ~loc "pad"] [%e unpadded] ~len:[%e len]]
;;

let parse_literal string ~where ~start ~until ~acc =
  if start >= until
  then acc
  else (
    let literal = String.sub string ~pos:start ~len:(until - start) in
    let loc = Where.skip_with_loc where literal in
    Part.Literal { txt = literal; loc } :: acc)
;;

let set_locs loc =
  object
    inherit Ast_traverse.map
    method! location _ = loc
  end
;;

let parse_error ~loc ~name string =
  Location.raise_errorf ~loc "invalid %s: %S" name string
;;

let parse_expression ~where ~loc ~name string =
  let lexbuf = Lexing.from_string string in
  lexbuf.lex_abs_pos <- loc.loc_start.pos_cnum;
  lexbuf.lex_curr_p <- loc.loc_start;
  match Parse.expression lexbuf with
  | exception _ -> parse_error ~loc ~name string
  | expr -> if Where.is_precise where then expr else (set_locs loc)#expression expr
;;

let parse_ident ~where ~loc ~name module_path =
  match parse_expression ~where ~loc ~name module_path with
  | { pexp_desc = Pexp_construct (ident, None); _ } -> ident
  | _ -> parse_error ~loc ~name module_path
;;

let parse_body ~where string =
  let loc = Where.skip_with_loc where string in
  parse_expression ~where ~loc ~name:"%{...} expression" string
;;

let parse_module_path ~where string =
  let loc = Where.skip_with_loc where string in
  parse_ident ~where ~loc ~name:"%{...} module path" string
;;

let parse_pad_length ~where string =
  let loc = Where.skip_with_loc where string in
  parse_expression ~where ~loc ~name:"%{...} pad length" string
;;

let parse_interpreted string ~where ~start ~until ~acc =
  Where.skip where "%{";
  let loc_start = Where.loc_start where in
  let string = String.sub string ~pos:start ~len:(until - start) in
  let value, module_path, pad_length =
    match String.rsplit2 string ~on:'#' with
    | None ->
      let value = parse_body ~where string in
      value, None, None
    | Some (body, formatting) ->
      let body = parse_body ~where body in
      Where.skip where "#";
      let module_path, pad_length =
        match String.rsplit2 formatting ~on:':' with
        | None ->
          let fn = parse_module_path ~where formatting in
          Some fn, None
        | Some (module_path, pad_length) ->
          let fn =
            if String.is_empty module_path
            then None
            else Some (parse_module_path ~where module_path)
          in
          Where.skip where ":";
          let len = parse_pad_length ~where pad_length in
          fn, Some len
      in
      body, module_path, pad_length
  in
  let loc_end = Where.loc_end where in
  Where.skip where "}";
  Part.Interpreted
    { loc_start; value; module_path; pad_length; loc_end; interpreted_string = string }
  :: acc
;;

type interpreted =
  { percent : int
  ; lbrace : int
  ; rbrace : int
  }

let find_interpreted string ~where ~pos =
  String.substr_index string ~pos ~pattern:"%{"
  |> Option.map ~f:(fun percent ->
       let lbrace = percent + 1 in
       match String.substr_index string ~pos:(lbrace + 1) ~pattern:"}" with
       | None ->
         Where.skip where (String.sub string ~pos ~len:(percent - pos));
         let loc = Where.skip_with_loc where "%{" in
         Location.raise_errorf ~loc "unterminated %%{"
       | Some rbrace -> { percent; lbrace; rbrace })
;;

let rec parse_from string ~where ~pos ~acc =
  match find_interpreted string ~where ~pos with
  | None ->
    let len = String.length string in
    let acc = parse_literal string ~where ~start:pos ~until:len ~acc in
    List.rev acc
  | Some { percent; lbrace; rbrace } ->
    let acc = parse_literal string ~where ~start:pos ~until:percent ~acc in
    let acc = parse_interpreted string ~where ~start:(lbrace + 1) ~until:rbrace ~acc in
    parse_from string ~where ~pos:(rbrace + 1) ~acc
;;

let parse ~(config : Config.t) ~string_loc ~delimiter string =
  let preprocess_before_parsing = config.preprocess_before_parsing in
  let string =
    match preprocess_before_parsing with
    | None -> string
    | Some preprocess -> preprocess string
  in
  let where =
    Where.create ~loc:string_loc ~delimiter ~string ~preprocess_before_parsing
  in
  let parts = parse_from string ~where ~pos:0 ~acc:[] in
  let locations_are_precise = Where.is_precise where in
  ({ parts; locations_are_precise } : Parse_result.t)
;;

let expand_part_to_expression ~config part =
  match (part : Part.t) with
  | Literal { txt; loc } ->
    [%expr [%e config_expr ~config ~loc "of_string"] [%e estring txt ~loc]]
  | Interpreted interpreted -> interpret ~config interpreted
;;

let concatenate ~config ~loc expressions =
  match expressions with
  | [] -> [%expr [%e config_expr ~config ~loc "empty"]]
  | [ expr ] -> [%expr [%e config_expr ~config ~loc "identity"] [%e expr]]
  | multiple -> [%expr [%e config_expr ~config ~loc "concat"] [%e elist ~loc multiple]]
;;

let expand ~config ~expr_loc ~string_loc ~string ~delimiter =
  (parse ~config ~string_loc ~delimiter string).parts
  |> List.map ~f:(expand_part_to_expression ~config)
  |> concatenate ~config ~loc:expr_loc
;;

let extension ~name ~(config : Config.t) =
  Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(pstr (pstr_eval (pexp_constant (pconst_string __' __ __)) nil ^:: nil))
    (fun ~loc:expr_loc ~path:_ { loc = string_loc; txt = string } _ delimiter ->
      Merlin_helpers.hide_expression
        (expand ~config ~expr_loc ~string_loc ~string ~delimiter))
;;

let (config_for_string : Config.t) =
  { fully_qualified_runtime_module = Ldot (Lident "Ppx_string_runtime", "For_string")
  ; conversion_function_name = "to_string"
  ; preprocess_before_parsing = None
  }
;;

let () =
  Ppxlib.Driver.register_transformation
    "ppx_string"
    ~extensions:[ extension ~name:"ppx_string.string" ~config:config_for_string ]
;;
