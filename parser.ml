type token =
  | EOF
  | DIMENSIONS
  | END
  | OF
  | ASSIGN
  | COMMA
  | LBRACKET
  | RBRACKET
  | LPARENT
  | RPARENT
  | DOT_DOT
  | DOT
  | DIVISION
  | MODULO
  | MULT
  | PLUS
  | MINUS
  | IF
  | THEN
  | ELSE
  | ELSIF
  | NOT
  | AND
  | OR
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | ID of (
# 71 "parser.mly"
        string
# 36 "parser.ml"
)
  | INT of (
# 70 "parser.mly"
       int
# 41 "parser.ml"
)

open Parsing
let _ = parse_error;;
# 17 "parser.mly"

open Common
open Ast
open Printf
open Symbols

(** Raise a syntax error with the given message.
	@param msg	Message of the error. *)
let error msg =
	raise (SyntaxError msg)


(** Restructure the when assignment into selections.
	@param f	Function to build the assignment.
	@param ws	Sequence of (expression, conditions) terminated
				by (expression, NO_COND).
	@return		Built statement. *)
let rec make_when f ws =
	match ws with
	| [(e, NO_COND)]	->	f e
	| (e, c)::t			-> IF_THEN(c, f e, make_when f t)
	| _ -> failwith "whens list not ended by (expression, NO_COND)."

# 70 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* DIMENSIONS *);
  258 (* END *);
  259 (* OF *);
  260 (* ASSIGN *);
  261 (* COMMA *);
  262 (* LBRACKET *);
  263 (* RBRACKET *);
  264 (* LPARENT *);
  265 (* RPARENT *);
  266 (* DOT_DOT *);
  267 (* DOT *);
  268 (* DIVISION *);
  269 (* MODULO *);
  270 (* MULT *);
  271 (* PLUS *);
  272 (* MINUS *);
  273 (* IF *);
  274 (* THEN *);
  275 (* ELSE *);
  276 (* ELSIF *);
  277 (* NOT *);
  278 (* AND *);
  279 (* OR *);
  280 (* EQ *);
  281 (* NEQ *);
  282 (* LT *);
  283 (* LEQ *);
  284 (* GT *);
  285 (* GEQ *);
    0|]

let yytransl_block = [|
  286 (* ID *);
  287 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\003\000\003\000\
\006\000\006\000\007\000\007\000\007\000\008\000\009\000\009\000\
\009\000\012\000\012\000\012\000\012\000\013\000\013\000\013\000\
\013\000\013\000\014\000\014\000\011\000\011\000\011\000\010\000\
\015\000\015\000\016\000\016\000\017\000\017\000\017\000\018\000\
\018\000\018\000\018\000\018\000\018\000\000\000"

let yylen = "\002\000\
\007\000\003\000\001\000\001\000\003\000\005\000\000\000\001\000\
\001\000\002\000\003\000\003\000\006\000\005\000\003\000\003\000\
\001\000\003\000\003\000\003\000\001\000\001\000\001\000\001\000\
\003\000\001\000\002\000\002\000\005\000\002\000\000\000\001\000\
\003\000\001\000\003\000\001\000\002\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\046\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\009\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\023\000\022\000\000\000\000\000\000\000\021\000\026\000\000\000\
\000\000\036\000\038\000\000\000\001\000\010\000\000\000\006\000\
\000\000\000\000\000\000\000\000\027\000\028\000\037\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\025\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\020\000\018\000\000\000\
\035\000\014\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\029\000"

let yydgoto = "\002\000\
\004\000\009\000\021\000\010\000\011\000\022\000\023\000\034\000\
\035\000\036\000\093\000\037\000\038\000\039\000\040\000\041\000\
\042\000\043\000"

let yysindex = "\006\000\
\238\254\000\000\015\255\000\000\021\255\252\254\026\255\022\255\
\029\255\031\255\000\000\008\255\038\255\016\255\030\255\071\255\
\000\000\058\255\004\255\088\255\105\000\016\255\000\000\103\255\
\000\000\092\255\121\255\004\255\060\255\060\255\004\255\000\000\
\000\000\000\000\085\255\110\255\090\255\000\000\000\000\113\255\
\120\255\000\000\000\000\060\255\000\000\000\000\060\255\000\000\
\112\255\070\255\132\255\060\255\000\000\000\000\000\000\060\255\
\060\255\060\255\060\255\060\255\060\255\060\255\060\255\016\255\
\060\255\060\255\060\255\004\255\004\255\119\255\119\255\137\255\
\000\000\000\000\056\255\090\255\090\255\119\255\119\255\119\255\
\119\255\119\255\119\255\254\254\000\000\000\000\000\000\120\255\
\000\000\000\000\016\255\004\255\143\255\016\255\128\255\000\000\
\016\255\254\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\145\255\000\000\000\000\000\000\148\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\149\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\005\255\
\040\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\088\000\093\000\000\000\
\000\000\000\000\000\000\030\000\059\000\039\255\097\255\099\255\
\107\255\109\255\115\255\148\255\000\000\000\000\000\000\064\255\
\000\000\000\000\000\000\000\000\000\000\149\255\000\000\000\000\
\000\000\148\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\137\000\203\255\236\255\242\255\
\249\255\231\255\055\000\083\000\232\255\000\000\000\000\086\000\
\234\255\000\000"

let yytablesize = 379
let yytable = "\024\000\
\017\000\046\000\051\000\018\000\053\000\054\000\001\000\024\000\
\055\000\018\000\084\000\028\000\003\000\032\000\019\000\005\000\
\091\000\092\000\029\000\030\000\050\000\018\000\032\000\006\000\
\031\000\007\000\008\000\020\000\012\000\015\000\014\000\013\000\
\019\000\032\000\033\000\015\000\070\000\094\000\016\000\071\000\
\085\000\086\000\087\000\098\000\075\000\020\000\089\000\040\000\
\034\000\024\000\078\000\079\000\080\000\081\000\082\000\083\000\
\040\000\034\000\016\000\007\000\040\000\040\000\034\000\046\000\
\073\000\018\000\095\000\052\000\017\000\024\000\056\000\057\000\
\033\000\046\000\029\000\030\000\024\000\046\000\073\000\024\000\
\026\000\033\000\024\000\024\000\056\000\057\000\033\000\012\000\
\027\000\032\000\033\000\044\000\011\000\058\000\059\000\060\000\
\061\000\062\000\063\000\056\000\057\000\065\000\066\000\067\000\
\045\000\041\000\047\000\042\000\058\000\059\000\060\000\061\000\
\062\000\063\000\041\000\043\000\042\000\044\000\041\000\041\000\
\042\000\042\000\048\000\045\000\043\000\049\000\044\000\064\000\
\043\000\043\000\044\000\044\000\045\000\056\000\057\000\068\000\
\045\000\045\000\076\000\077\000\074\000\069\000\072\000\090\000\
\096\000\097\000\003\000\007\000\008\000\031\000\030\000\025\000\
\099\000\088\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\017\000\000\000\000\000\000\000\017\000\000\000\
\000\000\017\000\000\000\000\000\000\000\000\000\000\000\017\000\
\017\000\017\000\017\000\017\000\017\000\000\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\015\000\
\000\000\000\000\000\000\015\000\000\000\000\000\015\000\000\000\
\000\000\000\000\000\000\000\000\015\000\015\000\015\000\015\000\
\015\000\015\000\000\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\016\000\000\000\000\000\000\000\
\016\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\
\000\000\016\000\016\000\016\000\016\000\016\000\016\000\000\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\012\000\000\000\000\000\000\000\012\000\011\000\000\000\
\000\000\000\000\011\000\000\000\000\000\000\000\000\000\000\000\
\012\000\000\000\012\000\012\000\000\000\011\000\000\000\011\000\
\011\000\000\000\000\000\000\000\000\000\012\000\000\000\000\000\
\000\000\000\000\011\000"

let yycheck = "\014\000\
\000\000\022\000\028\000\006\001\029\000\030\000\001\000\022\000\
\031\000\006\001\064\000\008\001\031\001\009\001\017\001\001\001\
\019\001\020\001\015\001\016\001\028\000\006\001\018\001\003\001\
\021\001\030\001\031\001\030\001\003\001\000\000\002\001\010\001\
\017\001\030\001\031\001\005\001\044\000\091\000\031\001\047\000\
\065\000\066\000\067\000\097\000\052\000\030\001\069\000\009\001\
\009\001\064\000\058\000\059\000\060\000\061\000\062\000\063\000\
\018\001\018\001\000\000\030\001\022\001\023\001\023\001\084\000\
\009\001\006\001\092\000\008\001\031\001\084\000\015\001\016\001\
\009\001\094\000\015\001\016\001\091\000\098\000\009\001\094\000\
\010\001\018\001\097\000\098\000\015\001\016\001\023\001\000\000\
\031\001\030\001\031\001\004\001\000\000\024\001\025\001\026\001\
\027\001\028\001\029\001\015\001\016\001\012\001\013\001\014\001\
\000\000\009\001\004\001\009\001\024\001\025\001\026\001\027\001\
\028\001\029\001\018\001\009\001\018\001\009\001\022\001\023\001\
\022\001\023\001\031\001\009\001\018\001\005\001\018\001\018\001\
\022\001\023\001\022\001\023\001\018\001\015\001\016\001\023\001\
\022\001\023\001\056\000\057\000\009\001\022\001\031\001\007\001\
\002\001\018\001\002\001\000\000\000\000\002\001\002\001\015\000\
\098\000\068\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\255\255\255\255\006\001\255\255\
\255\255\009\001\255\255\255\255\255\255\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\002\001\
\255\255\255\255\255\255\006\001\255\255\255\255\009\001\255\255\
\255\255\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\002\001\255\255\255\255\255\255\
\006\001\255\255\255\255\009\001\255\255\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\002\001\255\255\255\255\255\255\006\001\002\001\255\255\
\255\255\255\255\006\001\255\255\255\255\255\255\255\255\255\255\
\017\001\255\255\019\001\020\001\255\255\017\001\255\255\019\001\
\020\001\255\255\255\255\255\255\255\255\030\001\255\255\255\255\
\255\255\255\255\030\001"

let yynames_const = "\
  EOF\000\
  DIMENSIONS\000\
  END\000\
  OF\000\
  ASSIGN\000\
  COMMA\000\
  LBRACKET\000\
  RBRACKET\000\
  LPARENT\000\
  RPARENT\000\
  DOT_DOT\000\
  DOT\000\
  DIVISION\000\
  MODULO\000\
  MULT\000\
  PLUS\000\
  MINUS\000\
  IF\000\
  THEN\000\
  ELSE\000\
  ELSIF\000\
  NOT\000\
  AND\000\
  OR\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  "

let yynames_block = "\
  ID\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'config) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 80 "parser.mly"
 (
		if _1 != 2 then error "only 2 dimension accepted";
		(_4, _6)
	)
# 331 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 88 "parser.mly"
  (
			if _1 >= _3 then error "illegal field values";
			[("", (0, (_1, _3)))]
		)
# 342 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 93 "parser.mly"
  ( set_fields _1 )
# 349 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 98 "parser.mly"
  ( [_1] )
# 356 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 100 "parser.mly"
  (_3 :: _1 )
# 364 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 105 "parser.mly"
  (
			if _3 >= _5 then error "illegal field values";
			(_1, (_3, _5))
		)
# 376 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
  ( NOP )
# 382 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statement_list) in
    Obj.repr(
# 115 "parser.mly"
  ( _1 )
# 389 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 119 "parser.mly"
  ( _1 )
# 396 "parser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 121 "parser.mly"
  ( SEQ (_1,_2))
# 404 "parser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 127 "parser.mly"
  (
			if (fst _1) != 0 then error "assigned x must be 0";
			if (snd _1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, _3)
		)
# 416 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 133 "parser.mly"
  (    
    if get_var _1 = -1 then
        let reg = declare_var _1 in
        SET_VAR (reg, _3)
    else
        SET_VAR (get_var _1, _3) 
  )
# 430 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'statement_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'elsif_list) in
    Obj.repr(
# 141 "parser.mly"
  (
    IF_THEN(_2,_4,_5);
  
  )
# 442 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 148 "parser.mly"
  (
			if (_2 < -1) || (_2 > 1) then error "x out of range";
			if (_4 < -1) || (_4 > 1) then error "x out of range";
			(_2, _4)
		)
# 454 "parser.ml"
               : 'cell))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 156 "parser.mly"
    (BINOP(OP_ADD,_1,_3))
# 462 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 159 "parser.mly"
    (BINOP(OP_SUB,_1,_3))
# 470 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 162 "parser.mly"
    ( _1 )
# 477 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 167 "parser.mly"
    (BINOP(OP_MUL,_1,_3))
# 485 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 170 "parser.mly"
    (BINOP(OP_DIV,_1,_3))
# 493 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 173 "parser.mly"
    (BINOP(OP_MOD,_1,_3))
# 501 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 176 "parser.mly"
    ( _1 )
# 508 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 182 "parser.mly"
    (CELL (0, fst _1, snd _1))
# 515 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 185 "parser.mly"
    (CST _1)
# 522 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 188 "parser.mly"
    (
      let reg = get_var _1 in 
  
      VAR reg
    )
# 533 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 197 "parser.mly"
    ( _2 )
# 540 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sign) in
    Obj.repr(
# 199 "parser.mly"
    ( _1 )
# 547 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 204 "parser.mly"
    (_2)
# 554 "parser.ml"
               : 'sign))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 206 "parser.mly"
    (NEG (_2))
# 561 "parser.ml"
               : 'sign))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'elsif_list) in
    Obj.repr(
# 212 "parser.mly"
      ( IF_THEN(_2, _4, _5) )
# 570 "parser.ml"
               : 'elsif_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement_list) in
    Obj.repr(
# 214 "parser.mly"
    ( _2 )
# 577 "parser.ml"
               : 'elsif_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 216 "parser.mly"
    ( NOP )
# 583 "parser.ml"
               : 'elsif_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'condion_OR) in
    Obj.repr(
# 220 "parser.mly"
               ( _1 )
# 590 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'condion_OR) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'condition_AND) in
    Obj.repr(
# 224 "parser.mly"
                                ( OR(_1, _3) )
# 598 "parser.ml"
               : 'condion_OR))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'condition_AND) in
    Obj.repr(
# 225 "parser.mly"
                              ( _1 )
# 605 "parser.ml"
               : 'condion_OR))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'condition_AND) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'condition_NOT) in
    Obj.repr(
# 229 "parser.mly"
                                    ( AND (_1, _3) )
# 613 "parser.ml"
               : 'condition_AND))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'condition_NOT) in
    Obj.repr(
# 230 "parser.mly"
                               ( _1 )
# 620 "parser.ml"
               : 'condition_AND))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'condition_NOT) in
    Obj.repr(
# 234 "parser.mly"
                      ( NOT(_2) )
# 627 "parser.ml"
               : 'condition_NOT))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_condition) in
    Obj.repr(
# 235 "parser.mly"
                      ( _1 )
# 634 "parser.ml"
               : 'condition_NOT))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cond) in
    Obj.repr(
# 236 "parser.mly"
                         ( _2 )
# 641 "parser.ml"
               : 'condition_NOT))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 240 "parser.mly"
                              ( COMP(COMP_EQ, _1, _3) )
# 649 "parser.ml"
               : 'simple_condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 241 "parser.mly"
                              ( COMP(COMP_NE, _1, _3) )
# 657 "parser.ml"
               : 'simple_condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 242 "parser.mly"
                              ( COMP(COMP_LT, _1, _3) )
# 665 "parser.ml"
               : 'simple_condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 243 "parser.mly"
                              ( COMP(COMP_LE, _1, _3) )
# 673 "parser.ml"
               : 'simple_condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 244 "parser.mly"
                              ( COMP(COMP_GT, _1, _3) )
# 681 "parser.ml"
               : 'simple_condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 245 "parser.mly"
                              ( COMP(COMP_GE, _1, _3) )
# 689 "parser.ml"
               : 'simple_condition))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
