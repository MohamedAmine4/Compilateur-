(*
 * autocell - AutoCell compiler and viewer
 * Copyright (C) 2021  University of Toulouse, France <casse@irit.fr>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *)

open Ast
open Cell
open Quad
open Symbols

(** Variable containing the current x position. *)
let x = 0

(** Variable containing the current y position. *)
let y = 1

(** Variable containing the width of the array. *)
let w = 2

(** Variable containing the height of the array. *)
let h = 3

(** Variable containing 1! *)
let one = 4

(** Compute the position from the relative offset.
	@param x	X offset.
	@param y	Y offset.
	@return		Corresponding position. *)
let pos x y =
	match (x, y) with
	| (0, 0)	-> pCENTER
	| (0, -1)	-> pNORTH
	| (-1, -1)	-> pNORTHWEST
	| (-1, 0)	-> pWEST
	| (-1, +1)	-> pSOUTHWEST
	| (0, +1)	-> pSOUTH
	| (+1, +1)	-> pSOUTHEAST
	| (+1, 0)	-> pEAST
	| (+1, -1)	-> pNORTHEAST
	| _			-> failwith "bad offsets"



(** Compile an expression.
	@param e	Expression to compile.
	@return		(register containing the result, quads producing the result). *)
let error msg = failwith msg
let rec comp_expr e =
	match e with
	| NONE ->
		(0, [])
	| CELL (f, x, y) ->
		let v = new_reg () in
		(v, [
			INVOKE (cGET + f, v, pos x y)
		])
  | CST i ->
    let v =new_reg() in
    (v,[SETI(v,i)])
  | VAR x->
    if x = -1 then 
        error ("Variable not declared");
    (x, []) 
  | NEG x ->
    let (v,q)=comp_expr x in
    let reg =new_reg() in
    let reg_zero = new_reg() in
    (reg, q @ [SETI (reg_zero, 0); SUB (reg, reg_zero, v)]) 

  | BINOP(w,e1,e2)->
    let (v1,q1)=comp_expr e1 in
    let (v2,q2)=comp_expr e2 in
    let reg =new_reg() in
    let quad=match w with
    | OP_ADD -> ADD(reg,v1,v2)
    | OP_SUB-> SUB(reg,v1,v2)
    | OP_MOD -> MOD(reg,v1,v2)
    | OP_MUL -> MUL(reg,v1,v2)
    | OP_DIV-> DIV(reg,v1,v2)
    in
    (reg,q1 @ q2 @ [quad])
  | _ ->
		failwith "bad expression"


(** Compile a condition.
	@param c		Condition to compile.
	@param l_then	Label to branch to when the condition is true.
	@param l_else	Label to branch to when the condition is false.
	@return			Quads implementing the condition. *)
let rec comp_cond c l_then l_else =

	match c with
  | AND (e1, e2) ->
    let l_medium = new_lab() in
    let q1 = comp_cond e1 l_medium l_else in
    let q2 = comp_cond e2 l_medium l_else in
    q1 @ [LABEL l_medium] @ q2
  | OR (e1, e2) ->
      let l_medium = new_lab() in
      let q1 = comp_cond e1 l_then l_medium in
      let q2 = comp_cond e2 l_then l_else in
      q1 @ [LABEL l_medium] @ q2
  | NOT e ->
    comp_cond e l_else l_then

  | COMP (op, e1, e2) ->
      let (r1, q1) = comp_expr e1 in
      let (r2, q2) = comp_expr e2 in
      let quad = match op with
        | COMP_EQ -> GOTO_EQ (l_then, r1, r2)
        | COMP_NE -> GOTO_NE (l_then, r1, r2)
        | COMP_LT -> GOTO_LT (l_then, r1, r2)
        | COMP_LE -> GOTO_LE (l_then, r1, r2)
        | COMP_GT -> GOTO_GT (l_then, r1, r2)
        | COMP_GE -> GOTO_GE (l_then, r1, r2)
      in
      q1 @ q2 @ [quad; GOTO (l_else)]
	| _ ->
		failwith "bad condition"


(** Compile a statement.
	@param s	Statement to compile.
	@return		Quads implementing the statement. *)
let rec comp_stmt s =
	match s with
	| NOP ->
		[]
	| SEQ (s1, s2) ->
		(comp_stmt s1) @ (comp_stmt s2)
	| SET_CELL (f, e) ->
		let (v, q) = comp_expr e in
		q @ [
			INVOKE (cSET, v, f)
		]

  | SET_VAR(x,e)->
    let (v,q)=comp_expr e in 
    q @ [ SET(x,v) ]

  | IF_THEN(c, s1, s2) ->
    let l_then = new_lab() in
    let l_else = new_lab() in
    let l_end = new_lab() in

    let cond_quads = comp_cond c l_then l_else in
    let then_quads = comp_stmt s1 in
    let else_quads = comp_stmt s2 in
    cond_quads @
    [LABEL l_then] @
    then_quads @
    [GOTO l_end] @
    [LABEL l_else] @
    else_quads @
    [LABEL l_end] 
	| _ -> 
    failwith "bad instruction"

(** Compile the given application.
	@param flds		List of fields.
	@param stmt		Instructions.
	@return			List of quadss. *)	
let compile flds stmt =
	let x_lab = new_lab () in
	let y_lab = new_lab () in
	[
		INVOKE(cSIZE, w, h);
		SETI(one, 1);

		SETI(x, 0);
		LABEL x_lab;

		SETI(y, 0);
		LABEL y_lab;
		INVOKE(cMOVE, x, y)
	]
	@
	(comp_stmt stmt)
	@
	[
		ADD(y, y, one);
		GOTO_LT(y_lab, y, h);

		ADD(x, x, one);
		GOTO_LT(x_lab, x, w);
		STOP
	]
