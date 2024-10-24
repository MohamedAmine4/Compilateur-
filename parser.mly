/*
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
 */

%{

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

%}

%token EOF

/* keywords */
%token DIMENSIONS

%token END
%token OF

/* symbols */

 
%token ASSIGN
%token COMMA
%token LBRACKET RBRACKET
%token LPARENT RPARENT
%token DOT_DOT
%token DOT
%token DIVISION
%token MODULO
%token MULT
%token PLUS
%token MINUS
%token IF THEN ELSE ELSIF
%token FOR
%token EQ NEQ LT LEQ GT GEQ

/* values */
%token <string> ID
%token<int> INT
%token <string> ID


%start program
%type<Ast.prog> program

%%

program: INT DIMENSIONS OF config END opt_statements EOF
	{
		if $1 != 2 then error "only 2 dimension accepted";
		($4, $6)
	}
;

config:
	INT DOT_DOT INT
		{
			if $1 >= $3 then error "illegal field values";
			[("", (0, ($1, $3)))]
		}
|	fields
		{ set_fields $1 }
;

fields:
	field
		{ [$1] }
|	fields COMMA field
		{$3 :: $1 }
;

field:
	ID OF INT DOT_DOT INT
		{
			if $3 >= $5 then error "illegal field values";
			($1, ($3, $5))
		}
;

opt_statements:
	/* empty */
		{ NOP }
|	statement_list   /* pour supporter plusieur instruction */
		{ $1 }
;
statement_list:
  statement
  { $1 }
| statement_list statement
  { SEQ ($1,$2)}
;


statement:
	cell ASSIGN expression
		{
			if (fst $1) != 0 then error "assigned x must be 0";
			if (snd $1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, $3)
		}
|  ID ASSIGN expression    /* affectation des variables */
  {    
    if get_var $1 = -1 then
        let reg = declare_var $1 in
        SET_VAR (reg, $3)
    else
        SET_VAR (get_var $1, $3) 
  }
| IF cond THEN statement_list elsif_list END
  {
    IF_THEN($2,$4,$5);
  
  }
| FOR for_t ID statement_list END 
  {
    FOR($2,get_var $3 ,$4)
  }
;

cell:
	LBRACKET INT COMMA INT RBRACKET
		{
			if ($2 < -1) || ($2 > 1) then error "x out of range";
			if ($4 < -1) || ($4 > 1) then error "x out of range";
			($2, $4)
		}
;
expression:
  expression PLUS term
    {BINOP(OP_ADD,$1,$3)}
  /*  { printf "+\n";$1}*/
| expression MINUS term
    {BINOP(OP_SUB,$1,$3)}
    /*{ printf "-\n";$1 }*/
| term
    { $1 }
;

term:
  term MULT factor
    {BINOP(OP_MUL,$1,$3)}
    /*{ printf "*\n";$1}*/
| term DIVISION factor
    {BINOP(OP_DIV,$1,$3)}
    /*{ printf "/\n"; $1}*/
| term MODULO factor
    {BINOP(OP_MOD,$1,$3)}
    /*{ printf "%%\n"; $1}*/
| factor
    { $1 }
;

factor:

  cell
    {CELL (0, fst $1, snd $1)}
  /*  {printf "[%d,%d]\n"(fst $1) (snd $1); CELL (0, fst $1, snd $1) }*/
| INT
    {CST $1}
  /*  { printf "%d\n" $1;CST $1 }*/
| ID 
    {
      let reg = get_var $1 in 
  
      VAR reg
    }
  /* {si on veut qu'on affiche un message d'erreur quand le variable n'est pas declarer mettre if en commantaire}*/
  /*  { printf "%s\n" $1;NONE }*/

| LPARENT expression RPARENT
    { $2 }
| sign
    { $1 }
;

sign:
 PLUS factor
    {$2}
| MINUS factor
    {NEG ($2)}
  /*  { printf "NEG\n"; $2 }*/
  ;

cond:
  expression EQ expression  { COMP(COMP_EQ, $1, $3) }
| expression NEQ expression { COMP(COMP_NE, $1, $3) }
| expression LT expression  { COMP(COMP_LT, $1, $3) }
| expression LEQ expression { COMP(COMP_LE, $1, $3) }
| expression GT expression  { COMP(COMP_GT, $1, $3) }
| expression GEQ expression { COMP(COMP_GE, $1, $3) }


elsif_list:
  | ELSIF cond THEN statement_list elsif_list
    { IF_THEN($2, $4, $5) }
  | ELSE statement_list
    { $2 }
  | /* empty */
    { NOP }
;
