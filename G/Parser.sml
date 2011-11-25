local
type t__1__ = (int*int)
type t__2__ = string*(int*int)
type t__3__ = (int*int)
type t__4__ = (int*int)
type t__5__ = (int*int)
type t__6__ = string*(int*int)
type t__7__ = (int*int)
type t__8__ = (int*int)
type t__9__ = (int*int)
type t__10__ = (int*int)
type t__11__ = (int*int)
type t__12__ = int*(int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = (int*int)
type t__16__ = (int*int)
type t__17__ = string*(int*int)
type t__18__ = (int*int)
in
datatype token =
    ASSIGN of t__1__
  | CHAR of t__2__
  | COMMA of t__3__
  | ELSE of t__4__
  | EOF of t__5__
  | ID of t__6__
  | IF of t__7__
  | INT of t__8__
  | LESS of t__9__
  | LPAR of t__10__
  | MINUS of t__11__
  | NUM of t__12__
  | PLUS of t__13__
  | RETURN of t__14__
  | RPAR of t__15__
  | SEMICOLON of t__16__
  | STRING of t__17__
  | THEN of t__18__
end;

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

val yytransl = #[
  257 (* ASSIGN *),
  258 (* CHAR *),
  259 (* COMMA *),
  260 (* ELSE *),
  261 (* EOF *),
  262 (* ID *),
  263 (* IF *),
  264 (* INT *),
  265 (* LESS *),
  266 (* LPAR *),
  267 (* MINUS *),
  268 (* NUM *),
  269 (* PLUS *),
  270 (* RETURN *),
  271 (* RPAR *),
  272 (* SEMICOLON *),
  273 (* STRING *),
  274 (* THEN *),
    0];

val yylhs = "\255\255\
\\001\000\002\000\002\000\003\000\007\000\007\000\008\000\008\000\
\\006\000\004\000\005\000\005\000\013\000\013\000\013\000\013\000\
\\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\\010\000\010\000\011\000\011\000\012\000\000\000";

val yylen = "\002\000\
\\002\000\007\000\000\000\001\000\002\000\000\000\003\000\000\000\
\\002\000\001\000\003\000\001\000\002\000\005\000\007\000\003\000\
\\001\000\001\000\003\000\003\000\003\000\003\000\004\000\003\000\
\\000\000\001\000\001\000\003\000\001\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\004\000\030\000\000\000\000\000\001\000\010\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\017\000\000\000\000\000\000\000\000\000\000\000\
\\009\000\007\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\013\000\000\000\002\000\000\000\000\000\000\000\026\000\
\\000\000\024\000\016\000\000\000\000\000\000\000\000\000\011\000\
\\000\000\023\000\000\000\028\000\000\000\000\000\015\000";

val yydgoto = "\002\000\
\\004\000\005\000\006\000\024\000\025\000\015\000\011\000\012\000\
\\021\000\039\000\040\000\022\000\023\000";

val yysindex = "\005\000\
\\014\255\000\000\000\000\000\000\011\255\039\255\000\000\000\000\
\\046\255\000\000\034\255\014\255\056\255\039\255\038\255\076\255\
\\080\255\015\255\000\000\015\255\248\254\090\255\014\255\089\255\
\\000\000\000\000\015\255\015\255\069\255\063\255\015\255\015\255\
\\015\255\000\000\015\255\000\000\039\255\058\255\078\255\000\000\
\\072\255\000\000\000\000\023\255\000\000\000\000\064\255\000\000\
\\015\255\000\000\056\255\000\000\091\255\056\255\000\000";

val yyrindex = "\000\000\
\\092\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\004\255\000\000\000\000\000\000\000\000\079\255\017\255\
\\000\000\000\000\000\000\000\000\000\000\026\255\092\255\073\255\
\\000\000\000\000\081\255\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\083\255\000\000\000\000\
\\000\000\000\000\000\000\049\255\035\255\044\255\008\255\000\000\
\\081\255\000\000\000\000\000\000\255\254\000\000\000\000";

val yygindex = "\000\000\
\\000\000\076\000\088\000\095\000\065\000\000\000\000\000\000\000\
\\238\255\054\000\000\000\000\000\245\255";

val YYTABLESIZE = 103;
val yytable = "\029\000\
\\031\000\030\000\032\000\014\000\033\000\001\000\014\000\034\000\
\\038\000\041\000\019\000\008\000\044\000\045\000\046\000\007\000\
\\047\000\029\000\006\000\029\000\016\000\003\000\019\000\019\000\
\\018\000\029\000\019\000\029\000\018\000\029\000\038\000\029\000\
\\029\000\032\000\018\000\033\000\018\000\021\000\018\000\053\000\
\\018\000\018\000\055\000\021\000\008\000\021\000\020\000\021\000\
\\013\000\021\000\021\000\022\000\020\000\026\000\020\000\010\000\
\\020\000\022\000\020\000\020\000\049\000\016\000\017\000\022\000\
\\022\000\018\000\031\000\019\000\032\000\020\000\033\000\031\000\
\\031\000\032\000\032\000\033\000\033\000\031\000\043\000\032\000\
\\031\000\033\000\032\000\042\000\033\000\027\000\051\000\012\000\
\\012\000\028\000\035\000\037\000\050\000\005\000\054\000\025\000\
\\003\000\027\000\036\000\014\000\009\000\048\000\052\000";

val yycheck = "\018\000\
\\009\001\020\000\011\001\005\001\013\001\001\000\008\001\016\001\
\\027\000\028\000\003\001\008\001\031\000\032\000\033\000\005\001\
\\035\000\001\001\015\001\003\001\006\001\008\001\015\001\016\001\
\\010\001\009\001\012\001\011\001\003\001\013\001\049\000\015\001\
\\016\001\011\001\009\001\013\001\011\001\003\001\013\001\051\000\
\\015\001\016\001\054\000\009\001\006\001\011\001\003\001\013\001\
\\015\001\015\001\016\001\003\001\009\001\016\001\011\001\010\001\
\\013\001\009\001\015\001\016\001\003\001\006\001\007\001\015\001\
\\016\001\010\001\009\001\012\001\011\001\014\001\013\001\009\001\
\\009\001\011\001\011\001\013\001\013\001\009\001\016\001\011\001\
\\009\001\013\001\011\001\015\001\013\001\010\001\015\001\015\001\
\\016\001\010\001\001\001\003\001\015\001\015\001\004\001\015\001\
\\005\001\015\001\023\000\012\000\006\000\037\000\049\000";

val yyact = vector_ 31 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file Parser.grm, line 30 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : S100.FunDec list
val d__2__ = peekVal 0 : (int*int)
in
( (d__1__) ) end : S100.Prog))
;
(* Rule 2, file Parser.grm, line 35 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 6 : S100.Type
val d__2__ = peekVal 5 : S100.Sid
val d__3__ = peekVal 4 : (int*int)
val d__4__ = peekVal 3 : S100.Dec list
val d__5__ = peekVal 2 : (int*int)
val d__6__ = peekVal 1 : S100.Stat
val d__7__ = peekVal 0 : S100.FunDec list
in
( ((d__1__), (d__2__), (d__4__), (d__6__), (d__3__)) :: (d__7__) ) end : S100.FunDec list))
;
(* Rule 3, file Parser.grm, line 36 *)
val _ = update_ yyact 3
(fn () => repr(let
in
( [] ) end : S100.FunDec list))
;
(* Rule 4, file Parser.grm, line 39 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( S100.Int (d__1__) ) end : S100.Type))
;
(* Rule 5, file Parser.grm, line 42 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 1 : S100.Dec list
val d__2__ = peekVal 0 : S100.Dec
in
( (d__1__) @ [(d__2__)] ) end : S100.Dec list))
;
(* Rule 6, file Parser.grm, line 43 *)
val _ = update_ yyact 6
(fn () => repr(let
in
( [] ) end : S100.Dec list))
;
(* Rule 7, file Parser.grm, line 47 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Dec list
val d__2__ = peekVal 1 : S100.Dec
val d__3__ = peekVal 0 : (int*int)
in
( (d__1__) @ [(d__2__)] ) end : S100.Dec list))
;
(* Rule 8, file Parser.grm, line 48 *)
val _ = update_ yyact 8
(fn () => repr(let
in
( [] ) end : S100.Dec list))
;
(* Rule 9, file Parser.grm, line 51 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__1__ = peekVal 1 : S100.Type
val d__2__ = peekVal 0 : S100.Sid list
in
( ((d__1__), (d__2__)) ) end : S100.Dec))
;
(* Rule 10, file Parser.grm, line 54 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( S100.Val (d__1__) ) end : S100.Sid))
;
(* Rule 11, file Parser.grm, line 58 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Sid
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Sid list
in
( (d__1__) :: (d__3__) ) end : S100.Sid list))
;
(* Rule 12, file Parser.grm, line 59 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 0 : S100.Sid
in
( [(d__1__)] ) end : S100.Sid list))
;
(* Rule 13, file Parser.grm, line 61 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 1 : S100.Exp
val d__2__ = peekVal 0 : (int*int)
in
( S100.EX (d__1__) ) end : S100.Stat))
;
(* Rule 14, file Parser.grm, line 63 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : (int*int)
val d__3__ = peekVal 2 : S100.Exp
val d__4__ = peekVal 1 : (int*int)
val d__5__ = peekVal 0 : S100.Stat
in
( S100.If ((d__3__),(d__5__),(d__1__)) ) end : S100.Stat))
;
(* Rule 15, file Parser.grm, line 65 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 6 : (int*int)
val d__2__ = peekVal 5 : (int*int)
val d__3__ = peekVal 4 : S100.Exp
val d__4__ = peekVal 3 : (int*int)
val d__5__ = peekVal 2 : S100.Stat
val d__6__ = peekVal 1 : (int*int)
val d__7__ = peekVal 0 : S100.Stat
in
( S100.IfElse ((d__3__),(d__5__),(d__7__),(d__1__)) ) end : S100.Stat))
;
(* Rule 16, file Parser.grm, line 67 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : S100.Exp
val d__3__ = peekVal 0 : (int*int)
in
( S100.Return ((d__2__),(d__1__)) ) end : S100.Stat))
;
(* Rule 17, file Parser.grm, line 70 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__1__ = peekVal 0 : int*(int*int)
in
( S100.NumConst (d__1__) ) end : S100.Exp))
;
(* Rule 18, file Parser.grm, line 71 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__1__ = peekVal 0 : S100.Lval
in
( S100.LV (d__1__) ) end : S100.Exp))
;
(* Rule 19, file Parser.grm, line 73 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Lval
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp
in
( S100.Assign ((d__1__),(d__3__),(d__2__)) ) end : S100.Exp))
;
(* Rule 20, file Parser.grm, line 74 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp
in
( S100.Plus ((d__1__), (d__3__), (d__2__)) ) end : S100.Exp))
;
(* Rule 21, file Parser.grm, line 75 *)
val _ = update_ yyact 21
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp
in
( S100.Minus ((d__1__), (d__3__), (d__2__)) ) end : S100.Exp))
;
(* Rule 22, file Parser.grm, line 76 *)
val _ = update_ yyact 22
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp
in
( S100.Less ((d__1__), (d__3__), (d__2__)) ) end : S100.Exp))
;
(* Rule 23, file Parser.grm, line 78 *)
val _ = update_ yyact 23
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : S100.Exp list
val d__4__ = peekVal 0 : (int*int)
in
( S100.Call (#1 (d__1__), (d__3__), (d__2__)) ) end : S100.Exp))
;
(* Rule 24, file Parser.grm, line 80 *)
val _ = update_ yyact 24
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : S100.Exp
val d__3__ = peekVal 0 : (int*int)
in
( (d__2__) ) end : S100.Exp))
;
(* Rule 25, file Parser.grm, line 83 *)
val _ = update_ yyact 25
(fn () => repr(let
in
( [] ) end : S100.Exp list))
;
(* Rule 26, file Parser.grm, line 84 *)
val _ = update_ yyact 26
(fn () => repr(let
val d__1__ = peekVal 0 : S100.Exp list
in
( (d__1__) ) end : S100.Exp list))
;
(* Rule 27, file Parser.grm, line 87 *)
val _ = update_ yyact 27
(fn () => repr(let
val d__1__ = peekVal 0 : S100.Exp
in
( [(d__1__)] ) end : S100.Exp list))
;
(* Rule 28, file Parser.grm, line 89 *)
val _ = update_ yyact 28
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp list
in
( (d__1__) :: (d__3__) ) end : S100.Exp list))
;
(* Rule 29, file Parser.grm, line 91 *)
val _ = update_ yyact 29
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( S100.Var (d__1__) ) end : S100.Lval))
;
(* Entry Prog *)
val _ = update_ yyact 30 (fn () => raise yyexit (peekVal 0));
val yytables : parseTables =
  ( yyact,
    yytransl,
    yylhs,
    yylen,
    yydefred,
    yydgoto,
    yysindex,
    yyrindex,
    yygindex,
    YYTABLESIZE,
    yytable,
    yycheck );
fun Prog lexer lexbuf = yyparse yytables 1 lexer lexbuf;
