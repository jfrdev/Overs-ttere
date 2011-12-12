local
type t__1__ = (int*int)
type t__2__ = (int*int)
type t__3__ = char*(int*int)
type t__4__ = (int*int)
type t__5__ = (int*int)
type t__6__ = (int*int)
type t__7__ = (int*int)
type t__8__ = string*(int*int)
type t__9__ = (int*int)
type t__10__ = (int*int)
type t__11__ = (int*int)
type t__12__ = (int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = (int*int)
type t__16__ = int*(int*int)
type t__17__ = (int*int)
type t__18__ = (int*int)
type t__19__ = (int*int)
type t__20__ = (int*int)
type t__21__ = (int*int)
type t__22__ = (int*int)
type t__23__ = (int*int)
type t__24__ = string*(int*int)
type t__25__ = (int*int)
in
datatype token =
    ASSIGN of t__1__
  | CHAR of t__2__
  | CHARCONST of t__3__
  | COMMA of t__4__
  | ELSE of t__5__
  | EOF of t__6__
  | EQUAL of t__7__
  | ID of t__8__
  | IF of t__9__
  | INT of t__10__
  | LBLOCK of t__11__
  | LBRACKET of t__12__
  | LESS of t__13__
  | LPAR of t__14__
  | MINUS of t__15__
  | NUM of t__16__
  | PLUS of t__17__
  | RBLOCK of t__18__
  | RBRACKET of t__19__
  | REF of t__20__
  | RETURN of t__21__
  | RPAR of t__22__
  | SEMICOLON of t__23__
  | STRINGCONST of t__24__
  | WHILE of t__25__
end;

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

val yytransl = #[
  257 (* ASSIGN *),
  258 (* CHAR *),
  259 (* CHARCONST *),
  260 (* COMMA *),
  261 (* ELSE *),
  262 (* EOF *),
  263 (* EQUAL *),
  264 (* ID *),
  265 (* IF *),
  266 (* INT *),
  267 (* LBLOCK *),
  268 (* LBRACKET *),
  269 (* LESS *),
  270 (* LPAR *),
  271 (* MINUS *),
  272 (* NUM *),
  273 (* PLUS *),
  274 (* RBLOCK *),
  275 (* RBRACKET *),
  276 (* REF *),
  277 (* RETURN *),
  278 (* RPAR *),
  279 (* SEMICOLON *),
  280 (* STRINGCONST *),
  281 (* WHILE *),
    0];

val yylhs = "\255\255\
\\001\000\002\000\002\000\003\000\003\000\007\000\007\000\008\000\
\\008\000\006\000\004\000\004\000\005\000\005\000\013\000\013\000\
\\013\000\013\000\013\000\013\000\014\000\014\000\009\000\009\000\
\\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\\009\000\010\000\010\000\011\000\011\000\012\000\012\000\012\000\
\\000\000";

val yylen = "\002\000\
\\002\000\007\000\000\000\001\000\001\000\002\000\000\000\003\000\
\\000\000\002\000\001\000\002\000\003\000\001\000\002\000\005\000\
\\007\000\005\000\003\000\004\000\002\000\000\000\001\000\001\000\
\\001\000\001\000\003\000\003\000\003\000\003\000\003\000\004\000\
\\003\000\000\000\001\000\001\000\003\000\001\000\002\000\004\000\
\\002\000";

val yydefred = "\000\000\
\\000\000\000\000\005\000\004\000\041\000\000\000\000\000\001\000\
\\011\000\000\000\000\000\012\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\025\000\000\000\000\000\009\000\000\000\023\000\
\\000\000\024\000\000\000\000\000\000\000\000\000\000\000\010\000\
\\008\000\000\000\000\000\039\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\015\000\000\000\002\000\
\\000\000\000\000\000\000\000\000\035\000\000\000\000\000\000\000\
\\000\000\033\000\019\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\013\000\040\000\000\000\032\000\000\000\021\000\020\000\
\\000\000\037\000\000\000\018\000\000\000\017\000";

val yydgoto = "\002\000\
\\005\000\006\000\007\000\031\000\032\000\018\000\014\000\015\000\
\\028\000\052\000\053\000\029\000\056\000\057\000";

val yysindex = "\010\000\
\\045\255\000\000\000\000\000\000\000\000\030\255\023\255\000\000\
\\000\000\029\255\027\255\000\000\000\000\031\255\045\255\075\255\
\\023\255\028\255\000\000\246\254\048\255\000\000\036\255\000\000\
\\036\255\000\000\049\255\001\255\067\255\045\255\065\255\000\000\
\\000\000\036\255\036\255\000\000\036\255\056\255\161\255\158\255\
\\036\255\036\255\036\255\036\255\036\255\000\000\036\255\000\000\
\\023\255\184\255\090\255\051\255\000\000\172\255\028\255\075\255\
\\058\255\000\000\000\000\173\255\070\255\070\255\000\000\000\000\
\\104\255\000\000\000\000\036\255\000\000\075\255\000\000\000\000\
\\075\255\000\000\074\255\000\000\075\255\000\000";

val yyrindex = "\000\000\
\\076\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\007\255\000\000\000\000\000\000\
\\000\000\066\255\000\000\091\255\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\105\255\076\255\240\254\000\000\
\\000\000\000\000\068\255\000\000\000\000\083\255\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\071\255\000\000\000\000\000\000\000\000\083\255\
\\000\000\000\000\000\000\000\000\140\255\147\255\119\255\133\255\
\\052\255\000\000\000\000\068\255\000\000\000\000\000\000\000\000\
\\000\000\000\000\024\255\000\000\000\000\000\000";

val yygindex = "\000\000\
\\000\000\072\000\246\255\108\000\067\000\087\000\000\000\107\000\
\\234\255\062\000\000\000\000\000\240\255\075\000";

val YYTABLESIZE = 203;
val yytable = "\030\000\
\\039\000\034\000\040\000\035\000\017\000\014\000\014\000\042\000\
\\009\000\036\000\001\000\050\000\051\000\043\000\054\000\044\000\
\\009\000\045\000\060\000\061\000\062\000\063\000\064\000\046\000\
\\065\000\016\000\016\000\017\000\007\000\016\000\009\000\016\000\
\\016\000\016\000\016\000\008\000\012\000\016\000\019\000\016\000\
\\013\000\016\000\010\000\020\000\016\000\051\000\003\000\016\000\
\\016\000\023\000\033\000\024\000\016\000\075\000\004\000\027\000\
\\076\000\003\000\019\000\026\000\078\000\037\000\041\000\020\000\
\\021\000\004\000\022\000\047\000\049\000\023\000\027\000\024\000\
\\069\000\027\000\027\000\072\000\025\000\019\000\077\000\026\000\
\\027\000\003\000\020\000\021\000\044\000\022\000\045\000\006\000\
\\023\000\034\000\024\000\038\000\036\000\068\000\038\000\025\000\
\\042\000\038\000\026\000\027\000\022\000\048\000\043\000\038\000\
\\044\000\038\000\045\000\038\000\026\000\038\000\042\000\026\000\
\\038\000\038\000\011\000\066\000\043\000\026\000\044\000\026\000\
\\045\000\026\000\029\000\026\000\055\000\029\000\026\000\026\000\
\\038\000\074\000\071\000\029\000\000\000\029\000\000\000\029\000\
\\028\000\029\000\000\000\028\000\029\000\029\000\000\000\030\000\
\\000\000\028\000\030\000\028\000\000\000\028\000\031\000\028\000\
\\030\000\031\000\028\000\028\000\000\000\000\000\030\000\031\000\
\\000\000\030\000\030\000\000\000\042\000\031\000\000\000\042\000\
\\031\000\031\000\043\000\000\000\044\000\043\000\045\000\044\000\
\\000\000\045\000\042\000\042\000\059\000\000\000\058\000\000\000\
\\043\000\043\000\044\000\044\000\045\000\045\000\042\000\000\000\
\\000\000\070\000\073\000\000\000\043\000\000\000\044\000\000\000\
\\045\000\000\000\067\000";

val yycheck = "\016\000\
\\023\000\012\001\025\000\014\001\015\000\022\001\023\001\007\001\
\\002\001\020\001\001\000\034\000\035\000\013\001\037\000\015\001\
\\010\001\017\001\041\000\042\000\043\000\044\000\045\000\023\001\
\\047\000\002\001\003\001\038\000\022\001\006\001\008\001\008\001\
\\009\001\010\001\011\001\006\001\008\001\014\001\003\001\016\001\
\\014\001\018\001\020\001\008\001\021\001\068\000\002\001\024\001\
\\025\001\014\001\023\001\016\001\022\001\070\000\010\001\004\001\
\\073\000\002\001\003\001\024\001\077\000\014\001\014\001\008\001\
\\009\001\010\001\011\001\001\001\004\001\014\001\019\001\016\001\
\\022\001\022\001\023\001\018\001\021\001\003\001\005\001\024\001\
\\025\001\006\001\008\001\009\001\015\001\011\001\017\001\022\001\
\\014\001\022\001\016\001\001\001\022\001\004\001\004\001\021\001\
\\007\001\007\001\024\001\025\001\018\001\030\000\013\001\013\001\
\\015\001\015\001\017\001\017\001\004\001\019\001\007\001\007\001\
\\022\001\023\001\007\000\049\000\013\001\013\001\015\001\015\001\
\\017\001\017\001\004\001\019\001\038\000\007\001\022\001\023\001\
\\022\000\068\000\056\000\013\001\255\255\015\001\255\255\017\001\
\\004\001\019\001\255\255\007\001\022\001\023\001\255\255\004\001\
\\255\255\013\001\007\001\015\001\255\255\017\001\004\001\019\001\
\\013\001\007\001\022\001\023\001\255\255\255\255\019\001\013\001\
\\255\255\022\001\023\001\255\255\007\001\019\001\255\255\007\001\
\\022\001\023\001\013\001\255\255\015\001\013\001\017\001\015\001\
\\255\255\017\001\007\001\007\001\023\001\255\255\022\001\255\255\
\\013\001\013\001\015\001\015\001\017\001\017\001\007\001\255\255\
\\255\255\022\001\022\001\255\255\013\001\255\255\015\001\255\255\
\\017\001\255\255\019\001";

val yyact = vector_ 42 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file Parser.grm, line 31 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : S100.FunDec list
val d__2__ = peekVal 0 : (int*int)
in
( (d__1__) ) end : S100.Prog))
;
(* Rule 2, file Parser.grm, line 36 *)
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
(* Rule 3, file Parser.grm, line 37 *)
val _ = update_ yyact 3
(fn () => repr(let
in
( [] ) end : S100.FunDec list))
;
(* Rule 4, file Parser.grm, line 40 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( S100.Int (d__1__) ) end : S100.Type))
;
(* Rule 5, file Parser.grm, line 41 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( S100.Char (d__1__) ) end : S100.Type))
;
(* Rule 6, file Parser.grm, line 44 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 1 : S100.Dec list
val d__2__ = peekVal 0 : S100.Dec
in
( (d__1__) @ [(d__2__)] ) end : S100.Dec list))
;
(* Rule 7, file Parser.grm, line 45 *)
val _ = update_ yyact 7
(fn () => repr(let
in
( [] ) end : S100.Dec list))
;
(* Rule 8, file Parser.grm, line 49 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Dec list
val d__2__ = peekVal 1 : S100.Dec
val d__3__ = peekVal 0 : (int*int)
in
( (d__1__) @ [(d__2__)] ) end : S100.Dec list))
;
(* Rule 9, file Parser.grm, line 50 *)
val _ = update_ yyact 9
(fn () => repr(let
in
( [] ) end : S100.Dec list))
;
(* Rule 10, file Parser.grm, line 53 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 1 : S100.Type
val d__2__ = peekVal 0 : S100.Sid list
in
( ((d__1__), (d__2__)) ) end : S100.Dec))
;
(* Rule 11, file Parser.grm, line 56 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( S100.Val (d__1__) ) end : S100.Sid))
;
(* Rule 12, file Parser.grm, line 57 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 1 : (int*int)
val d__2__ = peekVal 0 : string*(int*int)
in
( S100.Ref (d__2__) ) end : S100.Sid))
;
(* Rule 13, file Parser.grm, line 60 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Sid
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Sid list
in
( (d__1__) :: (d__3__) ) end : S100.Sid list))
;
(* Rule 14, file Parser.grm, line 61 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 0 : S100.Sid
in
( [(d__1__)] ) end : S100.Sid list))
;
(* Rule 15, file Parser.grm, line 63 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 1 : S100.Exp
val d__2__ = peekVal 0 : (int*int)
in
( S100.EX (d__1__) ) end : S100.Stat))
;
(* Rule 16, file Parser.grm, line 65 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : (int*int)
val d__3__ = peekVal 2 : S100.Exp
val d__4__ = peekVal 1 : (int*int)
val d__5__ = peekVal 0 : S100.Stat
in
( S100.If ((d__3__),(d__5__),(d__1__)) ) end : S100.Stat))
;
(* Rule 17, file Parser.grm, line 67 *)
val _ = update_ yyact 17
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
(* Rule 18, file Parser.grm, line 69 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : (int*int)
val d__3__ = peekVal 2 : S100.Exp
val d__4__ = peekVal 1 : (int*int)
val d__5__ = peekVal 0 : S100.Stat
in
( S100.While ((d__3__),(d__5__),(d__1__)) ) end : S100.Stat))
;
(* Rule 19, file Parser.grm, line 71 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : S100.Exp
val d__3__ = peekVal 0 : (int*int)
in
( S100.Return ((d__2__),(d__1__)) ) end : S100.Stat))
;
(* Rule 20, file Parser.grm, line 73 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : S100.Dec list
val d__3__ = peekVal 1 : S100.Stat list
val d__4__ = peekVal 0 : (int*int)
in
( S100.Block ((d__2__),(d__3__),(d__1__)) ) end : S100.Stat))
;
(* Rule 21, file Parser.grm, line 76 *)
val _ = update_ yyact 21
(fn () => repr(let
val d__1__ = peekVal 1 : S100.Stat
val d__2__ = peekVal 0 : S100.Stat list
in
( (d__1__) :: (d__2__) ) end : S100.Stat list))
;
(* Rule 22, file Parser.grm, line 77 *)
val _ = update_ yyact 22
(fn () => repr(let
in
( [] ) end : S100.Stat list))
;
(* Rule 23, file Parser.grm, line 79 *)
val _ = update_ yyact 23
(fn () => repr(let
val d__1__ = peekVal 0 : int*(int*int)
in
( S100.NumConst (d__1__) ) end : S100.Exp))
;
(* Rule 24, file Parser.grm, line 80 *)
val _ = update_ yyact 24
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( S100.StringConst (d__1__) ) end : S100.Exp))
;
(* Rule 25, file Parser.grm, line 81 *)
val _ = update_ yyact 25
(fn () => repr(let
val d__1__ = peekVal 0 : char*(int*int)
in
( S100.CharConst (d__1__) ) end : S100.Exp))
;
(* Rule 26, file Parser.grm, line 82 *)
val _ = update_ yyact 26
(fn () => repr(let
val d__1__ = peekVal 0 : S100.Lval
in
( S100.LV (d__1__) ) end : S100.Exp))
;
(* Rule 27, file Parser.grm, line 84 *)
val _ = update_ yyact 27
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Lval
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp
in
( S100.Assign ((d__1__),(d__3__),(d__2__)) ) end : S100.Exp))
;
(* Rule 28, file Parser.grm, line 85 *)
val _ = update_ yyact 28
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp
in
( S100.Plus ((d__1__), (d__3__), (d__2__)) ) end : S100.Exp))
;
(* Rule 29, file Parser.grm, line 86 *)
val _ = update_ yyact 29
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp
in
( S100.Minus ((d__1__), (d__3__), (d__2__)) ) end : S100.Exp))
;
(* Rule 30, file Parser.grm, line 87 *)
val _ = update_ yyact 30
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp
in
( S100.Equal ((d__1__), (d__3__), (d__2__)) ) end : S100.Exp))
;
(* Rule 31, file Parser.grm, line 88 *)
val _ = update_ yyact 31
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp
in
( S100.Less ((d__1__), (d__3__), (d__2__)) ) end : S100.Exp))
;
(* Rule 32, file Parser.grm, line 90 *)
val _ = update_ yyact 32
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : S100.Exp list
val d__4__ = peekVal 0 : (int*int)
in
( S100.Call (#1 (d__1__), (d__3__), (d__2__)) ) end : S100.Exp))
;
(* Rule 33, file Parser.grm, line 92 *)
val _ = update_ yyact 33
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : S100.Exp
val d__3__ = peekVal 0 : (int*int)
in
( (d__2__) ) end : S100.Exp))
;
(* Rule 34, file Parser.grm, line 95 *)
val _ = update_ yyact 34
(fn () => repr(let
in
( [] ) end : S100.Exp list))
;
(* Rule 35, file Parser.grm, line 96 *)
val _ = update_ yyact 35
(fn () => repr(let
val d__1__ = peekVal 0 : S100.Exp list
in
( (d__1__) ) end : S100.Exp list))
;
(* Rule 36, file Parser.grm, line 99 *)
val _ = update_ yyact 36
(fn () => repr(let
val d__1__ = peekVal 0 : S100.Exp
in
( [(d__1__)] ) end : S100.Exp list))
;
(* Rule 37, file Parser.grm, line 101 *)
val _ = update_ yyact 37
(fn () => repr(let
val d__1__ = peekVal 2 : S100.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : S100.Exp list
in
( (d__1__) :: (d__3__) ) end : S100.Exp list))
;
(* Rule 38, file Parser.grm, line 103 *)
val _ = update_ yyact 38
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( S100.Var (d__1__) ) end : S100.Lval))
;
(* Rule 39, file Parser.grm, line 104 *)
val _ = update_ yyact 39
(fn () => repr(let
val d__1__ = peekVal 1 : string*(int*int)
val d__2__ = peekVal 0 : (int*int)
in
( S100.Deref (d__1__) ) end : S100.Lval))
;
(* Rule 40, file Parser.grm, line 106 *)
val _ = update_ yyact 40
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : S100.Exp
val d__4__ = peekVal 0 : (int*int)
in
( S100.Lookup (#1 (d__1__), (d__3__), (d__2__)) ) end : S100.Lval))
;
(* Entry Prog *)
val _ = update_ yyact 41 (fn () => raise yyexit (peekVal 0));
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
