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
type t__26__ = (int*int)
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
  | THEN of t__25__
  | WHILE of t__26__
end;

val Prog :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> S100.Prog;
