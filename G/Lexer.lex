{
 open Lexing;

 exception LexicalError of string * (int * int) (* (message, (line, column)) *)

 val currentLine = ref 1
 val lineStartPos = ref [0]

 fun getPos lexbuf = getLineCol (getLexemeStart lexbuf)
				(!currentLine)
				(!lineStartPos)

 and getLineCol pos line (p1::ps) =
       if pos>=p1 then (line, pos-p1)
       else getLineCol pos (line-1) ps
   | getLineCol pos line [] = raise LexicalError ("",(0,0))

 fun lexerError lexbuf s = 
     raise LexicalError (s, getPos lexbuf)

 fun keyword (s, pos) =
     case s of
         "if"           => Parser.IF pos
       | "then"         => Parser.THEN pos
       | "else"         => Parser.ELSE pos
       | "int"          => Parser.INT pos
       | "return"       => Parser.RETURN pos
       | "while"        => Parser.WHILE pos
       | "char"         => Parser.CHAR pos
       | _              => Parser.ID (s, pos)

 fun character (s, pos, lexbuf) =
     val char = String.substring(s, 1, s.size-2)
     case Char.fromCString (char) of
          NONE      => lexerError lexbuf "Invalid character"
        (*| SOME #"'" => lexerError lexbuf "Invalid character"*)
        | SOME c    => Parser.CHARCONST (c, pos)           (* HANDLE LEXICAL ERROR *)

 fun string (s, pos, lexbuf) =
     val str = String.substring(s, 1, s.size-2)
     case String.fromCString (str) of
          NONE      => lexerError lexbuf "Invalid string"
        | SOME c    => Parser.STRINGCONST (c, pos)           (* HANDLE LEXICAL ERROR *)

 }


rule Token = parse
    [` ` `\t` `\r`]+    { Token lexbuf } (* whitespace *)
    | "/*" ([^`*`] | `*`[^`/`])* "*/"
			{ Token lexbuf } (* comment *)
  | [`\n` `\012`]       { currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
			                   :: !lineStartPos;
                          Token lexbuf } (* newlines *)
  | [`0`-`9`]+          { case Int.fromString (getLexeme lexbuf) of
                               NONE   => lexerError lexbuf "Bad integer"
                             | SOME i => Parser.NUM (i, getPos lexbuf) }
  | [`a`-`z` `A`-`Z`] [`a`-`z` `A`-`Z` `0`-`9` `_`]*
                        { keyword (getLexeme lexbuf,getPos lexbuf) }
  | `*`[`a`-`z` `A`-`Z`][`a`-`z` `A`-`Z` `0`-`9` `_`]*
                        { Parser.REF (getLexeme lexbuf, getPos lexbuf) }
  | `'`(`\`([` `-`~`]|[0-7][0-7][0-7]) | [` `-`!` `#`-`&` `(`-`[` `]`-`~`])`'`
                        { character (getLexeme lexbuf, getPos lexbuf, lexbuf) }
  | `"`(`\`([` `-`~`]|[0-7][0-7][0-7]) | [` `-`!` `#`-`[` `]`-`~`])*`"`
                        { string (getLexeme lexbuf, getPos lexbuf, lexbuf) }
  | `+`                 { Parser.PLUS (getPos lexbuf) }
  | `-`                 { Parser.MINUS (getPos lexbuf) }
  | `<`                 { Parser.LESS (getPos lexbuf) }
  | `=`                 { Parser.ASSIGN (getPos lexbuf) }
  | `(`                 { Parser.LPAR (getPos lexbuf) }
  | `)`                 { Parser.RPAR (getPos lexbuf) }
  | `,`                 { Parser.COMMA (getPos lexbuf) }
  | `;`                 { Parser.SEMICOLON (getPos lexbuf) }
  | eof                 { Parser.EOF (getPos lexbuf) }
  | `{`                 { Parser.BEGINBLOCK (getPos lexbuf) }
  | `}`                 { Parser.ENDBLOCK (getPos lexbuf) }
  | "=="                { Parser.EQUAL (getPos lexbuf) }
  | _                   { lexerError lexbuf "Illegal symbol in input" }

;
