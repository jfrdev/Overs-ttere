structure Type :> Type =
struct

  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*(int*int)

  type pos = int*int

  datatype Type = Int
                | Char
                | Ref of Type

  fun convertType (S100.Int _) = Int
    | convertType (S100.Char _) = Char

  fun getName (S100.Val (f,p)) = f
    | getName (S100.Ref (f,p)) = f

  fun getType t (S100.Val (f,p)) = convertType t
    | getType t (S100.Ref (f,p)) = Ref(convertType t)

  (* lookup function for symbol table as list of (name,value) pairs *)
  fun lookup x []
        = NONE
    | lookup x ((y,v)::table)
        = if x=y then SOME v else lookup x table

  fun checkExp e vtable ftable =
    case e of
      S100.NumConst _ => Int
    | S100.CharConst _ => Char
    | S100.StringConst _ => Ref(Char)
    | S100.LV lv => checkLval lv vtable ftable
    | S100.Assign (lv,e1,p) =>
        (case (checkLval lv vtable ftable,
               checkExp e1 vtable ftable) of 
          (Ref(t), Ref _) => Ref(t)
        | (Ref _,_)       => raise Error ("Cannot assign to reference",p)
        | (_, Ref _)      => raise Error ("Cannot assign to reference",p)
        | (t1, t2)        => t1)
    | S100.Plus (e1,e2,p) =>
        (case (checkExp e1 vtable ftable,
	       checkExp e2 vtable ftable) of
	      (Ref _, Ref _) => raise Error ("Can't add references",p)
        | (Ref(t), _)    => Ref(t)
        | (_, Ref(t))    => Ref(t)
        | (_, _)         => Int)     
    | S100.Minus (e1,e2,p) =>
        (case (checkExp e1 vtable ftable,
	       checkExp e2 vtable ftable) of
          (Ref(t1), Ref(t2)) => if t1 = t2 then
                                  Int
                                else
                                  raise Error ("Type mismatch in subtraction",p)
        | (Ref(t), _)        => Ref(t)
        | (_, Ref _)         => raise Error ("Can't subtract a reference",p)
        | (_, _)             => Int)
    | S100.Less (e1,e2,p) =>
        (case (checkExp e1 vtable ftable, 
               checkExp e2 vtable ftable) of
          (Ref(t1), Ref(t2)) => if t1=t2 then Int else raise Error ("Type mismatch in less-than comparison",p)
        | (t1, t2)        => if t2 <> Ref(Int) andalso t2 <> Ref(Char) then
                               Int
                             else
                               raise Error ("Type mismatch in less-than comparison",p))
    | S100.Call (f,es,p) => 
        (case lookup f ftable of
	      NONE => raise Error ("Unknown function: "^f,p)
    	 | SOME (parT,resultT) =>
             let
               val argT = List.map (fn e => checkExp e vtable ftable) es
             in
	           if parT = argT then resultT
	           else raise Error ("Arguments don't match declaration of "^f, p)
	         end)
    | S100.Equal (e1,e2,p) =>
        (case (checkExp e1 vtable ftable, 
               checkExp e2 vtable ftable) of
          (Ref(t1), Ref(t2)) => if t1=t2 then Int else raise Error ("Type mismatch in equal comparison",p)
        | (t1, t2)        => if t2 <> Ref(Int) andalso t2 <> Ref(Char) then
                               Int
                             else
                               raise Error ("Type mismatch in equal comparison",p))

  and checkLval lv vtable ftable =
    case lv of
      S100.Var (x,p) =>
        (case lookup x vtable of
          SOME t => t
	    | NONE   => raise Error ("Unknown variable: "^x,p))
     | S100.Deref (x,p) =>
        (case lookup x vtable of
          SOME (Ref(t)) => t
        | SOME t        => raise Error ("Is not a reference: "^x,p)
        | NONE          => raise Error ("Unknown reference: "^x,p))
     | S100.Lookup (x,e,p) =>
         let
           val t1 = checkExp e vtable ftable
         in
           if t1 <> Ref(Int) andalso t1 <> Ref(Char) then
              (case lookup x vtable of
                SOME (Ref(t)) => t
              | SOME t        => raise Error ("Is not a reference: "^x,p)
              | NONE          => raise Error ("Unknown reference: "^x,p))
           else raise Error ("Expression not of type Int",p)
         end

  fun extend [] _ vtable = vtable
    | extend (S100.Val (x,p)::sids) t vtable =
      (case lookup x vtable of
        NONE   => extend sids t ((x,t)::vtable)
      | SOME _ => raise Error ("Double declaration of "^x,p))
    | extend (S100.Ref (x,p)::sids) t vtable =
      (case lookup x vtable of
        NONE   => extend sids t ((x,Ref(t))::vtable)
      | SOME _ => raise Error ("Double declaration of "^x,p))

  fun checkDecs [] = []
    | checkDecs ((t,sids)::ds) =
        extend (List.rev sids) (convertType t) (checkDecs ds)

  fun compareTypes t [] = true
    | compareTypes t1 ((t2,_)::tt) = if t1 = t2 then compareTypes t1 tt else
      false 

  fun comparertable [] = false
    | comparertable ((_,b)::tt) =
        if b = true then true else comparertable tt
 
  fun checkStat s vtable ftable rtable b =
    case s of
      S100.EX e => (checkExp e vtable ftable; rtable)
    | S100.If (e,s1,p) =>
        if checkExp e vtable ftable = Int
	    then (checkStat s1 vtable ftable rtable false)
    	else raise Error ("Condition should be integer",p)
        (*farlig*)
    | S100.IfElse (e,s1,s2,p) =>
        if checkExp e vtable ftable = Int
	      then
            let
              val rtable1' = checkStat s1 vtable ftable [] b
              val rtable2' = checkStat s2 vtable ftable [] b
            in
              if comparertable rtable1' andalso comparertable rtable2' then 
                checkStat s1 vtable ftable rtable b @ checkStat s2 vtable ftable rtable b
              else 
                checkStat s1 vtable ftable rtable false @ checkStat s2 vtable ftable rtable false
            end
	      else raise Error ("Condition should be integer",p) 
        (*farlig if not both then and else returns*)
    | S100.Return (e,p) => (let val x = (checkExp e vtable ftable) in
      (x,b)::rtable end)
    | S100.While (e,s1,p) => 
        if checkExp e vtable ftable = Int then 
          checkStat s1 vtable ftable rtable false
	    else raise Error ("Condition should be integer",p)
        (*farlig*)
    | S100.Block (ds,ss,p) => (checkStats ss ((checkDecs ds)@vtable) ftable rtable
    b)

  and checkStats [] _ _ _ _ = []
    | checkStats (s::ss) vtable ftable rtable b = 
    (checkStat s vtable ftable rtable b @ checkStats ss vtable ftable rtable b)

  fun checkFunDec (t,sf,decs,body,p) ftable =
    let
      val rtable = checkStat body (checkDecs decs) ftable [] true
      val k = (convertType t)
    in
      case (comparertable rtable, compareTypes k rtable) of
           (false,_)    =>  raise Error ("Return statement missing or not guaranteed",p)
         | (_, false)   =>  raise Error ("Return statements not of the same type",p)
         | (true, true) =>  ()
    end

  fun getFuns [] ftable = ftable
    | getFuns ((t,sf,decs,_,p)::fs) ftable =
        case lookup (getName sf) ftable of
	  NONE =>
            let
              val parT = (List.map (#2) (checkDecs decs))
	      val resultT = getType t sf
	    in
              getFuns fs ((getName sf, (parT,resultT)) :: ftable)
	    end
	| SOME _ => raise Error ("Redeclaration of "^ getName sf,p)

  fun checkProg fs =
    let
      val ftable = getFuns fs [("getint",([],Int)),
                               ("walloc",([Int],Ref(Int))),
                               ("balloc",([Int],Ref(Char))),
                               ("getstring",([Int],Ref(Char))),
			                   ("putint",([Int],Int)),
                               ("putstring",([Ref(Char)],Ref(Char)))]
    in
      List.app (fn f => checkFunDec f ftable) fs;
      case lookup "main" ftable of
	    NONE => raise Error ("No main function found",(0,0))
      | SOME ([],Int) => ()
      | _ => raise Error ("main function has illegal type",(0,0))
    end
end
