let fatal loc msg = print (join ["boot2: error ", loc, ": ", msg]); exit 1

let scanner filename src = all_tokens ()
    where
    and index = ref 0
    and line = ref 1
    and col = ref 1
    and getloc () = join [filename, ":", itoa !line, ":", itoa !col]
    and peek () =   if !index < size src then src `char_at !index else '\0'
    and update c =  if c == '\n' then line += 1; col := 1 else col += 1
    and advance () = let c = peek() in update c; index += 1; c
    and more? () =  !index < size src
    and error msg = fatal (getloc ()) msg
    and resv = ["!", "--", "->", "::", "=", "@", "and", "case",
        "datatype", "else", "fn", "if", "in", "infixl", "infixr",
        "let", "rec", "then", "where", "|"]
    and eof () = (getloc (), "eof", "")
    and all_tokens () = loop []
    where rec
    and loop out = if space () then loop (next() : out) else reverse (eof () : out)

    and next () =   let i   = !index in
                    let loc = getloc() in
                    let c   = advance () in
                    let raw = if digit? c || c == '-' && digit? (peek ()) then
                                  (loc, "int", get i digit?)
                              else if pun? c then
                                  (loc, implode [c], "")
                              else if c == '\'' then
                                  (loc, "char", get (i + 1) chr?)
                              else if c == '"' then
                                  (loc, "string", get (i + 1) str?)
                              else if sym? c then
                                  (loc, "id", get i sym?)
                              else if id? c then
                                  (loc, "id", get i id?)
                              else error ("invalid token: " ^ implode [c])
                    in post raw

    and pun? c      =   "()[],`;" `contains_char c
    and id? c       =   alnum? c || c == '\'' || c == '_' || c == '?' || c == '!'
    and sym? c      =   "!$%&*+-/:<=>@^|~" `contains_char c
    and str? '\\'   =   advance (); true
        --   '"'    =   advance (); false
        --    _     =   true
    and chr? '\\'   =   advance (); true
        --   '\''   =   advance (); false
        --   _      =   true
    and post (loc, "id", id)    =   if resv `contains? id then (loc, id, "")
                                    else (loc, "id", id)
        --   (loc, "string", s) =   if s `char_at -1 <> '"' then
                                        error "unclosed string"
                                    else (loc, "string", unescape (substr s 0 -2))
        --   (loc, "char", s)   =   if s `char_at -1 <> '\'' then
                                        error "unclosed char"
                                    else (loc, "char", unescape (substr s 0 -2))
        --   other              =   other

    and space ()    =   if space? (peek ()) then while space?; space () else
                        if peek () == '#' then while (<> '\n'); space () else
                        more? ()

    and while accept = if more? () && accept (peek ()) then advance (); while accept
                       else ()

    and get base accept = while accept; substr src base !index

datatype type = TYPE (string, type list)
         |      TYPEVAR ((string, type) ref)
         |      NO_TYPE

let fn_type []   x   = x
    --      args ret = TYPE ("fn", args ++ [ret])

datatype expr = EINT (int)
         |      ECHAR (char)
         |      ESTRING (string)
         |      EVAR (string)
         |      ETUPLE (expr list)
         |      ELIST (expr list)
         |      EDEREF (expr)
         |      EFN ((expr list, expr, (bool, (expr, expr) list)) list)
         |      EAPP (expr, expr list)
         |      EINFIX (expr, string, expr)
         |      ECASE (expr, (expr, expr) list)
         |      EIF (expr, expr, expr)
         |      ELET (bool, (expr, expr) list, expr)
         |      ESEQ (expr, expr)
         |      EAT (expr, string)
         |      ETYPING (expr, type)

datatype top = ETOP (bool, (expr, expr) list)

let parse src =
    let
    and operators = ref [] :: (string, int, int) list ref
    and types = ref [] :: (string, type) list ref
    and feed = ref src
    and getloc () = let ((loc, _, _):_) = !feed in loc
    and get_text () = let ((_, _, text):_) = !feed in text
    and error msg = fatal (getloc ()) msg
    and peek token = let ((_, type, _):_) = !feed in token == type
    and next () = feed := tl !feed; true
    and want token = if peek token then next () else false
    and need token = if not (want token) then error ("need " ^ token) else ()
    and next_op? consume? = if not (peek "id") then NONE
                            else case find (with_id (get_text ())) !operators
                                | SOME (id, lhs, rhs) -> (if consume? then next () else true);
                                                         SOME (id, lhs, rhs)
                                | NONE                -> NONE
                            where (with_id id (op_id, _, _) = op_id == id)
    and identifier () = get_text () `before need "id"
    and identifier? () = let id = get_text () in if want "id" then SOME id else NONE
    and parened? item = if want ("(") then SOME (item ()) `before need(")") else NONE
    and with_types tmp_types body = let old = types in
                                    types := tmp_types ++ !types;
                                    body () `before types := old
    in
    let rec
    and opt_paren item = if want "(" then item () `before need ")" else item ()
    and seq? item = case item false
                    | SOME x -> x : seq? item
                    | NONE   -> []
    and seq item = valueof (item true) : seq? item
    and csv item ender = if want ender then []
                         else item () : (if want "," then csv item ender
                                         else need ender; [])
    in
    let rec
    and script () = seq? tld
    and tld first? = let loc = getloc () in
                     if want "let" then SOME $ ETOP (want "rec", seq dec)
                     else if want "infixl" then infix 1; tld first?
                     else if want "infixr" then infix 0; tld first?
                     # else if want "datatype" then datatypedec (); tld first?
                     else if want "eof" then NONE
                     else error "need top-level declaration"
    and infix adjust = let lhs = atoi (get_text ()) `before need "int"
                       and rhs = lhs + adjust
                       and op_id _ = let id = get_text () in
                                     if want "id" then SOME id else NONE
                       in app (fn id -> operators := (id, lhs, rhs) : !operators) (seq? op_id)
    # and datatypedec () = let params = map to_typevar (get_params ()) in
    #                      let id = identifier () in
    #                      let type = TYPE (id, map snd params) in
    #                      types := (id, type) : !types;
    #                      # with_types params (fn () -> seq (condec type))
    #                      ()
    #                      where (
    #                         and get_params() = for_option (parened? identifier) singleton (const [])
    #                         and to_typevar ids = (ids, TYPEVAR (ref ("", NO_TYPE))))
    # and condec dt first? = if want "|" || first? then
    #                            let id = identifier() in
    #                            case parened? ty
    #                            | SOME arg -> SOME (id, fn_type [arg] dt)
    #                            | NONE     -> SOME (id, dt)
    #                        else NONE
    and expr () = let e = expr' () in
                  if want "@" then EAT (e, get_text () `before need "id")
                  else if want "::" then ETYPING (e, ty ())
                  else if want ";" then ESEQ (e, expr ())
                  else e
    and expr' () = if want "let" then ELET (want "rec", seq dec, need "in"; expr ())
                   else if want "case" then ECASE (expr (), seq? case_rule)
                   else if want "if" then EIF (expr (), need "then"; expr (), need "else"; expr ())
                   else inexpr ()
    and inexpr () = let rec
                    and loop args ops level =
                        case next_op? true
                        | NONE                -> let ([e], _) = reduce args ops level in e
                        | SOME (id, lhs, rhs) -> let (args', ops') = reduce args ops lhs in
                                                 loop (appexpr () : args') ((id, lhs) : ops') rhs
                    and reduce args []  _     = (args, [])
                        --     args ops level = let ((id, op_level) : ops') = ops in
                                                if op_level <= level then
                                                    let (y : x : args') = args in
                                                    (EINFIX (x, id, y) : args', ops')
                                                else (args, ops)
                    in loop [appexpr ()] [] 0
    and appexpr () = case seq atexpr
                     | [e]      -> e
                     | f : args -> EAPP (f, args)
    and atexpr required? = let text = get_text () in
                           if not required? && issome (next_op? false) then NONE
                           else if want "int" then SOME $ EINT (atoi text)
                           else if want "char" then SOME $ ECHAR (text `char_at 0)
                           else if want "string" then SOME $ ESTRING text
                           else if want "id" then SOME $ EVAR text
                           else if want "(" then SOME $ ETUPLE (csv expr ")")
                           else if want "[" then SOME $ ELIST (csv expr "]")
                           else if want "!" then SOME $ EDEREF (valueof (atexpr true))
                           else if want "fn" then SOME $ EFN (seq (fn_rule "->"))
                           else if required? then error "need expression"
                           else NONE
    and case_rule first? = if want "|" then
                               SOME (expr (), need "->"; expr ())
                           else NONE
    and fn_rule delim first? = if want "--" || first? then
                                   let (params, body) = (seq atexpr, need delim; expr ()) in
                                   let decs = if want "where" then (want "rec", opt_paren (fn _ -> seq dec))
                                              else (false, [])
                                   in SOME (params, body, decs)
                               else NONE
    and dec first? = if want "and" || first? then
                         let lhs = valueof (atexpr true) in
                         let rhs = if want "=" then expr ()
                                   else EFN (seq (fn_rule "=")) in
                         SOME (lhs, rhs)
                     else NONE
    and ty () = let (args, ret) = tail_last (atty () : seq? arrow)
                in fn_type args ret
                where (
                   and apply t con = TYPE (con, [t])
                   and base () = if want "(" then TYPE ("tuple", csv ty ")")
                                 else TYPE (identifier (), [])
                   and con _ = identifier? ()
                   and atty () = foldl apply (base ()) (seq? con)
                   and arrow _ = if want "->" then SOME (atty ()) else NONE)
    in script ()

datatype cexpr = CINT (int)
         |       CCHAR (char)
         |       CSTRING (string)
         |       CVAR (string)
         |       CTUPLE (cexpr list)
         |       CDEREF (cexpr)
         |       CFN (string list, cexpr)
         |       CAPP (cexpr, cexpr list)
         |       CCASE (cexpr, (cexpr, cexpr) list)
         |       CIF (cexpr, cexpr, cexpr)
         |       CLET ((cexpr, cexpr) list, cexpr)
         |       CREC ((cexpr, cexpr) list, cexpr)
         |       CAT (cexpr, string)

let path = "test.ml"
let src = valueof (readfile path)
let tokens = scanner path src
# let _ = app print tokens
let _ = print (parse tokens)
