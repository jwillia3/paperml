
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
                    let raw =   if digit? c || c == '-' && digit? (peek ()) then
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







datatype type =
| TYPEVAR ((string, type option) ref)
| TYPE (type list, string)
| FNTYPE (type, type)
| TUPTYPE (type list)

datatype value =
| INT (int)
| CHAR (char)
| STRING (string)

datatype expr =
| ELIT (string, value)
| EVAR (string, string)
| ETUP (string, expr list)
| ENIL (string)
| ECONS (string, expr, expr)
| EDEREF (string, expr)
| EFN (string, string, expr)
| EAPP (string, expr, expr)
| ECASE (string, expr, (expr, expr) list)
| EIF (string, expr, expr, expr)
| ELET (string, bool, (expr, expr) list, expr)
| ESEQ (string, expr, expr)
| ETYPING (string, expr, type)
| EAS (string, expr, string)



let booltype    = TYPE ([], "bool")
let inttype     = TYPE ([], "int")
let strtype     = TYPE ([], "string")
let listtype a  = TYPE ([a], "list")
let opttype a   = TYPE ([a], "option")
let reftype a   = TYPE ([a], "ref")
let typevar ()  = TYPEVAR (ref ("", NONE))
let basis_types = [ ("bool", booltype),
                    ("int", inttype),
                    ("string", strtype),
                    ("list", listtype (typevar ())),
                    ("option", opttype (typevar ())),
                    ("ref", reftype (typevar ()))]
let basis_constrs = []




# Shorten typevar chain and return most concrete type.
let rec prune
    -- (TYPEVAR (ref (id, SOME t) @ r)) =
        prune t `tap (fn t' -> r := (id, SOME t'))
    -- t = t

# Print type to string.
# This takes a list of types because typevars lack names coming in.
# We must assign them new names simultaneously for printing.
# Otherwise, two calls to `writetypes [typevar()]` would both print `_a`.
let writetypes types = map p (rename types)
    where rec
    and p
        -- (TYPEVAR (ref (_, SOME target))) = p target
        -- (TYPEVAR (ref (id, _)))  = id
        -- (TYPE ([], id))          = id
        -- (TYPE ([x], id))         = join [p x, " ", id]
        -- (TUPTYPE xs)             = "(" ^ join (separate_map "," p xs) ^ ")"
        -- (FNTYPE (FNTYPE _ @ x, r)) = join ["(", p x, ") -> ", p r]
        -- (FNTYPE (x, r))          = join [p x, " -> ", p r]

    and rename types =
        let uid = ref 0 in
        let gensym () = implode ['_', chr (96 + (uid += 1))] in
        let rec assign
            -- (TYPEVAR _ @ t)  = assign_var t
            -- (TYPE (ts, id))  = TYPE (map assign ts, id)
            -- (FNTYPE (a, b))  = FNTYPE (assign a, assign b)
            -- (TUPTYPE ts)     = TUPTYPE (map assign ts)
        and assign_var
            -- (TYPEVAR (ref (_, SOME t'))) = assign t'
            -- (TYPEVAR (ref ("", NONE) @ r) @ t) =
                r := (gensym (), NONE); t
            -- t = t
        in
        map assign types

let locof
    -- (ELIT (loc, _))      = loc
    -- (EVAR (loc, _))      = loc
    -- (ETUP (loc, _))      = loc
    -- (ENIL (loc))         = loc
    -- (ECONS (loc, _, _))  = loc
    -- (EDEREF (loc, _))    = loc
    -- (EFN (loc, _, _))    = loc
    -- (EAPP (loc, _, _))   = loc
    -- (ECASE (loc, _, _))  = loc
    -- (EIF (loc, _, _, _)) = loc
    -- (ELET (loc, _, _, _))= loc
    -- (ESEQ (loc, _, _))   = loc
    -- (ETYPING (loc, _, _))= loc
    -- (EAS (loc, _, _))    = loc

let printexpr e = p e where rec (
    and p
        -- (ELIT (_, val))      = pv val
        -- (EVAR (_, id))       = id
        -- (ETUP (_, xs))       = "(" ^ join (separate_map ", " p xs) ^ ")"
        -- (ENIL (_))           = "[]"
        -- (ECONS (_, x, xs))   = join ["(", p x, ":", p xs, ")"]
        -- (EDEREF (_, x))      = join ["! ", p x]
        -- (EAPP (_, f, x))     = join ["(", p f, " ", p x, ")"]
        -- (EFN (_, x, e))      = join ["(fn ", x, " -> ", p e, ")"]
        -- (ECASE (_, s, rs))   = join ["(case ", p s, join (flatmap pr rs), ")"]
        -- (EIF (_, a, b, c))   = join ["(if ", p a, " then ", p b, " else ", p c, ")"]
        -- (ELET (_, r, ds, e)) = join ["(let ", if r then "rec " else "",
                                        join (separate " and " $ map pd ds),
                                        " in ", p e, ")"]
        -- (ESEQ (_, a, b))     = join ["(", p a, "; ", p b, ")"]
        -- (ETYPING (_, e, t))  = join ["(", p e, " :: ", hd (writetypes [t]), ")"]
        -- (EAS (_, e, id))     = join ["(", p e, " @ ", id, ")"]

    and pr (lhs, rhs) = [" | ", p lhs, " -> ", p rhs]
    and pd (lhs, rhs) = join [p lhs, " = ", p rhs]

    and pv (INT i)      =   itoa i
    --     (CHAR c)     =   "'" ^ escape '\'' (implode [c]) ^ "'"
    --     (STRING s)   =   "\"" ^ escape '"' s ^ "\""
)


let parse tokens = script ()
where

# State variables.
and feed    = ref tokens
and tokstr  = ref ""
and toktype = ref ""
and tokloc  = ref ""
and infixes = ref []
and types   = ref basis_types
and constrs = ref basis_constrs

and getloc () =     let ((l, _, _) : _) = !feed in l

and error_at loc msg    = fatal loc msg
and error_next msg      = fatal (getloc()) msg
and error_here msg      = fatal !tokloc msg

and update (l, t, s)    = tokloc := l; toktype := t; tokstr := s
and advance ()          = case !feed
                          | ((_, "eof", _) : _) -> ""
                          | (cur : rest) -> feed := rest; update cur

and peek tok            = let ((_, tok', _) : _) = !feed in tok' == tok
and want tok            = peek tok && (advance (); true)
and need tok            = want tok || error_next ("need " ^ tok)
and needid ()           = need "id"; !tokstr

and nextstr ()          = let ((_, _, s) : _) = !feed in s
and peek_infix ()       = if peek "id" then lookup (nextstr()) !infixes else NONE
and want_infix level    = filter_option (fn (lhs, _) -> level == lhs) (peek_infix ())
and suffix x tok        = need tok; x

and listof item delim = suffix (loop ()) delim where rec (
                            loop () = if peek delim then [] else
                                      item () : (if want "," then loop () else []))

and sequence item     = loop () where rec (
                            loop () = for_option (item false)
                                                 (fn x -> x : loop ())
                                                 (const []))

and required_sequence item = valueof (item true) : sequence item

and foldseq get val = loop val where rec (
    loop val = case (get val)
               | (true, val)  -> loop val
               | (false, val) -> val)

# Types may be introduced in the block, but they're removed afterwards.
and restore_types do! = let old = !types in do! () `tap fn _ -> types := old

and script ()   =   update (hd !feed); script ()

where rec

and script () = if want "infixl" then infixdec true; script ()
                else if want "infixr" then infixdec false; script ()
                else if want "datatype" then typedec (); script ()
                else expr ()

and infixdec left? =    let lhs = need "int"; atoi !tokstr in
                        let rhs = lhs + (if left? then 1 else 0) in
                        sequence (fn _ ->
                            if want "id" then
                                infixes := (!tokstr, (lhs, rhs)) : !infixes;
                                SOME true
                            else NONE)

and typedec () =    let (arg, (TYPE (_, id) @ dt)) = header ()
                        in  types := (id, dt) : !types;
                            restore_types (fn () ->
                                types := arg ++ !types;
                                need "=";
                                required_sequence (condec dt))
                    where rec (
                        and header () = if want "(" then
                                            let id = needid () `suffix ")" in
                                            let arg = typevar () in
                                            ([(id, arg)], TYPE ([arg], id))
                                        else
                                            needid () << fn id -> ([], TYPE ([], id))
                        and condec dt first? =
                                    if want "|" || first? then
                                        let id = needid () in
                                        let t = if peek "(" then FNTYPE (type (), dt)
                                                else dt in
                                        SOME (constrs := (id, t) : !constrs)
                                    else NONE
                    )


and type () = restore_types type'
    where rec (
        and type' () = arrows (constrs (basic ()))
        and basic () =  if want "id" then
                            for_option (lookup !tokstr !types) nullary! undef_base!
                        else if want "(" then
                            case listof type' ")"
                            | [x] -> x
                            | xs  -> TUPTYPE xs
                        else error_next "need type"
        and nullary!
            -- (TYPE ([], _) @ t) = t
            -- (TYPEVAR _ @ t)  = t
            -- _                = error_here ("type needs arg: " ^ !tokstr)
        and coin_type id  = typevar () `tap fn t -> types := (id, t) : !types
        and undef_base! _ = if !tokstr `startswith "_" then coin_type !tokstr
                            else error_here ("undefined type: " ^ !tokstr)
        and undef_ctor! _ = error_here ("undefined type: " ^ !tokstr)
        and constrs t   =   foldseq apply t
        and apply t     =   if want "id" then
                                for_option (lookup !tokstr !types)
                                           (apply! t `of prune)
                                           undef_ctor!
                            else (false, t)
        and apply! t (TYPE ([x], id)) = (true, TYPE ([t], id))
            --     t (TYPE ([], id))  = error_here ("type takes no args: " ^ !tokstr)
            --     _ _                = error_here ("not a constructor")
        and arrows t    =   if want "->" then FNTYPE (t, type' ()) else t
    )

and letexpr rec? get    =   let loc = !tokloc in
                            let decs = required_sequence dec in
                            let body = get () in

                            app (only_rec_fn rec?) decs;
                            ELET (loc, rec?, decs, body)

                            where (
                                and var? (EVAR _) = true
                                    --   _        = false
                                and fn? (EFN _)   = true
                                    --  _         = false
                                and only_rec_fn rec? (lhs, rhs) =
                                    if not rec? || (var? lhs && fn? rhs) then ()
                                    else error_at (locof lhs) "let rec only defines functions")

and dec first? =    if want "and" || first? then
                        let lhs = valueof (aexpr true) in
                        let rhs = if want "=" then expr () else fnexpr "=" in
                        SOME (lhs, rhs)
                    else NONE

and where? body =   if want "where" then
                        let rec? = want "rec" in
                        where? (if want "(" then letexpr rec? (const body) `suffix ")"
                                else letexpr rec? (const body))
                    else body

and expr () =       tail (cexpr ())
                    where rec (
                        tail e =    if want "::" then
                                        tail (ETYPING (!tokloc, e, type ()))
                                    else if want "@" then
                                        tail (EAS (!tokloc, e, needid ()))
                                    else if want ";" then
                                        tail (ESEQ (!tokloc, e, expr ()))
                                    else e
                    )

and cexpr () =      let loc = getloc () in
                    if want "let" then
                        letexpr (want "rec") (fn _ -> want "in"; expr ())
                    else if want "case" then
                        ECASE (loc, expr (), required_sequence rule)
                    else if want "if" then
                        let a = expr () in
                        let b = need "then"; expr () in
                        let c = need "else"; expr () in
                        EIF (loc, a, b, c)
                    else iexpr 0

and iexpr 10    =   let lhs = valueof (aexpr true) in
                    let args = sequence aexpr in
                    foldl (fn f x -> EAPP (locof x, f, x)) lhs args

    --    level =   let rec operand_loop lhs =
                        case want_infix level
                        | NONE -> lhs
                        | SOME (lp, rp) ->
                            let loc = getloc () in
                            let op = EVAR (loc, advance (); !tokstr) in
                            let rhs = iexpr rp in
                            let all = EAPP (loc, EAPP (loc, op, lhs), rhs) in
                            operand_loop all
                    in operand_loop (iexpr (level + 1))

and aexpr required =    let loc = getloc () in
                        if not required && issome (peek_infix ()) then
                            NONE
                        else if want "int" then
                            SOME (ELIT (loc, INT (atoi !tokstr)))
                        else if want "string" then
                            SOME (ELIT (loc, STRING !tokstr))
                        else if want "char" then
                            SOME (ELIT (loc, CHAR (!tokstr `char_at 0)))
                        else if want "id" then
                            SOME (EVAR (loc, !tokstr))
                        else if want "(" then
                            case listof expr ")"
                            | [x]   -> SOME x
                            | list  -> SOME (ETUP (loc, list))
                        else if want "[" then
                            SOME (foldr (fn i rest -> ECONS (locof i, i, rest))
                                        (ENIL loc)
                                        (listof expr "]"))
                        else if want "!" then
                            SOME (EDEREF (loc, valueof (aexpr true)))
                        else if want "fn" then
                            SOME (fnexpr "->")
                        else if required then error_next "need expression"
                        else NONE

and fnexpr delim =
    let loc = !tokloc in
    let rules = required_sequence (fnrule delim) in
    if length rules == 1 && none? complexrule? rules then
        let [(_, params, body)] = rules in simple params body
    else tocase loc rules

    where (
        and fnrule delim first =
            if want "--" || first then
                let loc = getloc () in
                let params = required_sequence aexpr in
                let body = need delim; where? (expr ()) in
                SOME (loc, params, body)
            else NONE

        and complexparam? (EVAR _) = false -- _ = true
        and complexrule? (_, params, _) = any? complexparam? params

        # Make single-argument functions.
        and simple params body =
            let each (EVAR (loc, id)) e = EFN (loc, id, e) in
            foldr each body params

        # Convert `fn pat -> body` to `fn NEW -> case NEW | pat -> body`.
        and tocase loc (rules :: (string, expr list, expr) list) =
            app (check! arity) rules;
            simple new newcase

            where (
                and tuple _   [x] = x
                    --    loc xs  = ETUP (loc, xs)
                and rule_params (_, ps, _)  = ps
                and rule_case (_, ps, e)    = (tuple loc ps, e)
                and gensym i e              = EVAR (locof e, "$" ^ itoa i)
                and check! n (loc, ps, _)   = if length ps == n then ()
                                              else error_at loc "wrong param count"

                and first_params = rule_params (hd rules)
                and arity       = length first_params
                and new         = mapi gensym first_params
                and subject     = tuple loc new
                and newcase     = ECASE (loc, subject, map rule_case rules)
            )
    )

and rule first? =
    if want "|" then
        let lhs = expr () in
        let rhs = need "->"; expr () in
        SOME (lhs, rhs)
    else if first? then error_next "need rule"
    else NONE



let path    = "test.ml"
let src     = valueof (readfile path)
let tokens  = scanner path src
let _ = (print `of printexpr) (parse tokens)
let _ = print "done."
