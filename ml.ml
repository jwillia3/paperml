
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
    and resv = ["=", "fn", "->", "let", "rec", "and", "in",
        "case", "|", "if", "then", "else", "::", "infixl",
        "infixr", "datatype", "--", "!", "where"]
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

datatype value =
| INT (int)
| CHAR (char)
| STRING (string)

datatype expr =
| ELIT (string, value)
| ENIL (string)
| ETUP (string, expr list)

let parse tokens = script ()
where
and feed    = ref tokens
and tokstr  = ref ""
and toktype = ref ""
and tokloc  = ref ""
and getloc () =     let ((l, _, _) : _) = !feed in l
and error_next msg =    fatal (getloc()) msg
and error_here msg =    fatal !tokloc msg
and update (l, t, s) = tokloc := l; toktype := t; tokstr := s
and advance () =    case !feed
                    | ((_, "eof", _) : _) -> ""
                    | (cur : rest) -> feed := rest; update cur

and peek tok    =   let ((_, tok', _) : _) = !feed in tok' == tok
and want tok    =   peek tok && (advance (); true)
and need tok    =   want tok || error_next ("need " ^ tok)
and script ()   =   update (hd !feed); script ()

where rec
and script () = expr ()
and expr () = aexpr true
and aexpr required =    let loc = getloc () in
                        if want "int" then
                            ELIT (loc, INT (atoi !tokstr))
                        else if want "string" then
                            ELIT (loc, STRING !tokstr)
                        else if want "char" then
                            ELIT (loc, CHAR (!tokstr `char_at 0))
                        else if want "(" then
                            case csv ")"
                            | [x] -> x
                            | list -> ETUP (loc, list)
                        else fatal !tokloc "need expression"
and csv delim =
    if want delim then [] else
    let x = expr () in x : (if want "," then csv delim else need delim; [])


let rec pe (ELIT (_, val)) = pv val
--         (ETUP (_, xs))  = "(" ^ join (separate_map ", " pe xs) ^ ")"

and pv (INT i)      =   itoa i
--     (CHAR c)     =   "'" ^ escape '\'' (implode [c]) ^ "'"
--     (STRING s)   =   "\"" ^ escape '"' s ^ "\""


let filename = "test.ml"
# let filename = "ml.ml"
let src = valueof (readfile filename)
let tokens = scanner filename src
# let _ = app print $ tokens
let _ = (print `of pe) (parse tokens)
# let _ = print (parse tokens)
let _ = print "done."
