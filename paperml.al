datatype id :: string
datatype loc :: string

let error loc msg =
    print (join ["paperml.al: error ", loc, ": ", msg]);
    exit 1




#
#   Lexical Analysis.
#

datatype token :: {loc::string, txt::string, type::string}

let tokenise path src :: string -> string -> [token] =
    let index = ref 0
    let lineptr = ref 0
    let ln = ref 1
    let peek p = !index + 1 < strlen src and p (charat src (!index + 1))
    let getloc () = join [path, ":", itoa !ln, ":", itoa (!index - !lineptr + 1)]
    let while p =
        loop ()
        where rec loop () =
            if !index < strlen src and p (charat src !index) then
                incr index;
                loop ()
            else !index

    let space c =
        case c
        | '\n' -> incr ln; lineptr := !index + 1; true
        | ' ' -> true
        | '\t' -> true
        | '#' -> while ((<>) '\n'); incr ln; lineptr := !index + 1; true
        | _ -> false

    let resv = [
        "->",
        "::",
        "=",
        "and",
        "case",
        "datatype",
        "def",
        "else",
        "endc",
        "ende",
        "endf",
        "endw",
        "exception",
        "except",
        "if",
        "in",
        "infixl",
        "infixr",
        "let",
        "or",
        "rec",
        "then",
        "with",
        "where",
        "|",
    ]

    let isid c = isalnum c or c == '_' or c == '\''
    let issym c = issome (findchar "%&$+-/:<=>?@~^|*" c)
    let ispun c = issome (findchar "!()[]{},.;\\`" c)
    let quoted q c =
        if c == '\\' then
            incr index;
            true
        else if c == q then
            incr index;
            false
        else
            true

    let token type p =
        let loc = getloc ()
        let i = !index
        let i' = incr index; while p
        let txt = substr src i (i' - i)
        in
        case type
        | "int" -> {loc, type, txt}
        | "pun" -> {loc, type=txt, txt}
        | "char" ->
            if charat txt -1 <> '\'' then
                error loc "char not closed"
            else
                let txt = unescape (substr txt 1 -2)
                in
                if strlen txt <> 1 then
                    error loc "char must be one character"
                else
                    {loc, type, txt}
        | "string" ->
            if charat txt -1 <> '"' then
                error loc "string not closed"
            else
                {loc, type, txt=unescape (substr txt 1 -2)}
        | "id" ->
            if resv `contains txt then {loc, type=txt, txt} else {loc, type, txt}

    let single () =
        let c = charat src !index
        in
        if c == '-' and peek isdigit then token "int" isdigit
        else if isdigit c then token "int" isdigit
        else if ispun c then token "pun" (const false)
        else if c == '\'' then token "char" (quoted '\'')
        else if c == '\"' then token "string" (quoted '"')
        else if isid c then token "id" isid
        else if issym c then token "id" issym
        else error (getloc ()) "invalid token"

    let rec multi out =
        while space;
        if !index < strlen src then
            multi (single () : out)
        else
            rev ({loc=getloc (), type="eof", txt="eof"} : out)
    in
    multi []




#
#   Parsing.
#


datatype tyexp =
    | TEcon (loc, tyexp list, id)
    | TEtuple (loc, tyexp list)
    | TErecord (loc, (id, tyexp) list)
    | TEfn (loc, tyexp, tyexp)
    | TEscheme (loc, id list, tyexp)

datatype ast =
    | Aint (loc, int)
    | Achar (loc, char)
    | Astring (loc, string)
    | Aid (loc, id)
    | Anil loc
    | Acons (loc, ast, ast)
    | Atuple (loc, ast list)
    | Arecord (loc, (id, ast) list)
    | Ader (loc, ast)
    | Afn (loc, ast, ast)
    | Adot (loc, ast, id)
    | Awith (loc, ast, (id, ast) list)
    | Aapp (loc, ast, ast)
    | Aand (loc, ast, ast)
    | Aor (loc, ast, ast)
    | Alet (loc, ast, tyexp option, ast, ast)
    | Aletrec (loc, (loc, ast, tyexp option, ast) list, ast)
    | Aif (loc, ast, ast, ast)
    | Acase (loc, ast, (loc, ast, ast) list)
    | Aty (loc, ast, tyexp)
    | Aseq (loc, ast, ast)
    | Aexception (loc, id, ast)
    | Aexcept (loc, ast, (loc, id, ast, ast) list)

let aloc ast = case ast
    | Aint (loc, _) -> loc
    | Achar (loc, _) -> loc
    | Astring (loc, _) -> loc
    | Aid (loc, _) -> loc
    | Anil loc -> loc
    | Acons (loc, _, _) -> loc
    | Atuple (loc, _) -> loc
    | Arecord (loc, _) -> loc
    | Ader (loc, _) -> loc
    | Afn (loc, _, _) -> loc
    | Adot (loc, _, _) -> loc
    | Awith (loc, _, _) -> loc
    | Aapp (loc, _, _) -> loc
    | Aand (loc, _, _) -> loc
    | Aor (loc, _, _) -> loc
    | Alet (loc, _, _, _, _) -> loc
    | Aletrec (loc, _, _) -> loc
    | Aif (loc, _, _, _) -> loc
    | Acase (loc, _, _) -> loc
    | Aty (loc, _, _) -> loc
    | Aseq (loc, _, _) -> loc
    | Aexception (loc, _, _) -> loc
    | Aexcept (loc, _, _) -> loc

let rec set_let_body e body = case e
    | Alet (loc, lhs, t, rhs, e') -> Alet (loc, lhs, t, rhs, set_let_body e' body)
    | Aletrec (loc, ds, e') -> Aletrec (loc, ds, set_let_body e' body)
    | _ -> body


# Module declaration.
datatype md :: {
    body :: ast,
    aliases :: [ (loc, id, [id], tyexp) ],
    types :: [ (loc, id, [id], [(loc, id, tyexp option)]) ],
    exns :: [(loc, id, tyexp)],
}


# Parse module.
let parse init_md tokens :: md->[token]->md =

    let src :: [token] ref = ref tokens # QQQ REMOVE THIS EXPLICIT TYPE

    let infixes :: [(id, (int, bool))] ref =
        ref [
            ("of", (9, false)),
            ("*", (8, true)),
            ("/", (8, true)),
            ("rem", (8, true)),
            ("+", (7, true)),
            ("-", (7, true)),
            ("^", (7, true)),
            (":", (6, false)),
            ("==", (5, true)),
            ("<>", (5, true)),
            ("<", (5, true)),
            (">", (5, true)),
            ("<=", (5, true)),
            (">=", (5, true)),
            (":=", (4, false)),
            ("++", (4, false)),
            ("and", (3, false)),
            ("or", (2, false)),
            ("&", (1, true)),
            ("@", (1, true)),
            ("$", (0, false)),
        ]

    let getloc () = let {loc,_}:_ = !src in loc
    let next () = let t:src' = !src in src := src'; t
    let peek type = let {type=type',_}:_ = !src in type == type'
    let want type = if peek type then next (); true else false
    let unget tok = src := tok : !src

    let need type =
        if peek type then
            next ()
        else
            error (getloc ()) ("need " ^ type)

    let rec csv p delim =
        if want delim then
            []
        else
            p () : (if want "," then csv p delim else need delim; [])

    let rec seq p =
        case p ()
        | Some x -> x : seq p
        | None -> []

    let rec dsv delim p =
        want delim;
        p () : seq p'
        where p' _ = if want delim then Some (p ()) else None

    let rec rep p x =
        case p x
        | Some x' -> rep p x'
        | None -> x

    in

    let

    rec top (st::md) =
        let {loc, type, _} = next ()
        in
        case type
        | "infixl" -> infix_dec true; top st
        | "infixr" -> infix_dec false; top st
        | "datatype" -> top (datatype_dec st)
        | "exception" -> top (exn_dec st)
        | "let" -> top (st with {body = set_let_body st.body (lets "let")})
        | "eof" -> st
        | _ -> error loc "need top-level declaration"

    rec infix_dec lassoc =
        let lvl = atoi (need "int").txt
        in
        rep (\()->
            if peek "id" then
                infixes := ((need "id").txt, (lvl, lassoc)) : !infixes;
                Some ()
            else None)
            ();
        mark_infixes ()

    rec datatype_dec st =
        let {loc, txt=id, _} = need "id"
        let tvs = if want "with" then seq opt_id else []
        in
        if want "::" then
            st with { aliases = st.aliases ++ [(loc, id, tvs, ty ())] }
        else
            st with { types = st.types ++ [(loc, id, tvs, need "="; dsv "|" dc)] }

    rec dc () =
        let {loc, txt=id, _} = need "id"
        in
        (loc, id, opt_ty ())

    rec opt_id () = if peek "id" then Some ((need "id").txt) else None

    rec exn_dec st =
        let {loc, txt=id, _} = need "id"
        let t = if want "with" then ty () else TEtuple (loc, [])
        in
        st with {exns = st.exns ++ [(loc, id, t)]}

    rec lets cont =
        let loc = getloc ()
        let e =
            if want "rec" then
                Aletrec (loc, dsv "rec" dec, dummy ())
            else
                let (_, lhs, te, rhs) = dec ()
                in
                Alet (loc, lhs, te, rhs, dummy ())
        in
        if want cont then
            set_let_body e (lets cont)
        else
            e

    rec dec () = (getloc (), iexp (), dectype (), need "="; exp ())

    rec dectype () =
        if want "::" then
            if want "with" then
                Some (TEscheme (getloc (), seq opt_id, need "in"; ty ()))
            else
                Some (ty ())
        else
            None


    rec exp () =
        let e =
            let {loc, type, _} @ tok = next ()
            in
            case type
            | "let" -> set_let_body (lets "let") (need "in"; exp ())

            | "if" ->
                Aif (loc, exp (), need "then"; exp (), need "else"; exp ())

            | "case" ->
                let e = Acase (loc, exp (), seq rule)
                in
                want "endc";
                e

            | "exception" ->
                let e = Aexception (
                    loc,
                    (need "id").txt,
                    if want "with" then aexp () else Atuple (loc, []))
                in
                want "ende";
                e

            | _ ->
                unget tok;
                iexp ()
        in
        rep (\e->

            case next ()

            | {type="where", _} ->
                let e' = want "def"; set_let_body (lets "def") e
                in
                want "endw";
                Some e'

            | {type="::", loc, _} ->
                Some (Aty (loc, e, ty ()))

            | {type=";", loc, _} ->
                Some (Aseq (loc, e, exp ()))

            | {type="except", loc, _} ->
                Some (Aexcept (loc, e, dsv "|" exceptrule))

            | tok ->
                unget tok;
                None)
            e

    rec iexp () =
        fst (reduce (cexp (), -1, false, seq op_and_arg))

    rec op_and_arg () =
        if peek "infix" or peek "and" or peek "or" then
            let {loc, txt=id, _} = next ()
            let Some (lvl, lassoc) = assoc id !infixes
            in Some (loc, id, lvl, lassoc, cexp ())
        else if want "`" then
            let {loc, txt=id, _} = need "id"
            in Some (loc, id, 9, true, cexp ())
        else
            None

    rec cexp () =
        case (aexp (), seq opt_aexp)
        | (e, []) -> e
        | (f, xs) -> foldl (\f x-> Aapp (aloc f, f, x)) f xs

    rec aexp () =
        let e =
            let {loc, type, txt} = next ()
            in
            case type
            | "int" -> Aint (loc, atoi txt)
            | "char" -> Achar (loc, charat txt 0)
            | "string" -> Astring (loc, txt)
            | "id" -> Aid (loc, txt)
            | "(" ->
                if peek "infix" then
                    let {loc, txt=id, _} = next ()
                    let e = Aid (loc, id)
                    in
                    need ")";
                    e
                else
                    case csv exp ")"
                    | [x] -> x
                    | xs -> Atuple (loc, xs)
                    endc

            | "[" ->
                foldr
                    (\x xs -> Acons (aloc x, x, xs))
                    (Anil loc)
                    (csv exp "]")

            | "{" -> Arecord (loc, csv field "}")
            | "!" -> Ader (loc, aexp ())
            | "\\" ->
                let ps = seq opt_aexp
                let e = (need "->"; exp ())
                in
                want "endf";
                foldr (\p e -> Afn (loc, p, e)) e ps
            | _ -> error loc "need expression"
        in
        rep (\e ->
            let loc = getloc ()
            in
            if want "." then
                Some (Adot (loc, e, (need "id").txt))
            else if want "with" then
                Some (Awith (loc, e, need "{"; csv field "}"))
            else
                None)
            e

    rec opt_aexp () =
        if (case (hd !src).type
            | "int" -> true
            | "char" -> true
            | "string" -> true
            | "id" -> true
            | "(" -> true
            | "[" -> true
            | "{" -> true
            | "!" -> true
            | "\\" -> true
            | _ -> false
            endc)
        then
            Some (aexp ())
        else
            None

    rec field () =
        let {loc, txt=id, _} = need "id"
        let val = if want "=" then exp () else Aid (loc, id)
        in
        (id, val)

    rec reduce (e, lvl, lassoc, rest) =
        case rest
        | [] -> (e, [])
        | (loc, id, lvl', lassoc', e'):rest' ->
            if lvl > lvl' or (lvl == lvl' and lassoc) then
                (e, rest)
            else
                let (e'', rest'') = reduce (e', lvl', lassoc', rest')
                let e''' =
                    case id
                    | "and" -> Aand (loc, e, e'')
                    | "or" -> Aor (loc, e, e'')
                    | ":" -> Acons (loc, e, e'')
                    | _ -> Aapp (loc, Aapp(loc, Aid (loc, id), e), e'')
                    endc
                in
                reduce (e''', lvl, lassoc, rest'')
        endc

    rec rule () =
        if want "|" then
            Some (getloc (), exp (), need "->"; exp ())
        else
            None

    rec exceptrule () =
        let {loc, txt=id, _} = next ()
        let arg = if want "with" then aexp () else Atuple (loc, [])
        in
        (loc, id, arg, need "->"; exp ())

    rec opt_ty () =
        if (case (hd !src).type
            | "id" -> true
            | "(" -> true
            | "[" -> true
            | "{" -> true
            | _ -> false)
        then
            Some (ty ())
        else
            None

    rec ty () =
        let loc = getloc ()
        let t =
            case dsv "and" aty
            | [t] -> t
            | ts -> TEcon (loc, ts, (need "id").txt)
        in
        rep (\t->
            if peek "id" then
                Some (TEcon (loc, [t], (need "id").txt))
            else if want "->" then
                Some (TEfn (loc, t, ty ()))
            else
                None)
            t

    rec aty () =
        let {loc, type, txt} = next ()
        in
        case type
        | "id" -> TEcon (loc, [], txt)

        | "(" ->
            case csv ty ")"
            | [t] -> t
            | ts -> TEtuple (loc, ts)
            endc

        | "{" ->
            TErecord (loc, csv (\_-> ((need "id").txt, need "::"; ty ())) "}")

        | "[" ->
            let t = TEcon (loc, [ty ()], "list")
            in
            need "]";
            t

        | _ -> error loc "need type"
        endc

    rec dummy () = Atuple (getloc (), [])

    rec mark_infixes () =
        src := map mark !src
        where mark tok = case tok.type
        | "id" ->
            if issome (assoc tok.txt !infixes) then
                tok with {type="infix"}
            else
                tok
        | _ -> tok

    in
    mark_infixes ();
    top init_md




#
#   Core Language.
#
#   Types:
#   - records fields are always sorted.
#
#


datatype type =
    | Type (type list, id)
    | Typevar ((id, int, type option) ref)
    | Polyvar (id ref)
    | FnType (type, type)
    | TupType (type list)
    | RecType ((string, type) list)
    | OpenRecType (((string, type) list, type option) ref)

datatype pat =
    | Pint (loc, int)
    | Pchar (loc, char)
    | Pstring (loc, string)
    | Pvar (loc, id)
    | Pcon (loc, id)
    | Pany loc
    | Papp (loc, id, pat)
    | Ptuple (loc, pat list)
    | Precord (loc, (string, pat) list)
    | Popenrecord (loc, (string, pat) list)
    | Pty (loc, pat, type)
    | Pas (loc, pat, id)

datatype exp =
    | Cint (loc, int)
    | Cchar (loc, char)
    | Cstring (loc, string)
    | Cvar (loc, id)
    | Ccon (loc, id)
    | Ctuple (loc, exp list)
    | Crecord (loc, (id, exp) list)
    | Cwith (loc, exp, (id, exp) list)
    | Cfn (loc, id, exp)
    | Capp (loc, exp, exp)
    | Ccase (loc, exp, (loc, pat, exp) list)
    | Clet (loc, id, type option, exp, exp)
    | Cletrec (loc, (loc, id, type option, exp) list, exp)
    | Cexcept (loc, exp, (loc, id, pat, exp) list)
    | Cexception (loc, string, exp)
    | Cty (loc, exp, type)


let rec follow t = case t
    | Typevar !(_, _, Some t') -> t'
    | OpenRecType !(_, (Some t')) -> t'
    | _ -> t

let isfntype t = case t | FnType _ -> true | _ -> false

let istv t = case t | Typevar _ -> true | Polyvar _ -> true | _ -> false

let rec ty2str t =
    case follow t
    | Type ([], id) -> id
    | Type (ts, id) -> join [joinwith " and " (map paren ts), " ", id]
    | Typevar !(id, _, _) -> id
    | Polyvar !id -> id
    | FnType (lhs, rhs) ->
        join [paren lhs, " -> ", if isfntype rhs then paren rhs else ty2str rhs]
    | TupType ts -> join ["(", joinwith "," (map ty2str ts), ")"]
    | RecType fs -> join ["{", joinwith ", " (map field fs), "}"]
    | OpenRecType !([], _) -> "{_}"
    | OpenRecType !(fs, _) -> join ["{", joinwith ", " (map field fs), ", _ }"]
    endc

    where paren t =
        if
            case follow t
            | Type ([], _) -> false
            | Type _ -> true
            | Typevar _ -> false
            | Polyvar _ -> false
            | FnType _ -> true
            | TupType _ -> false
            | RecType _ -> false
            | OpenRecType _ -> false
            endc
        then
            join ["(", ty2str t, ")"]
        else
            ty2str t

    def field (id, t) = join [id, "::", ty2str t]


# Pretty-printing fragments.
datatype pp =
    | PPB           # break point.
    | PPT string    # text fragment.
    | PPJ [pp]      # joined fragments
    | PPG [pp]      # grouped fragments; indent on break

# Width of pretty-printing page.
let page_width = 80

let pretty_print e = fst (output true "\n" page_width (pp e))
    where

    def comma = [PPT ",", PPB]
    def bar = [PPB, PPT "| "]
    def _rec = [PPB, PPT "rec "]
    def blank = PPT ""

    def rec pp e = case e
        | Cint (_, i) -> PPT (itoa i)
        | Cchar (_, c) -> PPT ("'" ^ escape '\'' (chartostr c) ^ "'")
        | Cstring (_, s) -> PPT ("\"" ^ escape '"' s ^ "\"")
        | Cvar (_, id) -> PPT id
        | Ccon (_, id) -> PPT id
        | Ctuple (_, es) -> PPJ [PPT "(", csv (map pp es), PPT ")"]
        | Crecord (_, fs) -> PPJ [PPT "{", csv (map ppf fs), PPT "}"]
        | Cwith (_, e, fs) -> PPG [paren e, PPT "with {", csv (map ppf fs), PPT "}"]
        | Cfn (_, id, e) -> PPG [PPT "\\", PPT id, PPT "->", PPB, pp e]
        | Capp (_, Capp _ @ e, f) -> PPG [pp e, PPB, paren f]
        | Capp (_, e, f) -> PPG [paren e, PPB, paren f]
        | Ccase (_, e, rs) ->
            PPJ [PPT "case ", pp e, PPB,
                PPT "| ", dsv bar (map ppr rs), PPB,
                PPT "endc"]
        | Clet (_, id, _, v, e) ->
            PPJ [PPG [PPT "let ", PPT id, PPT " =", PPB, pp v], PPB, PPT "in", PPB, pp e]
        | Cletrec (_, ds, e) ->
            PPJ [PPT "let rec ", dsv _rec (map ppd ds), PPB, PPT "in", PPB, pp e]
        | Cexcept (_, e, rs) ->
            PPJ [pp e, PPB, PPT "except", PPB, dsv bar (map pper rs), PPB, PPT "ende"]
        | Cexception (_, id, e) ->
            PPJ [PPT "exception ", PPT id, PPB, PPT "with ", pp e]
        endc

    rec paren p =
        if needparen p then
            PPJ [PPT "(", pp p, PPT ")"]
        else
            pp p

    rec needparen p =
        case p
        | Cint _ -> false
        | Cchar _ -> false
        | Cstring _ -> false
        | Cvar _ -> false
        | Ccon _ -> false
        | Ctuple _ -> false
        | Crecord _ -> false
        | Cfn _ -> false

        | Cwith _ -> true
        | Capp _ -> true
        | Ccase _ -> true
        | Clet _ -> true
        | Cletrec _ -> true
        | Cexcept _ -> true
        | Cexception _ -> true
        endc

    rec ppp p = case p
        | Pint (_, i) -> PPT (itoa i)
        | Pchar (_, c) -> PPT ("'" ^ escape '\'' (chartostr c) ^ "'")
        | Pstring (_, s) -> PPT ("\"" ^ escape '"' s ^ "\"")
        | Pvar (_, id) -> PPT id
        | Pcon (_, id) -> PPT id
        | Pany _ -> PPT "_"
        | Papp (_, id, (Papp _) @ p) -> PPG [PPT id, PPB, PPT "(", ppp p, PPT ")"]
        | Papp (_, id, p) -> PPG [PPT id, PPB, ppp p]
        | Ptuple (_, ps) -> PPG [PPT "(", csv (map ppp ps), PPT ")"]
        | Precord (_, fs) -> PPG [PPT "{", csv (map pppf fs), PPT "}"]
        | Popenrecord (_, fs) -> PPG [PPT "{_, ", csv (map pppf fs), PPT "}"]
        | Pty (_, p, t) -> PPG [PPT "(", ppp p, PPT "::", PPT "QQQ", PPT ")"]
        | Pas (_, p, id) -> PPG [PPT "(", ppp p, PPT " @ ", PPT id, PPT ")"]

    rec ppd (loc, id, _, e) = PPJ [PPT id, PPT " =", PPB, pp e]
    rec ppf (id, e) = PPJ [PPT id, PPT " =", PPB, pp e]
    rec pppf (id, p) = PPJ [PPT id, PPT " =", PPB, ppp p]
    rec ppr (loc, lhs, rhs) = PPJ [ppp lhs, PPT " ->", PPB, pp rhs]
    rec pper (loc, id, p, rhs) =
        PPJ [PPT id, PPT " with ", ppp p, PPT " ->", PPB, pp rhs]

    rec dsv sep ps =
        case ps
        | [] -> blank
        | [p] -> p
        | p:ps' -> PPG (p : flatmap (\p-> sep ++ [p]) ps')

    rec csv ps = dsv comma ps

    rec output horiz br remain p =
        case p
        | PPB ->
            if horiz then
                (" ", remain - 1)
            else
                (br, page_width - strlen br - 1)
        | PPT s -> (s, remain - strlen s)
        | PPJ ps ->
            let f = output (remain >= sum' width ps) br
            let g (s, r) p = let (s', r') = f r p in (s':s, r')
            in
            onfst (join of rev) (foldl g ([], remain) ps)
        | PPG ps ->
            let horiz' = remain >= sum' width ps
            let br' = if horiz' then br else br ^ "  "
            let f = output horiz' br'
            let g (s, r) p = let (s', r') = f r p in (s':s, r')
            in
            onfst (join of rev) (foldl g ([], remain) ps)
        endc

    rec width p =
        case p
        | PPB -> 1
        | PPT s -> strlen s
        | PPJ ps -> sum' width ps
        | PPG ps -> sum' width ps
        endc


let make_module (md :: md) =
    # Module building state.
    let types :: [(id, type)] ref = ref []
    let cons :: [(id, type)] ref = ref []
    let exns :: [(id, type)] ref = ref []

    let gensym =
        let uid = ref 0
        in \() -> "$" ^ itoa (incr uid)

    in
        (
            let env = init ()
            in
            apply def_type md.types;
            apply def_alias md.aliases;
            apply def_cons md.types;
            apply def_exn md.exns;
            let body = to_core md.body
            in
            pretty_print body
        )

    where

    rec def_type (loc, id, tv_ids, dcs) =
        if issome (assoc id !types) then
            error loc ("redefining type: " ^ id)
        else
            with_tvs tv_ids \tvs -> [(id, Type (tvs, id))]

    rec def_alias (loc, id, tv_ids, te) =
        if issome (assoc id !types) then
            error loc ("redefining type: " ^ id)
        else
            with_tvs tv_ids \tvs -> [(id, to_type te)]

    rec def_cons (_, dt_id, tv_ids, dcs) =
        let Some dt = assoc dt_id !types
        in
        (with_tvs tv_ids \tvs -> apply def_con dcs; [])

        where def_con (loc, id, te_opt) =
            if issome (assoc id !cons) then
                error loc ("redefining constructor: " ^ id)
            else
                case te_opt
                | Some te -> cons := (id, FnType (to_type te, dt)) : !cons
                | None -> cons := (id, dt) : !cons
                endc
        endw

    rec def_exn (loc, id, te) =
        if issome (assoc id !exns) then
            error loc ("redefining exception: " ^ id)
        else
            exns := (id, to_type te) : !exns

    rec with_tvs tv_ids f =
        let tvs = map (Polyvar of ref) tv_ids
        let old_types = !types
        in
        types := zip tv_ids tvs ++ !types;
        types := f tvs ++ old_types

    rec to_core (ast :: ast) =
        case ast
        | Aint (loc, int) -> Cint (loc, int)
        | Achar (loc, char) -> Cchar (loc, char)
        | Astring (loc, string) -> Cstring (loc, string)

        | Aid (loc, id) ->
            if issome (assoc id !cons) then
                Ccon (loc, id)
            else
                Cvar (loc, id)

        | Anil loc -> Ccon (loc, "[]")

        | Acons (loc, lhs, rhs) ->
            Capp (loc,
                Ccon (loc, ":"),
                Ctuple (loc, [to_core lhs, to_core rhs]))

        | Atuple (loc, es) -> Ctuple (loc, map (to_core) es)

        | Arecord (loc, fs) -> Crecord (loc, map (onsnd (to_core)) fs)

        | Ader (loc, e) ->
            let (vp, ve) = genpair loc
            in
            Ccase (loc, to_core e, [(loc, Papp (loc, "ref", vp), ve)])

        | Afn (loc, lhs, rhs) ->
            case getvar lhs
            | Some id -> Cfn (loc, id, to_core rhs)
            | None ->
                let v = gensym ()
                let body =
                    Ccase (loc, Cvar (loc, v), [(loc, to_pat lhs, to_core rhs)])
                in
                Cfn (loc, v, body)
            endc

        | Adot (loc, e, id) ->
            let (vp, ve) = genpair loc
            let p = Popenrecord (loc, [(id, vp)])
            in
            Ccase (loc, to_core e, [(loc, p, ve)])

        | Awith (loc, e, fs) ->
            Cwith (loc, to_core e, map (onsnd (to_core)) fs)

        | Aapp (loc, f, x) ->
            Capp (loc, to_core f, to_core x)

        | Aand (loc, lhs, rhs) ->
            from_if loc (to_core lhs) (to_core rhs) (Ccon (loc, "false"))

        | Aor (loc, lhs, rhs) ->
            from_if loc (to_core lhs) (Ccon (loc, "true")) (to_core rhs)

        | Alet (loc, lhs, te, rhs, body) ->
            case to_dec loc lhs te rhs
            | Some (loc, id, t, val) -> Clet (loc, id, t, val, to_core body)
            | None -> Ccase (loc, to_core rhs, [(loc, to_pat lhs, to_core body)])
            endc

        | Aletrec (loc, ds, e) ->
            let ds' = map (\(loc, lhs, te, rhs)-> to_dec loc lhs te rhs) ds
            let check d =
                case d
                | (Some ((loc, id, t, rhs) @ dec)) ->
                    if isfn rhs then
                        dec
                    else
                        error loc "rec only defines functions"
                | None -> error loc "rec only defines functions"
            in
            Cletrec (loc, map check ds', to_core e)

        | Aif (loc, c, t, f) ->
            from_if loc (to_core c) (to_core t) (to_core f)

        | Acase (loc, e, rs) ->
            let rs' = map (\(loc, lhs, rhs)-> (loc, to_pat lhs, to_core rhs)) rs
            in
            Ccase (loc, to_core e, rs')

        | Aty (loc, e, te) -> Cty (loc, to_core e, to_type te)

        | Aseq (loc, e, f) -> Ccase (loc, to_core e, [(loc, Pany loc, to_core f)])

        | Aexception (loc, id, e) -> Cexception (loc, id, to_core e)

        | Aexcept (loc, e, rs) ->
            let rs' =
                map (\(loc, id, arg, rhs)-> (loc, id, to_pat arg, to_core rhs)) rs
            in
            Cexcept (loc, to_core e, rs')

        endc

    rec to_dec loc lhs te rhs =
        let t = mapopt to_type te
        in
        case getvar lhs

        | Some id -> Some (loc, id, t, to_core rhs)

        | None ->
            case app2dec lhs
            | Some (id, ps) ->
                Some (loc, id, t, to_core (foldr (\p e-> Afn (loc, p, e)) rhs ps))
            | None -> None
            endc

    rec from_if loc c t f =
        Ccase (loc, c, [(loc, Pcon (loc, "false"), f), (loc, Pcon (loc, "true"), t)])

    # Convert from app form to (id, [param]) for l.h.s. of let.
    rec app2dec ast = gather ast []
        where rec gather ast ps =
            case ast
            | Aapp (loc, Aapp _ @ lhs, rhs) -> gather lhs (rhs:ps)
            | Aapp (loc, Aid (_, id), rhs) -> Some (id, rhs:ps)
            | _ -> None
            endc
        endw

    rec to_pat ast =
        case ast
        | Aint (loc, int) -> Pint (loc, int)
        | Achar (loc, char) -> Pchar (loc, char)
        | Astring (loc, string) -> Pstring (loc, string)

        | Aid (loc, id) ->
            if issome (assoc id !cons) then
                Pcon (loc, id)
            else
                Pvar (loc, id)

        | Anil loc -> Pcon (loc, "[]")

        | Acons (loc, lhs, rhs) ->
            Papp (loc, ":", Ptuple (loc, [to_pat lhs, to_pat rhs]))

        | Atuple (loc, ps) -> Ptuple (loc, map (to_pat) ps)
        | Arecord (loc, fs) -> Precord (loc, map (onsnd (to_pat)) fs)

        | Ader (loc, p) -> Papp (loc, "ref", to_pat p)

        | Aapp (loc, Aapp(_, Aid (_, "@"), lhs), Aid (_, id)) ->
            Pas (loc, to_pat lhs, id)

        | Aapp (loc, Aapp(_, Aid (_, "@"), _), _) ->
            error loc "r.h.s. of @ must be identifier"

        | Aapp (loc, f, x) ->
            case getvar f
            | Some id ->
                if issome (assoc id !cons) then
                    Papp (loc, id, to_pat x)
                else
                    invalid_pat ast
            | _ -> invalid_pat ast
            endc

        | Aty (loc, p, te) -> Pty (loc, to_pat p, to_type te)


        | Afn _ -> invalid_pat ast
        | Adot _ -> invalid_pat ast
        | Awith _ -> invalid_pat ast
        | Aand _ -> invalid_pat ast
        | Aor _ -> invalid_pat ast
        | Alet _ -> invalid_pat ast
        | Aletrec _ -> invalid_pat ast
        | Aif _ -> invalid_pat ast
        | Acase _ -> invalid_pat ast
        | Aseq _ -> invalid_pat ast
        | Aexception _ -> invalid_pat ast
        | Aexcept _ -> invalid_pat ast
        endc

    rec invalid_pat ast =
        error (aloc ast) "invalid pattern"

    rec isfn exp = case exp | Cfn _ -> true | _ -> false

    rec getvar ast = case ast | Aid (_, id) -> Some id | _ -> None

    rec to_type te = case te

        | TEcon (loc, tes, id) ->

            case assoc id !types

            | Some (Type (ts, id')) ->
                if samelength ts tes then
                    Type (map to_type tes, id')
                else
                    error loc ("mismatched type args: " ^ id)

            | Some type ->
                if tes <> [] then
                    error loc ("not a type constructor: " ^ id)
                else
                    type

            | None -> error loc ("undefined type: " ^ id)

            endc

        | TEtuple (loc, tes) -> TupType (map to_type tes)

        | TErecord (loc, fs) ->
            let fs' = map (onsnd to_type) fs
            let fs'' = sort (\(id, _) (id', _) -> strcmp id id' <= 0) fs'
            in

            case dups (map fst fs'')
            | [] -> ()
            | dups -> error loc ("duplicate fields: " ^ joinwith ", " dups)
            endc;

            RecType fs''

        | TEfn (loc, lhs, rhs) -> FnType (to_type lhs, to_type rhs)

        | TEscheme (loc, tv_ids, te) ->
            let tvs = map (Polyvar of ref) tv_ids
            let old_types = !types
            let _ = types := zip tv_ids tvs ++ !types
            let type = to_type te
            let _  = types := old_types
            in
            type

        endc


    rec genpair loc = let id = gensym () in (Pvar (loc, id), Cvar (loc, id))

    rec init () =
        let a = Polyvar (ref "a")
        let unittype = TupType []
        let booltype = Type ([], "bool")
        let inttype = Type ([], "int")
        let chartype = Type ([], "char")
        let stringtype = Type ([], "string")
        let listtype = Type ([a], "list")
        let optiontype = Type ([a], "option")
        let reftype = Type ([a], "ref")
        in

        types := [
            ("bool", booltype),
            ("int", inttype),
            ("char", chartype),
            ("string", stringtype),
            ("list", listtype),
            ("option", optiontype),
            ("ref", reftype),
        ];

        cons := [
            ("false", booltype),
            ("true", booltype),
            ("None", optiontype),
            ("Some", FnType (a, optiontype)),
            ("[]", listtype),
            (":", FnType (TupType [a, listtype], listtype)),
            ("ref", reftype),
        ];

        exns := [
            ("match", a),
            ("division", inttype),
            ("exponent", inttype),
            ("index", inttype),
            ("size", inttype),
            ("empty", unittype),
        ];

        let f x y = FnType (x, y)
        let f2 x y z = FnType (x, f y z)
        let f3 x y z w = FnType (x, f2 y z w)
        let f4 x y z w v = FnType (x, f3 y z w v)
        let f5 x y z w v u = FnType (x, f4 y z w v u)
        let u = unittype
        let b = booltype
        let i = inttype
        let c = chartype
        let s = stringtype
        let l x = Type ([x], "list")
        let o x = Type ([x], "option")
        let r x = Type ([x], "ref")
        in
        [
            ("+", f2 i i i),
            ("-", f2 i i i),
            ("*", f2 i i i),
            ("/", f2 i i i),
            ("rem", f2 i i i),
            ("**", f2 i i i),
            ("<", f2 i i b),
            (">", f2 i i b),
            ("<=", f2 i i b),
            (">=", f2 i i b),
            ("==", f2 a a b),
            ("<>", f2 a a b),
            ("ord", f c i),
            ("chr", f i c),
            ("charcmp", f2 c c i),
            ("implode", f (l c) s),
            ("chartostr", f c s),
            ("^", f2 s s s),
            ("join", f (l s) s),
            ("strlen", f s i),
            ("charat", f2 s i c),
            ("strsplice", f4 s i i (l s) s),
            ("substr", f3 s i i s),
            ("strcmp", f2 s s i),
            ("substrcmp", f5 s i s i i i),
            ("findsubstr", f5 s i s i i (o i)),
            ("findchar", f3 s i c (o i)),
            ("atoi", f s i),
            ("itoa", f i s),
            (":=", f (r a) a),
            ("exit", f i a),
            ("prn", f a a),
            ("srand", f i u),
            ("rand", f u i),
            ("getenviron", f u (l $ TupType [s, s])),
            ("sysopen", f3 s i i i),
            ("sysclose", f i i),
            ("sysread", f2 i i s),
            ("syswrite", f2 i s i),
        ]


let path = "test.al"
let path = "std.al"
let path = "paperml.al"
let Some src = read_file path

let init_md = {
    aliases = [],
    types = [],
    exns = [],
    body = Atuple ("", []),
}

let main = tokenise path src & parse init_md & make_module & print
