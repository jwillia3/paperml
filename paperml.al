let error loc msg =
    print (join ["paperml.al: error ", loc, ": ", msg]);
    exit 1

datatype token :: {loc::string, txt::string, type::string}

let (tokenise :: string -> string -> [token]) = \path src ->
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
        "!",
        "->",
        "::",
        "=",
        "and",
        "case",
        "datatype",
        "def",
        "else",
        "endc",
        "endw",
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
    let issym c = issome (findchar "%&$+-/:<=>?@~^|*" 0 c)
    let ispun c = issome (findchar "!()[]{},.;\\`" 0 c)
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

datatype id :: string
datatype loc :: string

datatype tyexp =
    | TEcon (loc, tyexp list, id)
    | TEtuple (loc, tyexp list)
    | TErecord (loc, (id, tyexp) list)
    | TEfn (loc, tyexp, tyexp)

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
    | Aapp (loc, ast, ast list)
    | Aand (loc, ast, ast)
    | Aor (loc, ast, ast)
    | Alet (loc, ast, ast, ast)
    | Aletrec (loc, (ast, ast) list, ast)
    | Aif (loc, ast, ast, ast)
    | Acase (loc, ast, (ast, ast) list)
    | Aty (loc, ast, tyexp)
    | Aseq (loc, ast, ast)

let locof ast = case ast
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
    | Alet (loc, _, _, _) -> loc
    | Aletrec (loc, _, _) -> loc
    | Aif (loc, _, _, _) -> loc
    | Acase (loc, _, _) -> loc
    | Aty (loc, _, _) -> loc
    | Aseq (loc, _, _) -> loc

let rec set_let_body e body = case e
    | Alet (loc, lhs, rhs, e') -> Alet (loc, lhs, rhs, set_let_body e' body)
    | Aletrec (loc, ds, e') -> Aletrec (loc, ds, set_let_body e' body)
    | _ -> body


datatype dtd =
    | DTalias (loc, [id], tyexp)
    | DTdec (loc, [id], [(loc, id, tyexp option)])

datatype pstate :: {e::ast, dtds::[dtd]}

let parse (tokens::[token]) =
    let src = ref tokens

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

    let rec rep p x =
        case p x
        | Some x' -> rep p x'
        | None -> x

    in

    let

    rec top (st::pstate) =
        let {loc, type, _} = next ()
        in
        case type
        | "infixl" -> infix_dec true; top st
        | "infixr" -> infix_dec false; top st
        | "datatype" -> top (datatype_dec st)
        | "let" -> top (st with {e = set_let_body st.e (_let "let")})
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
        let tvs =
            if want "with" then
                seq \()-> if peek "id" then Some ((need "id").txt) else None
            else
                []
        let dec =
            if want "::" then
                DTalias (loc, tvs, ty ())
            else
                DTdec (loc, tvs, need "="; want "|"; dc () : seq dc')
        in
        st with {dtds = st.dtds ++ [dec]}

    rec dc' () =
        if want "|" then Some (dc ()) else None

    rec dc () =
        let {loc, txt=id, _} = need "id"
        in
        (loc, id, opt_ty ())

    rec _let cont =
        let loc = getloc ()
        let e =
            if peek "rec" then
                let ds = seq (\()-> if want "rec" then Some (dec ()) else None)
                in
                Aletrec (loc, ds, dummy ())
            else
                let (lhs, rhs) = dec ()
                in
                Alet (loc, lhs, rhs, dummy ())
        in
        if want cont then
            set_let_body e (_let cont)
        else
            e

    rec dec () = (exp (), need "="; exp ())

    rec exp () =
        let e =
            let {loc, type, _} @ tok = next ()
            in
            case type
            | "let" -> set_let_body (_let "let") (need "in"; exp ())

            | "if" ->
                Aif (loc, exp (), need "then"; exp (), need "else"; exp ())

            | "case" ->
                let e = Acase (loc, exp (), seq rule)
                in
                want "endc";
                e

            | _ ->
                unget tok;
                iexp ()
        in
        rep (\e->

            case next ()

            | {type="where", _} ->
                let e' = want "def"; set_let_body (_let "def") e
                in
                want "endw";
                Some e'

            | {type="::", loc, _} ->
                Some (Aty (loc, e, ty ()))

            | {type=";", loc, _} ->
                Some (Aseq (loc, e, exp ()))

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
        | (f, xs) -> Aapp (locof f, f, xs)

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
                    (\x xs -> Acons (locof x, x, xs))
                    (Anil loc)
                    (csv exp "]")

            | "{" -> Arecord (loc, csv field "}")
            | "!" -> Ader (loc, aexp ())
            | "\\" ->
                let ps = seq opt_aexp
                let e = (need "->"; exp ())
                in
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
                    | _ -> Aapp (loc, Aid (loc, id), [e, e''])
                    endc
                in
                reduce (e''', lvl, lassoc, rest'')
        endc

    rec rule () = if want "|" then Some (exp (), need "->"; exp ()) else None

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
            case aty () : seq (\()-> if want "and" then Some (aty ()) else None)
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
    top {e=dummy (), dtds=[]}


let path = "test.al"
let path = "std.al"
let path = "paperml.al"
let Some src = read_file path

let main = tokenise path src & parse & print
