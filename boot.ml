infixl 7 * / rem
infixl 6 + - ^
infixr 5 : ++
infixl 4 == <> < > <= >=
infixr 3 &&
infixr 2 ||
infixr 1 := +=
infixl 0 <<
infixr 0 $

# datatype (a) option = NONE | SOME (a);
# datatype bool = false | true
# datatype (a) ref = ref (a)

let print x = pr x; pr '\n'; x

let += r n = r := !r + n

let not true = false
    --  false = true

let of f g x = f (g x)
let const x _ = x
let identity x = x
let flip f x y = f y x
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let negate x = 0 - x
let $ f x = f x
let << x f = f x
let ^ x y = join [x, y]

let fst (x, _) = x
let snd (_, x) = x

let tap x f = f x; x

let map_option f (SOME x)   = SOME (f x)
    --         _ NONE       = NONE

let fold_option f (SOME x)  = f x
    --          _ NONE      = NONE

let for_option (SOME x) f _ = f x
    --          NONE _ g = g ()

let app_option do! option = for_option option do! (const ())

let filter_option accept (SOME x) = if accept x then SOME x else NONE
--                _      NONE     = NONE

let valueof (SOME x) = x

let isnone (SOME _) = false
    --      NONE    = true

let issome (SOME _) = true
    --      NONE    = false

let space? c = c == ' ' || c == '\t' || c == '\n' || c == '\f' ||
    c == '\r' || c == '\v'
let digit? c = let c = ord c in c >= 48 && c <= 57
let lower? c = let c = ord c in c >= 97 && c <= 122
let upper? c = let c = ord c in c >= 65 && c <= 90
let alpha? c = let c = ord c in c >= 97 && c <= 122 || c >= 65 && c <= 90
let alnum? c = let c = ord c in c >= 97 && c <= 122 ||
    c >= 65 && c <= 90 || c >= 48 && c <= 57
let tolower c = if upper? c then chr (ord c + 32) else c
let toupper c = if lower? c then chr (ord c - 32) else c

let hd (x : _) = x
let tl (_ : x) = x

let length list =
    count list 0
    where rec count (_ : rest) n    = count rest (n + 1)
    --              [] n            = n

let reverse list =
    loop list []
    where rec loop (x : rest) out   = loop rest (x : out)
    --              [] out          = out

let foldl combine leftmost list =
    loop list leftmost
    where rec loop (x : rest) out   = loop rest (combine out x)
          --       [] out            = out

let foldl_self combine (first : rest) =
    foldl combine first rest

let foldr combine rightmost list =
    foldl (flip combine) rightmost (reverse list)

let sum list = foldl (+) 0 list

let rec same_length (_ : x) (_ : y) = same_length x y
--                  [] []           = true
--                  _ _             = false

let zip left right =
    loop left right []
    where rec loop (x : x') (y : y') out    = loop x' y' ((x, y) : out)
    --          _ _ out                     = reverse out

let map transform list =
    loop list []
    where rec loop (x : rest) out   = loop rest (transform x : out)
    --              _         out   = reverse out

let mapi transform list =
    loop list 0 []
    where rec loop (x : rest) i out = loop rest (i + 1) (transform i x : out)
    --              _         _ out = reverse out

let rec app do! (x : rest) = do! x; app do! rest
--          _   []         = ()

let rec appi do! list = loop 0 list
where rec loop i (x : rest) = do! i x; loop (i + 1) rest
--             _ []         = ()


let append lhs rhs = foldr (:) rhs lhs
let ++ = append

let flatten list_of_lists = foldr append [] list_of_lists

let flatmap transform list = flatten (map transform list)

let replicate size value = loop size []
where rec loop  n out = if n <= 0 then out else loop (n - 1) (value : out)

let filter true_for list =
    foldr (fn new rest -> if true_for new then new : rest else rest) [] list

let any? true_for list = loop list
    where rec loop (x : rest) = true_for x || loop rest
    --             []         = false

let all? true_for list = loop list
    where rec loop (x : rest) = true_for x && loop rest
    --             []         = true

let none? true_for list = not (true_for `any? list)

let contains? list item = (== item) `any? list

let find true_for list = loop list
    where rec loop (x : rest) = if true_for x then SOME x else loop rest
    --             []         = NONE

let assoc key alist = find ((== key) `of fst) alist

let lookup key alist = map_option snd (assoc key alist)

let separate_map
-- _         transform [x]        = [transform x]
-- separator transform (x : rest) = transform x : flatmap (fn i -> [separator, transform i]) rest
-- _         _         []         = []

let separate separator list = separate_map separator identity list

let explode string = loop (size string) []
    where rec loop 0 out = out
              --   n out  = loop (n - 1) (string `char_at (n - 1) : out)

let split delim string = loop 0 []
    where rec loop i out =
        case findstr string i delim
        | SOME j -> loop (j + size delim) (substr string i j : out)
        | NONE   -> reverse (substr string i -1 : out)


let itoa n = if n < 0 then "-" ^ loop (negate n) []
             else if n == 0 then "0"
             else loop n []
    where rec loop 0 out = implode out
              --   n out = loop (n / 10) ("0123456789" `char_at (n rem 10) : out)


let atoi str = if str `char_at 0 == '-' then 0 - loop 1 0 else loop 0 0
    where rec loop i out =  if i >= size str then
                                out
                            else
                                let c = ord (str `char_at i) in
                                if c >= 48 || c <= 57 then
                                    loop (i + 1) (out * 10 + c - 48)
                                else out

let leftpad desired_width str = join (adjust (desired_width - size str))
where adjust diff = replicate diff " " ++ [str]


let rightpad desired_width str = join (adjust (desired_width - size str))
where adjust diff = [str] ++ replicate diff " "

let center desired_width str =
    if size str < desired_width then
        let margin = desired_width / 2 + size str / 2 + 1 in
        rightpad desired_width (leftpad margin str)
    else str

let contains_char string char = loop 0
where rec loop i = i < size string && (string `char_at i == char || loop (i + 1))

let unescape str = join (reverse (loop 0 []))
where rec loop i out =
    case findstr str i "\\"
    | NONE   -> substr str i -1 : out
    | SOME j -> replace i j out
and replace i j out =
    let prefix  = substr str i j in
    let (j', s) = case str `char_at (j + 1)
                  | '0' -> (2, "\0")
                  | 'a' -> (2, "\a")
                  | 'b' -> (2, "\b")
                  | 'e' -> (2, "\e")
                  | 'f' -> (2, "\f")
                  | 'n' -> (2, "\n")
                  | 'r' -> (2, "\r")
                  | 't' -> (2, "\t")
                  | 'v' -> (2, "\v")
                  | 'x' -> (4, fromhex (char_at str) (j + 2))
                  | c   -> (2, implode [c])
    in loop (j + j') (s : prefix : out)
and fromhex f i = implode [chr (hexdigit (f i) * 16 + hexdigit (f (i + 1)))]
and hexdigit c = if digit? c then ord c - 48 else
                 if alpha? c then ord (tolower c) - 97 else 0


let escape quote str = join (reverse (loop 0 []))
where rec loop i out =
    case find_escape i
    | (j, "") ->    substr str i j : out
    | (j, repl) ->  loop (j + 1) (repl : substr str i j : out)
and find_escape i =
    if i < size str then
        case str `char_at i
        | '\0' ->       (i, "\\0")
        | '\a' ->       (i, "\\a")
        | '\b' ->       (i, "\\b")
        | '\e' ->       (i, "\\e")
        | '\f' ->       (i, "\\f")
        | '\n' ->       (i, "\\n")
        | '\r' ->       (i, "\\r")
        | '\t' ->       (i, "\\t")
        | '\v' ->       (i, "\\v")
        | '\"' ->       (i, "\\\"")
        | '\\' ->       (i, "\\\\")
        | c ->          if c == quote then
                            (i, implode ['\\', quote])
                        else find_escape (i + 1)
    else (i, "")
