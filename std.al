let print x = prn x; prn '\n'; x

let of f g x = f (g x)
let f $ x = f x
let x & f = f x
let not x = if x then false else true
let neg x = 0 - x
let identity x = x
let const k _ = k
let flip f x y = f y x
let uncurry f x y = f (x, y)
let curry f (x, y) = f x y
let fst (x, _) = x
let snd (_, y) = y
let (:) hd tl = hd : tl
let hd (x:_) = x
let tl (_:xs) = xs
let equal x y = x == y
let notequal x y = x <> y

let issome (Some _) = true
let isnone None = true
let valof (Some x) = x

let getval _ (Some x)   = x
|   getval x _          = x

let singleton x = [x]

let apply f xs = loop xs where
    rec loop (x:xs) = f x; loop xs
    |   loop [] = ()

let rev xs = loop xs [] where
    rec loop (x:xs') out = loop xs' (x : out)
    |   loop [] out = out

let foldr f y xs = loop xs where
    rec loop [] = y
    |   loop (x:xs') = f x (loop xs')

let xs ++ ys = foldr (:) ys xs

let flatten xss = foldr (++) [] xss

let map f xs = foldr ((:) of f) [] xs

let flatmap f xs = flatten (map f xs)

let tabulate n f = loop 0 where
    rec loop i  if i < n    = f i : loop (i + 1)
    |   loop _ = []

let rec last [x] = x
    |   last (_:xs') = last xs'

let filter p xs = foldr (\x out -> if p x then x : out else out) [] xs

let find p xs = loop xs where
    rec loop (x:_)  if p x  = Some x
    |   loop (_:xs)         = loop xs
    |   loop []             = None

let rec all p [] = true
    |   all p (x:xs) = p x and all p xs

let rec any p [] = false
    |   any p (x:xs) = p x or any p xs

let none p xs = not (any p xs)

let partition p xs = foldr f ([], []) xs
    where   f x (as, bs) if p x = (x:as, bs)
    |       f x (as, bs)        = (as, x:bs)

let splitby p xs = loop xs [] where
    rec loop (x:xs')    out if p x  = loop xs' (x:out)
    |   loop xs         out         = (rev out, xs)

let takewhile p xs = fst (splitby p xs)

let dropwhile p xs = loop xs where
    rec loop (x:xs') if p x = loop xs'
    |   loop xs             = xs

let mergesort (<=) xs = hd (sort (map singleton xs)) where
    rec sort (as:bs:xs') = sort (merge as bs : sort xs')
    |   sort xs          = xs
    rec merge []        bs                  = bs
    |   merge as        []                  = as
    |   merge (a:as)    (b:bs) if a <= b    = a : merge as (b:bs)
    |   merge as        (b:bs)              = b : merge as bs

let stablesort (<=) xs = mergesort (<=) xs
