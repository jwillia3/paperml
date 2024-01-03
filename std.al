let print x = prn x; prn '\n'; x

let of f g x = f (g x)
let f $ x = f x
let x & f = f x
let not x = if x then false else true
let neg x = 0 - x
let min x y = if x < y then x else y
let max x y = if x > y then x else y
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

let incr r = r := !r + 1
let decr r = r := !r - 1

let even n = n rem 2 == 0
let odd n = n rem 2 == 1

let log2i n = loop 0 1 where
    rec loop exp val =
        if n < val then exp - 1
        else loop (exp + 1) (val + val)

let issome (Some _) = true
let isnone None = true
let valof (Some x) = x

let getval dflt x = case x
| Some x -> x
| None -> dflt

let singleton x = [x]

let apply f xs = loop xs where
    rec loop xs = case xs
    | x:xs' -> f x; loop xs'
    | [] -> ()

let rev xs = loop xs [] where
    rec loop xs out = case xs
    | x:xs' -> loop xs' (x:out)
    | [] -> out

let foldl f y xs = loop xs y where
    rec loop xs y = case xs
    | x:xs -> loop xs (f y x)
    | [] -> y

let foldl' f (x:xs) = foldl f x xs

let foldr f y xs = foldl (flip f) y (rev xs)

let foldr' f xs = foldl' (flip f) (rev xs)

let xs ++ ys = foldr (:) ys xs

let flatten xss = foldr (++) [] xss

let map f xs = foldr ((:) of f) [] xs

let flatmap f xs = flatten (map f xs)

let tabulate n f = loop 0 [] where
    rec loop i out =
        if i < n then
            loop (i + 1) (f i : out)
        else
            rev out

let range i j = tabulate (j - i) \x-> x + i

let maximum (x:xs) = foldl max x xs

let minimum (x:xs) = foldl min x xs

let rec last xs = case xs
    | [x] -> x
    | _:xs' -> last xs'

let filter p xs = foldr (\x out -> if p x then x : out else out) [] xs

let find p xs = loop xs where
    rec loop xs = case xs
    | x:xs' -> if p x then Some x else loop xs'
    | [] -> None

let all p xs = loop xs where
    rec loop xs = case xs
    | x:xs' -> p x and loop xs'
    | [] -> true

let any p xs = loop xs where
    rec loop xs = case xs
    | x:xs' -> p x or loop xs'
    | [] -> false

let none p xs = not (any p xs)

let partition p xs = foldr f ([], []) xs
    where f x (as, bs) = if p x then (x:as, bs) else (as, x:bs)

let splitby p xs = loop xs [] where
    rec loop xs out = case xs
    | x:xs' -> if p x then loop xs' (x:out) else (rev out, xs)
    | _ -> (rev out, xs)

let takewhile p xs = fst (splitby p xs)

let dropwhile p xs = loop xs where
    rec loop xs = case xs
    | x:xs' -> if p x then loop xs' else xs
    | [] -> xs

let rec take n xs =
    if n <= 0 then []
    else let x:xs' = xs in x : take (n - 1) xs'

let rec drop n xs =
    if n <= 0 then xs
    else let _:xs' = xs in drop (n - 1) xs'

let splitn n xs = (take n xs, drop n xs)

let zip xs ys = loop (xs, ys) []
    where rec loop pair out = case pair
    | (x:xs, y:ys) -> loop (xs, ys) ((x, y):out)
    | (_, _) -> rev out

let unzip pairs = foldr (\(x, y) (xs, ys) -> (x:xs, y:ys)) ([], []) pairs

let length xs = foldl (\n _ -> n + 1) 0 xs

let mergesort (<=) xs = hd (sort (map singleton xs)) where
    rec sort xs = case xs
    | (as:bs:xs') -> sort (merge as bs [] : sort xs')
    | _ -> xs

    rec merge as bs out = case (as, bs)
    | ([], _) -> rev (rev bs ++ out)
    | (_, []) -> rev (rev as ++ out)
    | (a:as', b:bs') -> if a <= b then merge as' bs (a:out) else merge as bs' (b:out)

let stablesort (<=) xs = mergesort (<=) xs

let sort (<=) xs = stablesort (<=) xs

let shuffle xs = map snd (sort (\a b -> fst a <= fst b) (map (\x-> (rand (), x)) xs))

#
# String Functions
#

let isdigit c = '0' `charcmp c <= 0 and c `charcmp '9' <= 0
let islower c = 'a' `charcmp c <= 0 and c `charcmp 'z' <= 0
let isupper c = 'A' `charcmp c <= 0 and c `charcmp 'Z' <= 0
let isalpha c = islower c or isupper c
let isalnum c = isalpha c or isdigit c
let isspace c = c == ' ' or c == '\t' or c == '\n'

let findstr src item = findsubstr src 0 item 0 -1
let findstr' src i item = findsubstr src i item 0 -1

let split delim str = loop 0 (findstr' str 0 delim)
    where
    rec loop    i   (Some j)    =   x:xs where
                                    also i' = j + strlen delim
                                    also x = substr str i (j - i)
                                    also xs =
                                        if i' < strlen str then
                                            loop i' (findstr' str i' delim)
                                        else
                                            [""]
    |   loop    i   None        =   [substr str i -1]
