
#
#   Contents.
#   -   Exceptions
#   -   Datatypes
#   I.   Misc.
#           print
#   II.  Higher-Order and Support Functions
#           of
#           $
#           &
#           not
#           neg
#           identity
#           const
#           flip
#           uncurry
#           curry
#           fst
#           snd
#           onfst
#           onsnd
#           equal
#           notequal
#           incr
#           decr
#   III. Numeric and Logical Functions
#           min
#           max
#   IV.  Optional Value Function
#           issome
#           isnone
#           valof
#           getval
#           mapopt
#           foldopt
#   IV.  List Functions
#           hd
#           tl
#           singleton
#           apply
#           rev
#           foldl
#           foldl'
#           foldr
#           foldr'
#           ++
#           flatten
#           map
#           flatmap
#           tabulate
#           range
#           maximum
#           minimum
#           sum
#           sum'
#           last
#           length
#           assoc
#           all
#           any
#           none
#           contains
#           find
#           filter
#           partition
#           splitby
#           takewhile
#           dropwhile
#           take
#           drop
#           nth
#           splitn
#           zip
#           unzip
#           mergesort
#           stablesort
#           sort
#           shuffle
#           uniq
#           dups
#   V.   String Functions
#           isdigit
#           islower
#           isupper
#           isalpha
#           isalnum
#           isspace
#           findchar
#           findstr
#           findstr'
#           split
#           joinwith
#           unescape
#           escape
#           read_file




exception hd
exception tl
exception value
exception invalid_escape with string
exception different_lengths


#
#   I. Misc.
#

let print x = prn x; prn '\n'; x




#
#   II. Higher-Order and Support Functions
#

let (of) f g x = f (g x)

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

let onfst f (x, y) = (f x, y)

let onsnd f (x, y) = (x, f y)

let equal x y = x == y

let notequal x y = x <> y

let incr r = r := !r + 1

let decr r = r := !r - 1




#
#   III. Numeric and Logical Functions
#

let min x y = if x < y then x else y

let max x y = if x > y then x else y




#
#   IV. Optional Value Functions
#

let issome x = case x | Some _ -> true | _ -> false

let isnone x = case x | None -> true | _ -> false


# Unconditionally get the value of an option.
let valof (Some x) = x


# Get value of option or return default.
let getval default x = case x
| Some x -> x
| None -> default


# Apply a function to the value of an option otherwise return None.
let mapopt f x = case x | Some x -> Some (f x) | None -> None


# Apply a function to the value of an option otherwise return default.
let foldopt f default o = case o | Some x -> f x | None -> default




#
# V. List Functions
#

let hd xs = case xs | (x:_) -> x | _ -> exception empty

let tl xs = case xs | (_:xs') -> xs | _ -> exception empty

let singleton x = [x]

let apply f xs :: with a b in (a->b)->[a]->() =
    loop xs
    where
    rec loop xs = case xs
    | x:xs' -> f x; loop xs'
    | [] -> ()

let rev xs = loop xs []
    where
    rec loop xs out = case xs
    | x:xs' -> loop xs' (x:out)
    | [] -> out

let foldl f y xs :: with a b in (a->b->a)->a->[b]->a =
    loop xs y
    where
    rec loop xs y = case xs
    | x:xs -> loop xs (f y x)
    | [] -> y

let foldl' f (x:xs) :: with a in (a->a->a)->[a]->a =
    foldl f x xs

let foldr f y xs :: with a b in (a->b->b)->b->[a]->b =
    foldl (flip f) y (rev xs)

let foldr' f xs :: with a in (a->a->a)->[a]->a =
    foldl' (flip f) (rev xs)

let xs ++ ys = foldr (:) ys xs

let flatten xss = foldr (++) [] xss

let map f xs = foldr ((:) of f) [] xs

let flatmap f xs = flatten (map f xs)

# Create a list of length n calling f with each index.
let tabulate n f :: with a in int->(int->a)->[a] =
    loop 0 []
    where
    rec loop i out =
        if i < n then
            loop (i + 1) (f i : out)
        else
            rev out

let range i j = tabulate (j - i) \x-> x + i

let maximum (x:xs) = foldl max x xs

let minimum (x:xs) = foldl min x xs

# Sum the list of integers.
let sum xs = foldl (+) 0 xs


# Sum the integers returned by applying f to each element of the list.
let sum' f xs = foldl (\n x -> n + f x) 0 xs


# Return the last element of the list or raise exception `empty`.
let rec last xs = case xs
    | [x] -> x
    | _:xs' -> last xs'
    | [] -> exception empty


# Return the length of a list.
let length xs = foldl (\n _ -> n + 1) 0 xs


# Return true if the lists are the same length.
let samelength xs ys = loop xs ys
    where
    rec loop xs ys = case (xs, ys)
    | (_:xs', _:ys') -> loop xs' ys'
    | ([], []) -> true
    | ([], _) -> false
    | (_, []) -> false


# Return the value of an item in an association list (alist).
let assoc id xs = loop xs where
    rec loop xs = case xs
    | (id', val) : xs' -> if id == id' then Some val else loop xs'
    | [] -> None


# Return true if all elements match predicate p. An empty list is true.
let all p xs = loop xs where
    rec loop xs = case xs
    | x:xs' -> p x and loop xs'
    | [] -> true


# Return true if any elements match predicate p. An empty list is false.
let any p xs = loop xs where
    rec loop xs = case xs
    | x:xs' -> p x or loop xs'
    | [] -> false


# Return true if none of the elements match a predicate p. An empty list is true.
let none p xs = not (any p xs)


# Return true if there is an element that is equal (==) to y.
let contains xs y = any ((==) y) xs


# Find the first element that matches predicate p.
let find p xs = loop xs where
    rec loop xs = case xs
    | x:xs' -> if p x then Some x else loop xs'
    | [] -> None


# Return a list of elements that match predicate p.
let filter p xs = foldr (\x out -> if p x then x : out else out) [] xs

# Return a pair where all of the elements that match predicate p are on the
# left and all of the elements that do not match predicate p are on the right.
# `(filter p xs, filter (not of p) xs)`

let partition p xs :: with a in (a->bool)->[a]->([a], [a]) =
    foldr f ([], []) xs
    where f x (as, bs) = if p x then (x:as, bs) else (as, x:bs)



# Find the point in the list where predicate p does not match.
# Return a pair of the elements before that point on the left and
# all of the elements after that point on the right.
# `(takewhile p xs, dropwhile p xs)`

let splitby p xs =
    loop xs [] where
    rec loop xs out = case xs
    | x:xs' -> if p x then loop xs' (x:out) else (rev out, xs)
    | _ -> (rev out, xs)


# Return the initial elements that match predicate p.
let takewhile p xs = fst (splitby p xs)


# Remove the initial elements that do not match predicate p.
let dropwhile p xs = loop xs where
    rec loop xs = case xs
    | x:xs' -> if p x then loop xs' else xs
    | [] -> xs


# Return the first n elements of the list.
# If n is greater than the length of xs, the exception `size` is raised.
let rec take n xs =
    if n <= 0 then
        []
    else
        case xs
        | x:xs' -> x : take (n - 1) xs'
        | [] -> []


# Return the list without the first n elements.
# If n is greater than the length of xs, the exception `size` is raised.
let drop n xs = loop n xs where
    rec loop i xs =
        if i <= 0 then
            xs
        else
            case xs
            | _:xs' -> loop (i - 1) xs'
            | [] -> exception size with n


# Get the nth' element of the list.
let nth xs n =
    case drop n xs
    | x:_ -> x
    | [] -> exception empty


# Split the list at the given index.
let splitn n xs = (take n xs, drop n xs)

# Combine two lists into a list of pairs.
# If one list is shorter, the exception `different_lengths` is raised.
let zip xs ys :: with a b in [a]->[b]->[(a, b)] =
    loop (xs, ys) []
    where
    rec loop pair out = case pair
    | (x:xs, y:ys) -> loop (xs, ys) ((x, y):out)
    | ([], []) -> rev out
    | (_, _) -> exception different_lengths

# Convert a list of pairs into a pair of lists.
let unzip pairs = foldr (\(x, y) (xs, ys) -> (x:xs, y:ys)) ([], []) pairs


# Sort a list with the mergesort algorithm.
# (<=) is the function used to compare elements.
# This is a stable sort.
let mergesort (<=) xs = hd (sort (map singleton xs)) where
    rec sort xs = case xs
    | (as:bs:xs') -> sort (merge as bs [] : sort xs')
    | _ -> xs

    rec merge as bs out = case (as, bs)
    | ([], _) -> rev (rev bs ++ out)
    | (_, []) -> rev (rev as ++ out)
    | (a:as', b:bs') -> if a <= b then merge as' bs (a:out) else merge as bs' (b:out)


# Use a stable sort to sort the list.
# (<=) is the function used to compare elements.
let stablesort (<=) xs = mergesort (<=) xs


# Sort a list.
# (<=) is the function used to compare elements.
# This sort may not be stable.
let sort (<=) xs = stablesort (<=) xs


# Shuffle a list.
let shuffle xs = map snd (sort (\a b -> fst a <= fst b) (map (\x-> (rand (), x)) xs))


# Return duplicate values from a SORTED list.
let dups xs = loop xs []
    where rec loop xs out = case xs
    | x:y:xs' ->
        if x == y then
            loop (dropwhile (equal x) xs') (x:out)
        else
            loop xs' out
    | _ -> rev out

# Return unique values from a SORTED list.
let uniq xs = loop xs []
    where rec loop xs out = case xs
    | x:y:xs' ->
        if x == y then
            loop (dropwhile (equal x) xs') out
        else
            loop (y:xs') (x:out)
    | [x] -> rev (x:out)
    | [] -> rev out




#
#   V. String Functions
#

let isdigit c = '0' `charcmp c <= 0 and c `charcmp '9' <= 0
let islower c = 'a' `charcmp c <= 0 and c `charcmp 'z' <= 0
let isupper c = 'A' `charcmp c <= 0 and c `charcmp 'Z' <= 0
let isalpha c = islower c or isupper c
let isalnum c = isalpha c or isdigit c
let isspace c = c == ' ' or c == '\t' or c == '\n'

let findchar src c = findchar' src 0 c
let findstr src item = findsubstr src 0 item 0 -1
let findstr' src i item = findsubstr src i item 0 -1

let split delim str = loop 0 (findstr' str 0 delim)
    where rec loop i opt =
    case opt
    | Some j ->
        x:xs
        where
        def i' = j + strlen delim
        def x = substr str i (j - i)
        def xs =
            if i' < strlen str then
                loop i' (findstr' str i' delim)
            else
                [""]
    | None -> [substr str i -1]

let joinwith sep xs = case xs
    | [] -> ""
    | [x] -> x
    | x:xs -> join (x : flatmap (\i-> [sep, i]) xs)
    endc

let unescape src =
    loop 0 []
    where rec loop i out =
        if i < strlen src then
            case findchar' src i '\\'
            | None -> join (rev (substr src i -1 : out))
            | Some j ->
                let s = substr src i (j - i)
                let (i', s2) = unesc (j + 1)
                in
                loop i' (s2 : s : out)
        else
            join (rev out)
    rec unesc i =
        if i >= strlen src then
            exception invalid_escape with src
        else
            case charat src i
            | 'a' -> (i + 1, "\a")
            | 'b' -> (i + 1, "\b")
            | 'e' -> (i + 1, "\e")
            | 'f' -> (i + 1, "\f")
            | 'n' -> (i + 1, "\n")
            | 'r' -> (i + 1, "\r")
            | 't' -> (i + 1, "\t")
            | 'v' -> (i + 1, "\v")
            | 'x' ->
                if i + 2 >= strlen src then
                    exception invalid_escape with src
                else
                    let x = findchar "0123456789abcdef" (charat src (i + 1))
                    let y = findchar "0123456789abcdef" (charat src (i + 2))
                    let s =
                        case (x, y)
                        | (Some x, Some y) -> chartostr (chr (x*16 + y))
                        | _ -> exception invalid_escape with src
                    in
                    (i + 3, s)
            | c -> (i + 1, chartostr c)

# QQQ
let escape quote src = src


let read_file path =
    case sysopen path O_RDONLY 0
    | -1 -> None
    | fd -> Some (readall [])
        where
        rec readall out =
            case sysread fd 65536
            | "" -> join (rev out)
            | part -> readall (part : out)
        endw
