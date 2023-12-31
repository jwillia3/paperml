infixl 10
infixl 9    # `
infixr 9 of
infixl 8 * / rem
infixl 7 + - ^
infixr 6 :
infixl 5 == <> <= >= < >
infixr 4 := ++
infixr 3 # and
infixr 2 # or
infixl 1 @
infixr 0 $

# let of f g x = f (g x)
#
# let rec foldr f y xs =
#     case xs
#     | [] -> y
#     | x:xs' -> f x (foldr f y xs')
#
# let xs ++ ys = foldr (:) ys xs
#
# let flatten xss = foldr (++) [] xss
#
# let map f xs = foldr ((:) of f) [] xs
#
# let flatmap f xs = flatten (map f xs)

let main = ()
