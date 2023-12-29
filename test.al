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

datatype BOOL = FALSE | TRUE
datatype LIST with a = NIL | CONS a (a LIST)
datatype MAP with a b = NOMAP | MAP a b (a and b MAP)

datatype QWERTY with a :: [string]

let _ = ! !abc
