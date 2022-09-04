infixl 7 * / rem
infixl 6 + - ^
infixr 5 :
infixl 4 == <> < > <= >=
infixr 3 &&
infixr 2 ||
infixr 1 :=
infixl 0 <<
infixr 0 $

# datatype bool = false | true
datatype (a) option = NONE | SOME (a);

ref 1 == ref 1
