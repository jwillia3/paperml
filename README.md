Grammar
-------

```
top ::= (<idec> | <dtd> | <exndec> <let>)+

idec ::= ('inifxl' | 'infixr') <int> <id>*

dtd ::=
  'datatype' <id> ['with' <id>*] '::' <ty>
  'datatype' <id> ['with' <id>*] '=' ['|'] <dcd>

dcd ::= <id> <aty> ['|' <dcd>]

excndec ::= 'exception' <id> ['with' <ty>]

let ::= ('let' <dec>)+

dec ::=
    <pat> '=' <exp>
    ('rec' <pat> '=' <exp>)+

exp ::=
    <let> 'in' <exp>
    'if' <exp> 'then' <exp> 'else' <exp>
    'case' <exp> ('|' <pat> '->' <exp>)* ['endc']
    'exception' ['with' <aexp>]
    <iexp>
    <exp> '::' <ty>
    <exp> 'where' <dec> ('def' <dec>)* 'endw'
    <exp> ';' <exp>
    <exp> 'except' (<id> ['with' <apat>] '->' <exp>)+ ['ende']

iexp ::= <cexp> (<infix> <cexp>)*

cexp ::= <aexp>* <aexp>

aexp ::=
    <literal>
    <id>
    '(' {<exp>} ')'
    '[' {<exp>} ']'
    <rec>
    '!' <aexp>
    '\' <apat>+ '->' <exp>
    <aexp> '.' <id>
    <aexp> 'with' <rec>

literal ::=
    <int>
    <char>
    <string>

rec ::= '{' {<id> ['=' <exp>]} '}'

pat ::=
    apat
    <id> <apat>*
    <cexp> '@' id
    <pat> <infix> <pat>
    <pat> '::' <ty>

apat ::=
    <literal>
    '_'
    <id>
    '(' {<pat>} ')'
    '[' {<pat>} ']'
    '{' {<id> ['=' <pat>]} '}'

ty :: =
    <aty> ('and' <aty>)* <tycon>
    <aty> '->' <ty>
    <aty>

aty ::=
    <var>
    <tycon>
    '(' {<ty>} ')'
    '[' <ty> ']'
    '{' {<id> '::' <ty>} '}'

id ::=
    /[a-zA-Z_][a-zA-Z0-9_']*/
    /[%&$+-/:<=>?@~^|*]/
    (not reserved)

int ::= /-?[0-9]*/

string ::= /"(\.|[^"])*"/
    (escapes: \a \b \e \f \n \r \t \v \xHH)

char ::= /'(\.|[^'])'/

reserved:
    -> :: = |
    and case datatype def else endc ende endw except exception
    if in infixl infixr let or rec then with where

punctuation:
    ! ( ) [ ] { } , . ; \ `

[] indicates optional
{} indicates a comma-separated list (optionally with trailing comma)
() indicates grouping
* indicates one or more repetition
+ indicates at least one repetition

```

Infixes
-------

```
Lvl Right   Left
10
9   of      `
8           * / rem
7           + - ^
6   :
5           == <> <= >= < >
4   := ++
3   and
2   or
1           & @
0   $
```

'and', 'or', and ':' are treated specially as infixes.


Exceptions
----------

Exceptions are raised with `exception ID` or `exception ID with val`.

Exceptions must be declared beforehand.

Exceptions may not be polymorphic.
Consider, at location [A], it cannot be determined what type x is.

```
datatype wild with a = a
exception bad with wild

...

(
    exception bad with 0;
    exception bad with ""
)
except bad with x -> x # [A]
```
