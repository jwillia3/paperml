# Grammar
```
top ::= (<idec> | <tdec> | <let>)+
idec ::= ('inifxl' | 'infixr') <int> <id>*                  Infix declaration
tdec ::=
  'datatype' <id> ['with' <id>*] '::' <ty>                  Type alias
  'datatype' <id> ['with' <id>*] '=' ['|'] cdec ('|' cdec)  Type dec
cdec ::= <id> <aty>                                         Data constructor dec
let ::=
    <let>+                                                  Sequential decs
    'let' <dec>                                             Single dec
dec ::=
    <pat> '=' <exp>                                         Value dec
    <fdec>
    ('rec' <fdec>)+                                         Recursive dec
fdec ::=
    <fdec> '|' <fdec>
    <id> <pat>+ '=' <exp>
    <pat> <infix> <pat> '=' <exp>
exp ::=
    <let> 'in' <exp>                                        Local dec
    'if' <exp> 'then' <exp> 'else' <exp>                    Condition exp
    'case' <exp> <rule>* ['endc']                           Case exp
    <iexp>                                                  Infix exp
    <exp> '::' <ty>                                         Constraint
    <exp> 'where' <dec> ('also' <dec>)* 'endw'              Local dec
    <exp> ';' <exp>                                         Sequence
iexp ::=
    <cexp> (<infix> <cexp>)*                                Infix application
    <cexp> ('and'|'or') <cexp>                              Logic exp
cexp ::= <aexp>* <aexp>                                     Application exp
aexp ::=
    <literal>                                               Literal value
    <var>                                                   Variable
    <con>                                                   Constructor
    '(' {<exp>} ')'                                         Tuple
    '[' {<exp>} ']'                                         List
    <rec>                                                   Record
    '!' <aexp>                                              Dereference
    '\' <apat>+ '->' <exp> ('|' <apat>+ '->' <exp>)*        Function
    <aexp> '.' <id>                                         Field access
    <aexp> 'with' <rec>                                     Record update
rec ::= '{' {<id> ['=' <exp>]} '}'                          Record
rule ::= '|' <pat> ['if' <exp>] '->' <exp>                  Case rule
pat ::=
    apat                                                    Simple pattern
    <id> <apat>*                                            Constructor app
    <cexp> '@' id                                           Layered pat
    <pat> <infix> <pat>                                     Infix app
    <pat> '::' <ty>                                         Constraint
apat ::=
    <literal>                                               Literal value
    '_'                                                     Any pattern
    <var>                                                   Variable
    <con>                                                   Constructor const
    '(' {<pat>} ')'                                         Tuple
    '[' {<pat>} ']'                                         List
    '{' {<id> ['=' <pat>]} '}'                              Record
ty :: =
    <aty> ('and' <aty>)* <tycon>                            Type constructor
    <aty> '->' <ty>                                         Function constructor
    <aty>                                                   Simple type
aty ::=
    <var>                                                   Type variable
    <tycon>                                                 0-Type constructor
    '(' {<ty>} ')'                                          Tuple
    '[' <ty> ']'                                            List
    '{' {<id> '::' <ty>} '}'                                Record
literal ::=
    <int>
    <char>
    <string>
id ::= /[a-zA-Z_][a-zA-Z0-9_']*|[%&$+-/:<=>?@~^|*]/ (not reserved)
int ::= /-?[0-9]*/
string ::= /"(\.|[^"])*"/       (escapes: \a \b \e \f \n \r \t \v \xHH)
char ::= /'(\.|[^'])'/

[] indicates optional
{} indicates a comma-separated list (optionally with trailing comma)
() indicates grouping
* indicates one or more repetition
+ indicates at least one repetition

reserved:
    ! -> :: = also and case datatype else endc endw if
    in infixl infixr let or rec then with where |


punctuation:
    ! ( ) [ ] { } , . ; \
```

## Infixes
```
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
infixl 1 & @
infixr 0 $
```
