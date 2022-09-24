
[ ] Exceptions
[ ] Print closure origin

## Grammar
```
script: infix/datatype/'let' ['rec'] let ...

infix:      'infixl'/'inifxr' INT ID...

datatype:   'datatype' ['(' ID ')'] ID '=' condecs
condecs:    '|' ID ['(' type ')']

let:        ['and'] decs
decs:       aexpr (fnrules | '=' expr) ['and' decs]
fnrules:    aexpr aexpr... '=' expr [where...] ['--' fnrules]

where:      'where' ['rec'] '(' let ')'
            'where' ['rec'] let

expr:       expr '@' ID
            expr '::' type
            expr ';' expr
            iexpr
            let ['rec'] 'in' expr
            'case' rules
            'if' expr 'then' expr 'else' expr
iexpr:      aexpr... aexpr [['`'] ID iexpr]
aexpr:      INT / CHAR / STRING
            ID
            '(' expr,... ')'
            '[' expr,... ']'
            '!' aexpr
            'fn' fnrules
rules:      '|' expr '->' expr [rules]

type:       (ID / '(' type,... ')') ID... ['->' type]

ID:         [a-zA-Z0-9_][a-zA-Z0-9_'?!]
PUN:        ( ) [ ] , ; `
SYMBOL:     ! $ % & * + - / : < = > @ ^ | ~
RESV:       ! -- -> :: = and case datatype else fn if in
            infixl infixr let rec then where |
ESCAPES:    \0 \a \b \e \f \n \r \t \v \xHH
```

## Abstract Machine
| Instruction   | Pre-Condition         | Post-Condition        |
|---------------|-----------------------|-----------------------|
| HLT           | v.S                   | * return to caller    |
| LIT n         |                       | v.S                   |
| NIL           |                       | v.S                   |
| CONS          | w.v.S                 | (v:w).S               |
| VAR n         | n...E                 | v.S                   |
| CLOS c        |                       | clos(c,E).S           |
| RET           | v.e.c.S               | C=c E=e v.S           |
| APP           | w.v.S                 | E.C.S C=w(c) E=v.w(e) |
| TAIL          | w.v.S                 | C=w(c) E=v.w(e)       |
| POP           | v.S                   | S                     |
| AS            | v.S                   | v.S E=v.e             |
| LET           | v.S                   | v.E S                 |
| REC n         | n...v.S               | S n...v.E             |
| DROP          | v.S                   | S                     |
| JMP n         |                       | C=n                   |
| BRF n         | v.S                   | C=n if v==false       |
| PEQ n         | v.S                   | S (see matching)      |
| PTUP          | v.S                   | ...v.S (see matching) |
| PCON          | v.S                   | hd.tl.S (see matching)|
| PDAT n        | v.S                   | S (see matching)      |
| LAST          | v.S                   | P=(S,E,0) v.S         |
| PAT c         | v.S                   | p=(S,E,c) v.S         |
| DREF          | v.S                   | (!v).S                |

Matching:
- Each pattern recogniser examines the value on the top of the stack
- Each recogniser pops the value from the stack (except `AS`)
- If the recogniser accepts, execution continues
- If the recogniser rejects, jump to P(C)
- If P(C) is 0, crash
- Before each rule, P is set with `LAST` or `PAT` to set P

## Notes
- New typevars must start with underscore
