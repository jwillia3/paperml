#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct { int len; char chars[]; } string;
typedef struct { char *name; int line, col; } location;
typedef struct node node;
typedef struct nodes { node *node; struct nodes *next; } nodes;
typedef struct value value;
typedef struct values values;
typedef struct type type;
typedef struct types { type *type; struct types *next; } types;
typedef struct typesub { type *from, *to; struct typesub *next; } typesub;
typedef struct infix { char *id; int lhs, rhs; struct infix *next; } infix;
typedef struct fnrules fnrules;
typedef struct senv senv;

typedef enum {
    // Keep in sync with tokname.
    TEOF, TLPAREN, TRPAREN, TLBRACE, TRBRACE, TCOMMA, TBACK,
    TSEMI, TINT, TCHAR, TSTRING, TID, TEQUAL, TFN, TARROW, TLET,
    TREC, TAND, TIN, TCASE, TBAR, TIF, TTHEN, TELSE, TTYPING,
    TINFIXL, TINFIXR, TDATATYPE, TMORE, TDEREF,
} toktype;

char *tokname[] = {"end of file", "(", ")", "[", "]", ",", "`",
    ";", "int", "char", "string", "id", "=", "fn", "->", "let",
    "rec", "and", "in", "case", "|", "if", "then", "else", "::",
    "infixl", "infixr", "datatype", "--", "!", 0 };

struct value {
    enum { NIL, INT, CHAR, STRING, TUPLE, CONS, DATA, CLOSURE, NOVAL, } form;
    union {
        int integer;
        string *str;
        struct tuple *tup;
        struct cons *cons;
        struct data *data;
        struct clos *clos;
        void *hack;
    };
};

struct tuple {
    int len;
    value vals[];
};

struct cons {
    value hd;
    value tl;
};

struct data {
    char *constr;
    value arg;
};

struct clos {
    int c;
    values *e;
    uint8_t arity;
    uint8_t remain;
    uint8_t op;
};

struct values { value val; values *next; };

struct node {
    enum { ELIT, EVAR, ETUP, ENIL, ECONS, EFN,
        EAPP, ELET, EREC, ECASE, EIF, ETYPING, ESEQ, EDEREF, } form;
    location loc;
    union {
        struct { value val; type *type; } lit;
        nodes *tup;
        struct { char *id; int index; };
        struct { node *lhs, *rhs; };
        struct { char *id, *par; node *body; } fn;
        struct { nodes *decs; node *body; } let;
        struct { node *subject; nodes *rules; } _case;
        struct { node *a, *b, *c; } _if;
        struct { node *body; type *type; } typing;
        node *deref;
    };
};

struct type {
    enum { TYPEVAR, REGULAR, FNTYPE, TUPTYPE } form;
    char *id;
    type *inst;
    types *args;
};

struct senv {
    char *id;
    type *type;
    value val;
    senv *next;
};

struct fnrules {
    location loc;
    nodes *params;
    node *body;
    fnrules *next;
};

#define emit(X) (program[ec] = X, ec++)
#define emiti(X, Y) (emit(X), emit(Y))

typedef enum opcode { IHLT, ILIT, ITUP, INIL, ICONS, IDAT, IVAR,
    ICLOS, IRET, IAPP, ITAIL, IPOP, ILET, IREC, IDROP, IJMP,
    IBRF, IPEQ, IPTUP, IPCON, IPDAT, ILAST, IPAT, IDER,
} opcode;
struct { char *name; bool arg; } instructions[] = {
    {"HLT",0}, {"LIT",1}, {"TUP",1}, {"NIL",0}, {"CONS",0},
    {"DAT",1}, {"VAR",1}, {"CLOS",1}, {"RET",0}, {"APP",0},
    {"TAIL",0}, {"POP",0}, {"LET",0}, {"REC",1}, {"DROP",1},
    {"JMP",1}, {"BRF",1}, {"PEQ",1}, {"PTUP",0}, {"PCON",0},
    {"PDAT",1}, {"LAST",0}, {"PAT",1}, {"DER",0},
};
#define NINSTR ((int) (sizeof instructions / sizeof *instructions))

typedef enum native_op { NADD=1, NSUB, NMUL, NDIV, NREM, NLT, NGT,
    NLE, NGE, NEQ, NNE, NPR, NSET,
} native_op;





string      *interns[65536];
int         ninterns;
location    sloc;
char        *source, *src, *sol;
toktype     token;
bool        peeked;
int         tokint;
char        tokbuf[65536];
string      *tokstr;
infix       *infixes;
infix       backquote_infix = {"`", 9, 10, NULL};
type        *booltype, *inttype, *chartype, *stringtype;
types       *nongenerics;
senv        *all_types;
senv        *constrs;
char        *list_id, *cons_id, *ref_id, *ignore_id;
value       nil = {NIL}, noval = {NOVAL, {0}}, unit, _true, _false;
int         program[8 * 1024 * 1024];
int         ec; // `c` that we're emitting at.
value       constants[1024 * 1024];
int         nconstants;
int         stack_size = 1024 * 1024;





#define new(TYPE, ...)\
    ((TYPE*) memcpy(malloc(sizeof (TYPE)), &(TYPE){__VA_ARGS__}, sizeof (TYPE)))

string *newstr(int len, char *chars) {
    if (len < 0 && chars) len = strlen(chars);
    string *out = malloc(sizeof (string) + len + 1);
    out->len = len;
    if (chars) memcpy(out->chars, chars, len);
    out->chars[len] = 0;
    return out;
}

string *intern(int len, char *chars) {
    if (len < 0) len = strlen(chars);
    for (string **i = interns + ninterns; i-- > interns; )
        if ((*i)->len == len && !memcmp((*i)->chars, chars, len))
            return *i;
    return interns[ninterns++] = newstr(len, chars);
}

#define cstr(CHARS) intern(-1, CHARS)->chars




#define value(F,...) (value) {F, __VA_ARGS__}
#define values(VAL, NEXT) new(values, VAL, NEXT)
#define hack(X) value(NOVAL, .hack=X)
#define theint(X) value(INT, .integer=X)
#define thechar(X) value(CHAR, .integer=X)
#define thestr(X) value(STRING, .str=X)
#define thetup(X) value(TUPLE, .tup=X)
#define thecons(X) value(CONS, .cons=X)
#define thedata(X) value(DATA, .data=X)
#define theclos(X) value(CLOSURE, .clos=X)



#define valtype(X) X.form
#define iscons(X) (valtype(X) == CONS)
#define isdata(X) (valtype(X) == DATA)
#define isnoval(X) (valtype(X) == NOVAL)



// Value extractors.
// These do not check whether the type is correct.
#define intval(X) (X).integer
#define charval(X) (X).integer
#define strval(X) (X).str
#define tupval(X) (X).tup
#define consval(X) (X).cons
#define closval(X) (X).clos
#define dataval(X) (X).data
#define hackval(X) (X).hack
#define size(X) strval(X)->len
#define chars(X) strval(X)->chars
#define tuplen(X) tupval(X)->len
#define tupat(X, N) tupval(X)->vals[N]
#define hd(X) consval(X)->hd
#define tl(X) consval(X)->tl
#define datacon(X) dataval(X)->constr
#define dataarg(X) dataval(X)->arg



value newtup(int len, value *vals) {
    struct tuple *tup = malloc(sizeof *tup + len * sizeof *vals);
    tup->len = len;
    if (vals) memcpy(tup->vals, vals, len * sizeof *vals);
    return thetup(tup);
}

#define cons(HD, TL) thecons(new(struct cons, HD, TL))
#define newclos(C, E) theclos(new(struct clos, C, E, 0, 0, 0))
#define newnative(E,ARITY,LEFT,OP) theclos(new(struct clos,0,E,ARITY,LEFT,OP))

#define newdata(CONSTR, ARG) thedata(new(struct data, CONSTR, ARG))


void print_escaped(int len, char *chars, int quote) {
    static char *escapes[256] = {[0] = "\\0", ['\a'] = "\\a",
        ['\b'] = "\\b", ['\033'] = "\\e", ['\f'] = "\\f",
        ['\n'] = "\\n", ['\r'] = "\\r", ['\t'] = "\\t",
        ['\v'] = "\\v", };
    char *end = chars + len;
    for (char *i = chars; i < end; i++) {
        char *base = i;
        while (i < end && *i >= 32 &&
            *i != quote && !escapes[(uint8_t) *i])
            { i++; }
        fwrite(base, 1, i - base, stdout);
        if (i == end) break;
        if (*i == quote) { putchar('\\'), putchar(quote); continue; }
        char *seq = escapes[(uint8_t) *i];
        if (seq) fputs(seq, stdout);
        else printf("\\x%02x", (uint8_t) *i);
    }
}

void printval(value val) {
    switch (valtype(val)) {
    case INT:
        printf("%d", intval(val));
        break;
    case CHAR:
        putchar('\'');
        print_escaped(1, (char[]) {charval(val)}, '\'');
        putchar('\'');
        break;
    case STRING:
        putchar('"');
        print_escaped(size(val), chars(val), '"');
        putchar('"');
        break;
    case TUPLE:
        putchar('(');
        for (int i = 0; i < tuplen(val); i++)
            i? fputs(", ", stdout): 0,
            printval(tupat(val, i));
        putchar(')');
        break;
    case NIL:
        fputs("[]", stdout);
        break;
    case CONS:
        putchar('[');
        for (value i = val; iscons(i); i = tl(i))
            printval(hd(i)),
            iscons(tl(i))? fputs(", ", stdout): 0;
        putchar(']');
        break;
    case DATA:
        fputs(datacon(val), stdout);
        if (!isnoval(dataarg(val)))
            putchar('('),
            printval(dataarg(val)),
            putchar(')');
        break;
    case CLOSURE:
        fputs("(closure)", stdout);
        break;
    case NOVAL:
        fputs("***NOVAL***", stdout);
        break;
    }
}

bool equal(value a, value b) {
    if (valtype(a) != valtype(b)) return false;
    switch (valtype(a)) {
    case INT:   return intval(a) == intval(b);
    case CHAR:  return charval(a) == charval(b);
    case STRING:
        return strval(a) == strval(b) ||
            (size(a) == size(b) && !memcmp(chars(a), chars(b), size(a)));
    case TUPLE:
        for (int i = 0; i < tuplen(a); i++)
            if (!equal(tupat(a, i), tupat(b, i))) return false;
        return true;
    case NIL:   return true;
    case CONS:
        while (iscons(a) && iscons(b))
            if (!equal(hd(a), hd(b))) return false;
        return iscons(a) == iscons(b);
    case CLOSURE:
        return closval(a) == closval(b);
    case DATA:
        if (dataval(a) == dataval(b)) return true;
        if (datacon(a) == ref_id) return false;
        return datacon(a) == datacon(b) && equal(dataarg(a), dataarg(b));
    case NOVAL:
    }
    return false;
}



// Type Constructors.

#define type(F, ...) new(struct type, F, __VA_ARGS__)
#define types(T, N) new(struct types, T, N)
#define typesub(FROM, TO, NEXT) new(struct typesub, FROM, TO, NEXT)
#define regular(ARG, ID) type(REGULAR, .id=ID, .args=ARG? types(ARG, 0): 0)
#define typevar() type(TYPEVAR)
#define fntype(FROM,TO) type(FNTYPE, .args=types(FROM, types(TO, 0)))
#define tuptype(TYPES) type(TUPTYPE, .args=TYPES)



#define senv(ID, TYPE, VAL, NEXT) new(struct senv, ID, TYPE, VAL, NEXT)
#define infix(ID, LHS, RHS, NEXT) new(struct infix, ID, LHS, RHS, NEXT)
#define fnrules(L, PARAMS, BODY, NEXT) new(struct fnrules, L,PARAMS,BODY,NEXT)



#define node(F,L,...) new(struct node, F, .loc=L, __VA_ARGS__)
#define nodes(HD, TL) new(struct nodes, HD, TL)
#define elit(L, X, TYPE) node(ELIT, L, .lit={X, TYPE})
#define econstr(L, X, TYPE) node(ELIT, L, .lit={X, TYPE})
#define evar(L, X) node(EVAR, L, .id=X)
#define etup(L, X) node(ETUP, L, .tup=X)
#define enil(L) node(ENIL, L)
#define econs(L, HD, TL) node(ECONS, L, .lhs=HD, .rhs=TL)
#define ederef(L, BODY) node(EDEREF, L, .deref=BODY)
#define efn(L, ID, PAR, BODY) node(EFN, L, .fn={ID, PAR, BODY})
#define eapp(L, LHS, RHS) node(EAPP, L, .lhs=LHS, .rhs=RHS)
#define elet(L, DECS, BODY) node(ELET, L, .let={DECS, BODY})
#define erec(L, DECS, BODY) node(EREC, L, .let={DECS, BODY})
#define ecase(L, SUBJECT, DECS) node(ECASE, L, ._case={SUBJECT, DECS})
#define eif(L, A, B, C) node(EIF, L, ._if={A, B, C})
#define etyping(L, BODY, TYPE) node(ETYPING, L, .typing={BODY, TYPE})
#define eseq(L, LHS, RHS) node(ESEQ, L, .lhs=LHS, .rhs=RHS)




_Noreturn void _fatal(location loc, char *msg, va_list ap) {
    printf("boot: error %s:%d:%d: ", loc.name, loc.line, loc.col);
    vprintf(msg, ap);
    putchar('\n');
    exit(1);
}

_Noreturn void syntax(char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    _fatal(sloc, msg, ap);
}

_Noreturn void semantic(node *e, char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    _fatal(e->loc, msg, ap);
}

_Noreturn void fatal(location loc, char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    _fatal(loc, msg, ap);
}

string *readfile(char *fn) {
    FILE *file = fopen(fn, "rb");
    if (!file) return 0;
    fseek(file, 0, SEEK_END);
    int size = ftell(file);
    rewind(file);
    string *out = newstr(size, 0);
    fread(out->chars, 1, size, file);
    fclose(file);
    return out;
}

void opensrc(char *fn) {
    sloc = (location) {cstr(fn), 1, 1};
    string *tmp = readfile(fn);
    if (!tmp) syntax("cannot open source file");
    source = src = sol = tmp->chars;
}

int hex_escape(void) {
    static char hex[256] = {['0']=1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
        ['a']=11, 12, 13, 14, 15, 16, ['A']=11, 12, 13, 14, 15, 16};
    char a = hex[(uint8_t) *src];
    char b = hex[(uint8_t) (a? src[1]: 0)];
    if (!a || !b)
        sloc.col = src - sol + 1,
        syntax("invalid hex escape");
    src += 2;
    return (a - 1) * 16 + b - 1;
}

int character(void) {
    if (*src == '\\')
        switch ((src += 2)[-1]) {
        case '0': return '\0';
        case 'a': return '\a';
        case 'b': return '\b';
        case 'e': return '\033';
        case 'f': return '\f';
        case 'n': return '\n';
        case 'r': return '\r';
        case 't': return '\t';
        case 'v': return '\x0b';
        case 'x': return hex_escape();
        default: return src[-1];
        }
    return *src++;
}

toktype next(void) {
    if (peeked)
        return peeked = false, token;

    for ( ; isspace(*src) || *src == '#'; src++)
        if (*src == '\n') sloc.line++, sloc.col = 1, sol = src + 1;
        else if (*src == '#') while (src[1] && src[1] != '\n') src++;

    sloc.col = src - sol + 1;

    if (!*src) return token = TEOF;

    if (isdigit(src[*src == '-'])) {
        tokint = strtol(src, &src, 0);
        return token = TINT;
    }

    for (toktype t = TLPAREN; t <= TSEMI; t++)
        if (*src == tokname[t][0]) return src++, token = t;

    if (*src == '\'') {
        src++;
        tokint = character();
        if (*src++ != '\'') syntax("unclosed character");
        return token = TCHAR;
    }

    if (*src == '"') {
        char *t = tokbuf;
        src++;
        while (true)
            if (*src == '"') {
                tokstr = intern(t - tokbuf, tokbuf);
                return src++, token = TSTRING;
            } else if (!*src) syntax("unclosed string");
            else *t++ = character();
    }

    char *base = src, *symbol = "!$%&*+-/:<=>@^|~";
    if (isalnum(*src) || *src == '_')
        while (*src && (isalnum(*src) || strchr("_'?!", *src))) src++;
    if (src == base) while (*src && strchr(symbol, *src)) src++;
    if (src == base) syntax("illegal token: %c", *src);

    tokstr = intern(src - base, base);

    for (toktype t = TID + 1; tokname[t]; t++)
        if (tokname[t] == tokstr->chars) return token = t;
    return token = TID;
}

#define peek(WANT) (next(), peeked = true, token == WANT)
#define want(WANT) (peeked = next() != WANT, !peeked)
#define need(WANT) ((void) (want(WANT)? 0: syntax("need %s", tokname[WANT])))
#define getloc() ((void) peek(0), sloc)




node *expr(void);
node *aexpr(bool required);
type *typ(void);

senv *lookup(senv *env, char *id, int *index) {
    for (senv *i = env; i; i = i->next)
        if (i->id == id) return i;
        else if (isnoval(i->val) && index) (*index)++;
    return 0;
}

infix *peek_infix(void) {
    if (want(TBACK)) {
        need(TID);
        peeked = true;
        return infix(tokstr->chars, 9, 10, 0);
    }
    if (!peek(TID)) return false;
    for (infix *i = infixes; i; i = i->next)
        if (i->id == tokstr->chars) return i;
    return 0;
}

nodes *csv(toktype delim) {
    if (want(delim)) return 0;
    node    *hd = expr();
    return nodes(hd, want(TCOMMA)? csv(delim): (need(delim), NULL));
}

nodes *sequence(node *get(bool first)) {
    node *x = get(false);
    return x? nodes(x, sequence(get)): 0;
}

#define required_sequence(GET) nodes(GET(true), sequence(GET))

types *tuptyp(void) {
    if (want(TRPAREN)) return 0;
    type *item = typ();
    return want(TCOMMA)? types(item, tuptyp()): (need(TRPAREN), types(item, 0));
}

type *typ(void) {
    type    *type = 0;
    senv    *sym;

    // Get basic type.
    if (want(TID)) {
        if ((sym = lookup(all_types, tokstr->chars, 0))) {
            type = sym->type;
            if (type->form == REGULAR && sym->type->args)
                syntax("type constructor needs args: %s", type->id);
        } else if (*tokstr->chars == '_') // Define type if it does not exist.
            all_types = senv(tokstr->chars, typevar(), noval, all_types);
        else syntax("undefined type: %s", tokstr->chars);
    }
    else if (want(TLPAREN)) {
        types   *args = tuptyp();
        type = args && !args->next? args->type: tuptype(args);
    } else syntax("need type");

    // Apply type constructors.
    while (want(TID))
        if ((sym = lookup(all_types, tokstr->chars, 0))) {
            if (sym->type->form != REGULAR || !sym->type->args)
                syntax("type constructor takes no arg: %s", sym->type->id);
            type = regular(type, sym->type->id);
        } else syntax("undefined type constructor: %s", tokstr->chars);

    if (want(TARROW)) return fntype(type, typ());
    return type;
}

node *simplify_list(nodes *vals, location loc) {
    if (!vals) return enil(loc);
    return econs(vals->node->loc, vals->node, simplify_list(vals->next, loc));
}

char *patname(node *pat) {
    // WARNING: Do not look through ETYPING.
    // Params are just IDs so it would strip the typing.
    return pat->form == EVAR && pat->id != ignore_id? pat->id: 0;
}

int countnodes(nodes *i) { return i? 1 + countnodes(i->next): 0; }

nodes *getparams(void) {
    node *arg = aexpr(false);
    return arg? nodes(arg, getparams()): 0;
}

fnrules *getfnrules(toktype delim) {
    location loc = getloc();
    nodes   *params = required_sequence(aexpr);
    node    *body = (need(delim), expr());
    fnrules *rest = want(TMORE)? getfnrules(delim): 0;
    return fnrules(loc, params, body, rest);
}

// Convert fnrules to normal case rules.
nodes *fn_to_case(fnrules *i) {
    if (!i) return 0;
    node    *lhs = i->params->next? etup(i->loc, i->params): i->params->node;
    node    *rule = node(-1, i->loc, .lhs=lhs, .rhs=i->body);
    return nodes(rule, fn_to_case(i->next));
}

// Fold parameters over body to make a curried function.
node *foldfn(char *id, nodes *ps, node *body) {
    return ps
        ? efn(ps->node->loc, id, patname(ps->node), foldfn(id, ps->next, body))
        : body;
}

// Determine if any param is not a simple var or typed simple var.
bool complexparams(nodes *i) {
    return i && (!patname(i->node) || complexparams(i->next));
}

node *fnexpr(char *id, toktype delim) {
    location loc = getloc();
    fnrules *rules = ((void) want(TMORE), getfnrules(delim));
    bool    complex = rules->next;
    for (fnrules *i = rules; !complex && i; i = i->next)
        complex = complexparams(i->params);

    if (!complex) return foldfn(id, rules->params, rules->body);

    int     nparams = countnodes(rules->params);
    nodes   *canonial = 0;

    // Make sure each rule has the same number of params.
    for (fnrules *i = rules->next; i; i = i->next)
        if (countnodes(i->params) != nparams)
            fatal(i->loc, "this rule has wrong number of params");

    // Make the canonical params.
    for (int i = 0; i < nparams; i++) {
        char buf[32];
        sprintf(buf, "$%d", nparams - i - 1);
        canonial = nodes(evar(loc, cstr(buf)), canonial);
    }

    nodes   *case_rules = fn_to_case(rules);
    node    *subject = nparams == 1? canonial->node: etup(loc, canonial);
    node    *new_body = ecase(loc, subject, case_rules);
    return foldfn(id, canonial, new_body);
}

node *aexpr(bool required) {
    location    loc = getloc();
    if (!required && peek(TID) && peek_infix()) return false; // Avoid operator.
    if (want(TINT)) return elit(loc, theint(tokint), inttype);
    if (want(TCHAR)) return elit(loc, thechar(tokint), chartype);
    if (want(TSTRING)) return elit(loc, thestr(tokstr), stringtype);
    if (want(TID)) return evar(loc, tokstr->chars);
    if (want(TLPAREN)) {
        nodes *vals = csv(TRPAREN);
        return vals && !vals->next? vals->node: etup(loc, vals);
    }
    if (want(TLBRACE)) return simplify_list(csv(TRBRACE), sloc);
    if (want(TDEREF)) return ederef(loc, aexpr(true));
    if (want(TFN)) return fnexpr(0, TARROW);
    if (required) syntax("need expresion");
    return 0;
}

node *iexpr(int level) {
    if (level == 10) {
        node    *lhs = aexpr(true);
        node    *rhs;
        while ((rhs = aexpr(false))) lhs = eapp(lhs->loc, lhs, rhs);
        return lhs;
    } else {
        node    *lhs = iexpr(level + 1);
        for (infix *i; (i = peek_infix()) && i->lhs == level; ) {
            location loc = getloc();
            next();
            if (i->id == cons_id)
                lhs = econs(loc, lhs, iexpr(i->rhs));
            else
                lhs = eapp(loc, evar(loc, i->id), lhs),
                lhs = eapp(loc, lhs, iexpr(i->rhs));
        }
        return lhs;
    }
}

node *dec(bool first) {
    if (!want(TAND) && !first) return 0;
    location loc = getloc();
    node    *lhs = aexpr(true);
    node    *rhs = want(TEQUAL)? expr(): fnexpr(patname(lhs), TEQUAL);
    return node(-1, loc, .lhs=lhs, .rhs=rhs);
}

node *rule(bool first) {
    (void) first;
    if (!want(TBAR)) return 0;
    location loc = getloc();
    node    *lhs = expr();
    node    *rhs = (need(TARROW), expr());
    return node(-1, loc, .lhs=lhs, .rhs=rhs);
}

node *_expr(void) {
    location loc = getloc();
    if (want(TLET)) {
        bool rec = want(TREC);
        nodes *decs = required_sequence(dec);
        node *body = (need(TIN), expr());
        return rec? erec(loc, decs, body): elet(loc, decs, body);
    }
    else if (want(TCASE)) {
        node    *subject = expr();
        nodes   *rules = required_sequence(rule);
        return ecase(loc, subject, rules);
    }
    else if (want(TIF)) {
        node    *a = expr();
        node    *b = (need(TTHEN), expr());
        node    *c = (need(TELSE), expr());
        return eif(loc, a, b, c);
    }
    else return iexpr(0);
}

node *expr(void) {
    node *e = _expr();
    while (want(TTYPING))
        e = etyping(e->loc, e, typ());
    return want(TSEMI)? eseq(e->loc, e, expr()): e;
}




// Shorten type variable instantiation chain.
type *prune(type *t) {
    return t->inst? (t->inst = prune(t->inst)): t;
}

bool occurs_in(type *var, type *type) {
    type = prune(type);
    if (type->form == TYPEVAR)
        return type == var;
    for (types *i = type->args; i; i = i->next)
        if (occurs_in(var, i->type)) return true;
    return false;
}

type *fresh(typesub **subs, type *type) {
    type = prune(type);
    if (type->form == TYPEVAR) {
        for (types *i = nongenerics; i; i = i->next)
            if (occurs_in(type, i->type)) return type;
        for (typesub *i = *subs; i; i = i->next)
            if (i->from == type) return i->to;
        return (*subs = typesub(type, typevar(), *subs))->to;
    }

    if (!type->args) return type;

    types *args = 0;
    types **ptr = &args;
    for (types *i = type->args; i; i = i->next)
        *ptr = types(fresh(subs, i->type), 0),
        ptr = &(*ptr)->next;
    return type(type->form, .id=type->id, .args=args);
}

bool unifies(type *t, type *u) {
    t = prune(t);
    u = prune(u);

    if (t->form == TYPEVAR)
        return occurs_in(t, u)? t == u: (t->inst = u, true);

    if (u->form == TYPEVAR) return unifies(u, t);

    if (t->id != u->id) return false;
    types   *i = t->args;
    types   *j = u->args;
    for ( ; i && j; i = i->next, j = j->next)
        if (!unifies(i->type, j->type)) return false;
    return !i == !j;
}

// Give typevars names for printing.
void rename_typevars(type *type, int *uid) {
    type = prune(type);
    if (type->form == TYPEVAR && !type->id)
        type->id = intern(2, (char[]) {'_', (*uid)++ + 'a'})->chars;
    for (types *i = type->args; i; i = i->next)
        rename_typevars(i->type, uid);
}

char *_writetype(char *out, type *type, bool paren) {
    type = prune(type);
    switch (type->form) {
    case TYPEVAR:
        return out + sprintf(out, "%s", type->id);
    case REGULAR:
        if (type->args) out = _writetype(out, type->args->type, true);
        if (type->args) *out++ = ' ';
        return out += sprintf(out, "%s", type->id);
    case FNTYPE:
        if (paren)
            return *out++ = '(',
                    out = _writetype(out, type, false),
                    *out++ = ')',
                    out;
        out = _writetype(out, type->args->type, true);
        out += sprintf(out, " -> ");
        return _writetype(out, type->args->next->type, false);
    case TUPTYPE:
        *out++ = '(';
        for (types *i = type->args; i; i = i->next)
            out = _writetype(out, i->type, false),
            out += i->next? sprintf(out, ", "): 0;
        *out++ = ')';
        return out;
    }
    return out;
}

char *writetype(type *type) {
    char buf[4 * 1024];
    char *end = _writetype(buf, type, false);
    return newstr(end - buf, buf)->chars;
}

// Assert that two types are equal. Raise an error otherwise.
type *unify(node *offender, type *want, type *got) {
    if (!unifies(want, got)) {
        int     uid = 0;
        rename_typevars(want, &uid);
        rename_typevars(got, &uid);
        semantic(offender, "type mismatch:\nwant: %s\ngot:  %s",
            writetype(want), writetype(got));
    }
    return prune(want);
}

// This is required for the "Value Restriction" to polymorphism.
bool is_nonexpansive(node *expr) {
    switch (expr->form) {
    case ELIT:
        return !isdata(expr->lit.val) || datacon(expr->lit.val) != ref_id;
    case EVAR:
    case ENIL:
    case EFN:
        return true;
    case ETUP:
        for (nodes *i = expr->tup; i; i = i->next)
            if (!is_nonexpansive(i->node)) return false;
        return true;
    case ECONS:
        return is_nonexpansive(expr->lhs) && is_nonexpansive(expr->rhs);
    case EAPP:
        return expr->lhs->form == ELIT &&
                isdata(expr->lhs->lit.val) &&
                is_nonexpansive(expr->lhs) &&
                is_nonexpansive(expr->rhs);
    case ETYPING:
        return is_nonexpansive(expr->typing.body);
    case ELET:
    case EREC:
    case ECASE:
    case EIF:
    case ESEQ:
    case EDEREF:
        return false;
    }
    return false;
}

// Check pattern.
// Any variables defined are non-generic.
type *checkpat(senv **env, node *pat) {
    types   *tup = 0, **tupptr = &tup;
    type    *t, *u;
    senv    *sym;

    switch (pat->form) {
    case ELIT:
        return pat->lit.type;

    case EVAR:
        if (pat->id == ignore_id) return typevar();

        // See if this is a constructor.
        if ((sym = lookup(constrs, pat->id, 0))) {
            *pat = *elit(pat->loc, sym->val, sym->type);
            return fresh((typesub*[]) {0}, sym->type);
        }

        t = typevar();
        nongenerics = types(t, nongenerics);
        *env = senv(pat->id, t, noval, *env);
        return t;

    case ETUP:
        for (nodes *i = pat->tup; i; i = i->next)
            *tupptr = types(checkpat(env, i->node), 0),
            tupptr = &(*tupptr)->next;
        return tuptype(tup);

    case ENIL:
        return regular(typevar(), list_id);

    case ECONS:
        t = checkpat(env, pat->lhs);
        u = checkpat(env, pat->rhs);
        return unify(pat->rhs, regular(t, list_id), u);

    case EAPP:
        t = checkpat(env, pat->lhs);
        // If the l.h.s. was a constructor, it would have transformed.
        if (pat->lhs->form != ELIT || !isdata(pat->lhs->lit.val))
            goto invalid;
        u = checkpat(env, pat->rhs);
        return unify(pat, fntype(u, typevar()), t)->args->next->type;

    case ETYPING:
        return unify(pat->typing.body, pat->typing.type,
            checkpat(env, pat->typing.body));

    case EFN:
    case ELET:
    case EREC:
    case ECASE:
    case EIF:
    case ESEQ:
    case EDEREF:
    }

    invalid:
    semantic(pat, "invalid pattern");
}

type *check(senv *env, node *expr) {
    types   *tup = 0, **tupptr = &tup;
    types   *oldng = nongenerics;
    type    *t, *u;
    senv    *local = env;
    senv    *var;

    switch (expr->form) {
    case ELIT:      return expr->lit.type;
    case ENIL:      return regular(typevar(), list_id);

    case EVAR:
        // Calculate the DeBruijn index of the variable.
        var = lookup(env, expr->id, &expr->index);
        if (!var) semantic(expr, "undefined: %s", expr->id);

        // Substitute variable for known value.
        if (!isnoval(var->val)) {
            *expr = *elit(expr->loc, var->val, var->type);
            return fresh((typesub*[]) {0}, var->type);
        }

        // Each use of a variable recreates its type unless it is non-generic.
        return fresh((typesub*[]) {0}, var->type);

    case ETUP:
        for (nodes *i = expr->tup; i; i = i->next)
            *tupptr = types(check(env, i->node), 0),
            tupptr = &(*tupptr)->next;
        return tuptype(tup);

    case ECONS:
        t = check(env, expr->lhs);
        u = check(env, expr->rhs);
        return unify(expr->rhs, regular(t, list_id), u);

    case EFN:
        // Parameters are non-generic in the body.
        t = typevar();
        nongenerics = types(t, nongenerics);
        u = check(senv(expr->fn.par, t, noval, env), expr->fn.body);
        nongenerics = oldng;
        return fntype(t, u);

    case EAPP:
        t = check(env, expr->lhs);
        u = check(env, expr->rhs);
        return unify(expr->lhs, fntype(u, typevar()), t)->args->next->type;

    case ELET:
        for (nodes *i = expr->let.decs; i; i = i->next) {
            types   *tmpng = nongenerics;
            u = check(local, i->node->rhs);
            t = checkpat(&local, i->node->lhs);
            unify(i->node->rhs, t, u);

            // "Value Restriction" to polymorphism.
            // If r.h.s. is a value, the vars on l.h.s. can be generic.
            // Otherwise, leave them on the non-generic list until after body.
            if (is_nonexpansive(i->node->rhs))
                nongenerics = tmpng;
        }
        t = check(local, expr->let.body);
        nongenerics = oldng;
        return t;

    case EREC:
        // Define all l.h.s. functions names.
        // They are all non-generic until the body.
        for (nodes *i = expr->let.decs; i; i = i->next) {
            if (!patname(i->node->lhs) || i->node->rhs->form != EFN)
                semantic(i->node->lhs, "let rec only defines functions");
            t = typevar();
            nongenerics = types(t, nongenerics);
            local = senv(patname(i->node->lhs), t, noval, local);
        }

        // Check all r.h.s. and apply restrictions from their bodies.
        for (nodes *i = expr->let.decs; i; i = i->next) {
            t = lookup(local, patname(i->node->lhs), 0)->type;
            unify(i->node->lhs, t, check(local, i->node->rhs));
        }

        nongenerics = oldng;
        return check(local, expr->let.body);

    case ECASE:
        t = check(env, expr->_case.subject);
        u = typevar();
        for (nodes *i = expr->_case.rules; i; i = i->next) {
            local = env;
            unify(i->node->lhs, t, checkpat(&local, i->node->lhs));
            unify(i->node->rhs, u, check(local, i->node->rhs));
            nongenerics = oldng;
        }
        return u;

    case EIF:
        unify(expr->_if.a, booltype, check(env, expr->_if.a));
        t = check(env, expr->_if.b);
        return unify(expr->_if.c, t, check(env, expr->_if.c));

    case ETYPING:
        return unify(expr->typing.body, expr->typing.type,
            check(env, expr->typing.body));

    case ESEQ:
        check(env, expr->lhs);
        return check(env, expr->rhs);

    case EDEREF:
        t = check(env, expr->deref);
        return unify(expr->deref, regular(typevar(), ref_id), t)->args->type;
    }
    semantic(expr, "UNHANDLED_CHECK");
}








int addconst(value val) {
    for (int i = 0; i < nconstants; i++)
        if (equal(constants[i], val)) return i;
    constants[nconstants] = val;
    return nconstants++;
}

// Patch chain of branches to go to the current ec.
void patch(int pc) {
    int next = program[pc];
    program[pc] = ec;
    if (next) patch(next);
}

void compilepat(node *pat) {
    switch (pat->form) {
    case ELIT:      emiti(IPEQ, addconst(pat->lit.val)); break;
    case ENIL:      emiti(IPEQ, addconst(nil)); break;
    case EVAR:      if (pat->id == ignore_id) emit(IPOP); else emit(ILET); break;
    case ETUP:
        emit(IPTUP);
        for (nodes *i = pat->tup; i; i = i->next) compilepat(i->node);
        break;
    case ECONS:
        emit(IPCON);
        compilepat(pat->lhs);
        compilepat(pat->rhs);
        break;
    case EAPP:
        emiti(IPDAT, addconst(thestr(intern(-1, datacon(pat->lhs->lit.val)))));
        compilepat(pat->rhs);
        break;
    case ETYPING:
        compilepat(pat->typing.body);
        break;
    case EFN:
    case ELET:
    case EREC:
    case ECASE:
    case EIF:
    case ESEQ:
    case EDEREF:
        break;
    }
}

void compile(node *expr, bool is_tail) {
    int     n = 0;
    int     after_body, to_body_pc, else_case, after_all = 0;

    switch (expr->form) {
    case ELIT:
        emiti(ILIT, addconst(expr->lit.val));
        if (is_tail) emit(IRET);
        return;

    case EVAR:
        emiti(IVAR, expr->index);
        if (is_tail) emit(IRET);
        return;

    case ETUP:
        for (nodes *i = expr->tup; i; i = i->next)
            compile(i->node, false),
            n++;
        emiti(ITUP, n);
        if (is_tail) emit(IRET);
        return;

    case ENIL:
        emit(INIL);
        if (is_tail) emit(IRET);
        return;

    case ECONS:
        compile(expr->lhs, false);
        compile(expr->rhs, false);
        emit(ICONS);
        if (is_tail) emit(IRET);
        return;

    case EFN:
        to_body_pc = emiti(ICLOS, 0);
        after_body = emiti(IJMP, 0);
        patch(to_body_pc);
        compile(expr->fn.body, true);
        patch(after_body);
        if (is_tail) emit(IRET);
        return;

    case EAPP:
        compile(expr->lhs, false);
        compile(expr->rhs, false);
        emit(is_tail? ITAIL: IAPP);
        return;

    case ELET:
        for (nodes *i = expr->let.decs; i; i = i->next) {
            compile(i->node->rhs, false);
            if (!patname(i->node->lhs)) emit(ILAST);
            compilepat(i->node->lhs);
            n++;
        }
        compile(expr->let.body, is_tail);
        if (!is_tail) emiti(IDROP, n);
        return;

    case EREC:
        for (nodes *i = expr->let.decs; i; i = i->next)
            compile(i->node->rhs, false),
            n++;
        emiti(IREC, n);
        compile(expr->let.body, is_tail);
        return;

    case ECASE:
        compile(expr->_case.subject, false);
        for (nodes *i = expr->_case.rules; i; i = i->next) {
            int after_rule = i->next? emiti(IPAT, 0): emit(ILAST);
            compilepat(i->node->lhs);
            compile(i->node->rhs, is_tail);
            if (!is_tail) after_all = emiti(IJMP, after_all);
            if (i->next) patch(after_rule);
        }
        if (!is_tail) patch(after_all);
        return;

    case EIF:
        compile(expr->_if.a, false);
        else_case = emiti(IBRF, 0);
        compile(expr->_if.b, is_tail);
        after_all = emiti(IJMP, 0);
        patch(else_case);
        compile(expr->_if.c, is_tail);
        patch(after_all);
        return;

    case ETYPING:
        compile(expr->typing.body, is_tail);
        return;

    case ESEQ:
        compile(expr->lhs, false);
        emit(IPOP);
        compile(expr->rhs, is_tail);
        return;

    case EDEREF:
        compile(expr->deref, false);
        emit(IDER);
        if (is_tail) emit(IRET);
        return;
    }
    semantic(expr, "UNCOMPILED");
}


void listing(int from, int to) {
    for (int i = from; i < to; i++) {
        int op = program[i];
        if (op < 0 || op >= NINSTR) printf("INVALID_OPCODE: %d\n", op), exit(2);
        printf("%04X %-4s", i, instructions[op].name);
        if (instructions[op].arg) printf(" %04X", program[++i]);
        puts("");
    }
}



#define PUSH(X) do { if (s >= endofstack) goto overflow; *++s = (X); } while (0)
#define XCHG(N,X) (s -= N - 1, *s = X)

value eval(void) {
    int     *c = program;
    value   *stack = malloc(stack_size * sizeof *stack);
    value   *endofstack = stack + stack_size;
    value   *s = stack - 1;
    values  *e = 0;
    values  *var;
    value   a, b;
    int     n;
    bool    is_tail = false;
    struct pat { value subject, *s; values *e; int *c; } p = {noval, 0, 0, 0};

    while (true)
    // while (listing(c-program, c-program+1), true)
    switch ((opcode) *c++) {
    case IHLT:
        if (s != stack) printf("INVALID_STACK: %ld", s - stack + 1), exit(2);
        return *s;
    case ILIT:      PUSH(constants[*c++]); break;
    case IVAR:      n = *c++;
                    var = e;
                    while (n--) var = var->next;
                    PUSH(var->val);
                    break;
    case ITUP:      n = *c++; XCHG(n, newtup(n, s)); break;
    case INIL:      PUSH(nil); break;
    case ICONS:     XCHG(2, cons(s[0], s[1])); break;
    case IDAT:      XCHG(1, newdata(chars(constants[*c++]), *s)); break;
    case ICLOS:     PUSH(newclos(*c++, e)); break;
    case IRET:

    ret:
                    a = *s--;
                    c = hackval(*s--);
                    e = hackval(*s--);
                    PUSH(a);
                    break;
    case ITAIL:
    case IAPP:      is_tail = c[-1] == ITAIL;
                    a = *s--;
                    b = *s--;
                    if (isdata(b)) {
                        PUSH(newdata(datacon(b), a));
                        if (is_tail) goto ret;
                    }
                    else if (closval(b)->remain > 1) {
                        PUSH(newnative(
                            values(a, closval(b)->e),
                            closval(b)->arity,
                            closval(b)->remain - 1,
                            closval(b)->op));
                        if (is_tail) goto ret;
                    }
                    else if (closval(b)->op)
                        goto native;
                    else {
                        if (!is_tail) {
                            PUSH(hack(e));
                            PUSH(hack(c));
                        }
                        c = program + closval(b)->c;
                        e = values(a, closval(b)->e);
                    }
                    break;
    case IPOP:      s--; break;
    case ILET:      e = values(*s--, e); break;
    case IREC:      n = *c;
                    for (int i = n; i--; ) e = values(s[-i], e);
                    for (values *i = e; n--; i = i->next)
                        closval(i->val)->e = e;
                    s -= *c++;
                    break;
    case IDROP:     n = *c++; while (n--) e = e->next; break;
    case IJMP:      c = program + *c; break;
    case IBRF:      c = dataval(*s--) != dataval(_true)? program + *c: c + 1;
                    break;
    case IPEQ:      if (!equal(*s--, constants[*c++])) goto reject; break;
    case IPTUP:     a = *s--;
                    for (int i = tuplen(a); i--; ) PUSH(tupat(a, i));
                    break;
    case IPCON:     a = *s--;
                    if (!iscons(a)) goto reject;
                    PUSH(tl(a));
                    PUSH(hd(a));
                    break;
    case IPDAT:     a = *s--;
                    if (datacon(a) != chars(constants[*c++])) goto reject;
                    PUSH(dataarg(a));
                    break;
    case ILAST:     p = (struct pat) {*s, s, e, 0}; break;
    case IPAT:      p = (struct pat) {*s, s, e, program + *c++}; break;
    case IDER:      XCHG(1, dataarg(*s)); break;

    native:
        // Handle native operations.
        // The last arg is in `a`. The function is in `b`.
        // Ops should clean up their args.
        n = closval(b)->arity - 1;
        for (values *i = closval(b)->e; n--; i = i->next)
            PUSH(i->val);

        switch ((native_op) closval(b)->op) {
        case NADD: XCHG(1, theint(intval(*s) + intval(a))); break;
        case NSUB: XCHG(1, theint(intval(*s) - intval(a))); break;
        case NMUL: XCHG(1, theint(intval(*s) * intval(a))); break;
        case NDIV: XCHG(1, theint(intval(*s) / intval(a))); break;
        case NREM: XCHG(1, theint(intval(*s) % intval(a))); break;
        case NLT:  XCHG(1, intval(*s) < intval(a)? _true: _false); break;
        case NGT:  XCHG(1, intval(*s) > intval(a)? _true: _false); break;
        case NLE:  XCHG(1, intval(*s) <= intval(a)? _true: _false); break;
        case NGE:  XCHG(1, intval(*s) >= intval(a)? _true: _false); break;
        case NEQ:  XCHG(1, equal(*s, a)? _true: _false); break;
        case NNE:  XCHG(1, equal(*s, a)? _false: _true); break;
        case NPR:
            if (valtype(a) == STRING) fwrite(chars(a), 1, size(a), stdout);
            else if (valtype(a) == CHAR) putchar(charval(a));
            else printval(a);
            PUSH(a);
            break;
        case NSET:  dataval(*s)->arg = a; XCHG(1, a); break;
        }
        if (is_tail) goto ret;
        continue;

    reject:
        if (p.c) { s = p.s; e = p.e; c = p.c; *s = p.subject; continue; }
        puts("FAILED PATTERN");
        printval(p.subject);
        puts("");
        exit(1);

    overflow:
        puts("STACK OVERFLOW");
        exit(1);
    }
}

// Define a native function.
// Pass list of param types and return type then NULL.
senv *native(senv *env, uint8_t op, char *id, ...) {
    va_list ap;
    va_start(ap, id);
    int     n = 0;
    types   *tmp = 0;
    type    *out;
    while ((out = va_arg(ap, type*)))
        tmp = types(out, tmp),
        n++;
    n--;
    out = tmp->type;
    for (tmp = tmp->next; tmp; tmp = tmp->next)
        out = fntype(tmp->type, out);
    value val = newnative(0, n, n, op);
    return senv(cstr(id), out, val, env);
}


int main(int argc, char **argv) {
    (void) argc;

    setvbuf(stdout, 0, _IONBF, 0);

    unit = newtup(0, 0);

    for (int i = 0; tokname[i]; i++)
        tokname[i] = cstr(tokname[i]);

    list_id = cstr("list");
    cons_id = cstr(":");
    ref_id = cstr("ref");
    ignore_id = cstr("_");

    type    *atype = typevar();

    inttype = regular(0, cstr("int"));
    chartype = regular(0, cstr("char"));
    stringtype = regular(0, cstr("string"));
    booltype = regular(0, cstr("bool"));

    all_types = senv(inttype->id, inttype, noval, all_types);
    all_types = senv(chartype->id, chartype, noval, all_types);
    all_types = senv(stringtype->id, stringtype, noval, all_types);
    all_types = senv(booltype->id, booltype, noval, all_types);
    all_types = senv(list_id, regular(atype, list_id), noval, all_types);
    all_types = senv(ref_id, regular(atype, ref_id), noval, all_types);

    _false = newdata(cstr("false"), noval);
    _true = newdata(cstr("true"), noval);
    constrs = senv(cstr("false"), booltype, _false, constrs);
    constrs = senv(cstr("true"), booltype, _true, constrs);

    constrs = senv(cstr("ref"), fntype(atype, regular(atype, ref_id)),
        newdata(cstr("ref"), noval), constrs);

    opensrc(argv[1]);

    while (true)
        if (want(TINFIXL) || want(TINFIXR)) {
            int     rhs = token == TINFIXL? 1: 0;
            int     lhs = (need(TINT), tokint);
            while (want(TID))
                infixes = infix(tokstr->chars, lhs, lhs + rhs, infixes);
        }
        else if (want(TDATATYPE)) {
            type    *arg = 0;
            char    *arg_id = 0;
            if (want(TLPAREN)) {
                arg_id = (need(TID), tokstr->chars);
                arg = typevar();
                need(TRPAREN);
            }
            type    *dt = regular(arg, (need(TID), tokstr->chars));

            // Define type and possibly arg;
            if (lookup(all_types, dt->id, 0))
                syntax("type redefined: %s", dt->id);
            all_types = senv(dt->id, dt, noval, all_types);
            if (arg) all_types = senv(arg_id, arg, noval, all_types);

            need(TEQUAL);
            want(TBAR);
            do {
                char *id = (need(TID), tokstr->chars);
                if (lookup(constrs, id, 0))
                    syntax("constructor redefined: %s", id);
                type *type = peek(TLPAREN)? fntype(typ(), dt): dt;
                constrs = senv(id, type, newdata(id, noval), constrs);
            } while (want(TBAR));

            if (arg) all_types = all_types->next; // Drop arg.
        }
        else if (want(TSEMI)) {}
        else break;

    senv *basis = 0;
    for (senv *i = constrs; i; i = i->next)
        basis = senv(i->id, i->type, i->val, basis);

    basis = native(basis, NADD, "+", inttype, inttype, inttype, NULL);
    basis = native(basis, NSUB, "-", inttype, inttype, inttype, NULL);
    basis = native(basis, NMUL, "*", inttype, inttype, inttype, NULL);
    basis = native(basis, NDIV, "/", inttype, inttype, inttype, NULL);
    basis = native(basis, NREM, "rem", inttype, inttype, inttype, NULL);
    basis = native(basis, NLT, "<", inttype, inttype, booltype, NULL);
    basis = native(basis, NGT, ">", inttype, inttype, booltype, NULL);
    basis = native(basis, NLE, "<=", inttype, inttype, booltype, NULL);
    basis = native(basis, NGE, ">=", inttype, inttype, booltype, NULL);
    basis = native(basis, NEQ, "==", atype, atype, booltype, NULL);
    basis = native(basis, NNE, "<>", atype, atype, booltype, NULL);
    basis = native(basis, NPR, "pr", atype, atype, NULL);
    basis = native(basis, NSET, ":=", regular(atype, ref_id), atype, atype, NULL);

    node *e = expr();
    need(TEOF);
    type *t = check(basis, e);
    compile(e, false);
    emit(IHLT);
    // puts("\nLISTING");
    // listing(0, ec);
    // rename_typevars(t, (int[]){0});
    // printf("\nit :: %s\n", writetype(t));
    (void)t;

    value x = eval();
    fputs("# ", stdout), printval(x);
    puts("\ndone.");
}
