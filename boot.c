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
    TINFIXL, TINFIXR, TDATATYPE, TMORE, TDEREF, TWHERE, TAS } toktype;

char *tokname[] = {"end of file", "(", ")", "[", "]", ",", "`",
    ";", "int", "char", "string", "id", "=", "fn", "->", "let",
    "rec", "and", "in", "case", "|", "if", "then", "else", "::",
    "infixl", "infixr", "datatype", "--", "!", "where", "@", 0 };

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
        EAPP, ELET, EREC, ECASE, EIF, ETYPING, ESEQ, EDEREF,
        EAS } form;
    location loc;
    union {
        struct { value val; type *type; } lit;
        nodes *tup;
        struct { char *id; int index; };
        struct { node *lhs, *rhs; int ndrop; };
        struct { char *id, *par; node *body; } fn;
        struct { nodes *decs; node *body; int ndrop; } let;
        struct { node *subject; nodes *rules; } _case;
        struct { node *a, *b, *c; } _if;
        struct { node *body; type *type; } typing;
        struct { node *e; char *id; } as;
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

#define emit(X) (code[ec] = X, ec++)
#define emiti(X, Y) (emit(X), emit(Y))

typedef enum opcode { IHLT, ILIT, ITUP, INIL, ICONS, IVAR,
    ICLOS, IRET, IAPP, ITAIL, IPOP, IAS, ILET, IREC, IDROP,
    IJMP, IBRF, IPEQ, IPTUP, IPCON, IPDAT, ILAST, IPAT, IDREF,
} opcode;
struct { char *name; bool arg; } instructions[] = {
    {"HLT",0}, {"LIT",1}, {"TUP",1}, {"NIL",0}, {"CONS",0},
    {"VAR",1}, {"CLOS",1}, {"RET",0}, {"APP",0}, {"TAIL",0},
    {"POP",0}, {"AS",0}, {"LET",0}, {"REC",1}, {"DROP",1},
    {"JMP",1}, {"BRF",1}, {"PEQ",1}, {"PTUP",0}, {"PCON",0},
    {"PDAT",1}, {"LAST",0}, {"PAT",1}, {"DREF",0},
};
#define NINSTR ((int) (sizeof instructions / sizeof *instructions))

// Native op numbers.
// These must not start at 0 since that is how closures distinguished.
typedef enum native_op { NCONS=1, NADD, NSUB, NMUL, NDIV, NREM,
    NLT, NGT, NLE, NGE, NEQ, NNE, NPR, NSET, NSIZE, NCHAT,
    NSUBS, NFINDS, NJOIN, NIMPLODE, NORD, NCHR, NRDF, NEXIT,
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
type        *booltype, *inttype, *chartype, *strtype;
types       *nongenerics;
senv        *all_types;
senv        *constrs;
char        *list_id, *cons_id, *ref_id, *ignore_id, *and_id, *or_id;
char        *some_id, *opt_id;
value       nil = {NIL}, noval = {NOVAL, {0}}, unit, _true, _false;
value       empty_str, none;
int         *code;
location    *codeloc;
int         ec; // `c` that we're emitting at.
value       constants[1024 * 1024];
int         nconstants;
int         stack_size = 1024 * 1024;
int         code_size = 8 * 1024 * 1024;





#define new(TYPE, ...)\
    ((TYPE*) memcpy(malloc(sizeof (TYPE)), &(TYPE){__VA_ARGS__}, sizeof (TYPE)))

_Noreturn void _fatal(location loc, char *msg, va_list ap) {
    printf("boot: error %s:%d:%d: ",
        loc.name? loc.name: "?", loc.line, loc.col);
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

void printloc(location loc) {
    printf("> %s:%d:%d\n", loc.name, loc.line, loc.col);
}

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




#define value(F,...) ((value) {F, __VA_ARGS__})
#define values(VAL, NEXT) (new(values, VAL, NEXT))
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
#define some(ARG) newdata(some_id, ARG)



#define type(F, ...) new(struct type, F, __VA_ARGS__)
#define types(T, N) new(struct types, T, N)
#define typesub(FROM, TO, NEXT) new(struct typesub, FROM, TO, NEXT)
#define regular(ARG, ID) type(REGULAR, .id=ID, .args=ARG? types(ARG, 0): 0)
#define typevar() type(TYPEVAR)
#define fntype(FROM,TO) type(FNTYPE, .args=types(FROM, types(TO, 0)))
#define tuptype(TYPES) type(TUPTYPE, .args=TYPES)
#define opttype(BASE) regular(BASE, opt_id)
#define listtype(BASE) regular(BASE, list_id)
#define reftype(BASE) regular(BASE, ref_id)



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
#define elet(L, DECS, BODY) node(ELET, L, .let={DECS, BODY, 0})
#define erec(L, DECS, BODY) node(EREC, L, .let={DECS, BODY, 0})
#define ecase(L, SUBJECT, DECS) node(ECASE, L, ._case={SUBJECT, DECS})
#define eif(L, A, B, C) node(EIF, L, ._if={A, B, C})
#define eas(L, X, ID) node(EAS, L, .as={X, ID})
#define etyping(L, BODY, TYPE) node(ETYPING, L, .typing={BODY, TYPE})
#define eseq(L, LHS, RHS) node(ESEQ, L, .lhs=LHS, .rhs=RHS)




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

    while (true)
        if (*src == '\n') src++, sloc.line++, sloc.col = 1, sol = src;
        else if (isspace(*src)) src++;
        else if (*src == '#')  while (*src && *src != '\n') src++;
        else break;

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
node *letexpr(bool rec, node *body, bool need_body);
type *typeexpr(void);

senv *lookup(senv *env, char *id, int *index) {
    for (senv *i = env; i; i = i->next)
        if (i->id == id) return i;
        else if (isnoval(i->val) && index) (*index)++;
    return 0;
}

// Return the descriptor for the next token if it is an operator.
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

types *tuptypeexpr(void) {
    if (want(TRPAREN)) return 0;
    type *item = typeexpr();
    return want(TCOMMA)
        ? types(item, tuptypeexpr())
        : (need(TRPAREN), types(item, 0));
}

// Read a type expression.
// WARNING: This may introduce types into all_types. (e.g. `_a`)
// Save all_types before calling and restore after.
type *typeexpr(void) {
    type    *type = 0;
    senv    *sym;

    // Get basic type.
    if (want(TID)) {
        if ((sym = lookup(all_types, tokstr->chars, 0))) {
            type = sym->type;
            if (type->form == REGULAR && sym->type->args)
                syntax("type constructor needs args: %s", type->id);
        } else if (*tokstr->chars == '_') // Define type if it does not exist.
            type = typevar(),
            all_types = senv(tokstr->chars, type, noval, all_types);
        else syntax("undefined type: %s", tokstr->chars);
    }
    else if (want(TLPAREN)) {
        types   *args = tuptypeexpr();
        type = args && !args->next? args->type: tuptype(args);
    } else syntax("need type");

    // Apply type constructors.
    while (want(TID))
        if ((sym = lookup(all_types, tokstr->chars, 0))) {
            if (sym->type->form != REGULAR || !sym->type->args)
                syntax("type constructor takes no arg: %s", sym->type->id);
            type = regular(type, sym->type->id);
        } else syntax("undefined type constructor: %s", tokstr->chars);

    if (want(TARROW)) return fntype(type, typeexpr());
    return type;
}

char *patname(node *pat, bool through_typing) {
    // WARNING: Do not look through ETYPING for params.
    // Params are just IDs so it would strip the typing.
    if (through_typing && pat->form == ETYPING)
        return patname(pat->typing.body, true);
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
    if (want(TWHERE)) {
        bool rec = want(TREC);
        bool need_end = want(TLPAREN);
        body = letexpr(rec, body, false);
        if (need_end) need(TRPAREN);
    }
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
        ? efn(ps->node->loc, id, patname(ps->node, false),
            foldfn(id, ps->next, body))
        : body;
}

// Determine if any param is not a simple var or typed simple var.
bool complexparams(nodes *i) {
    return i && (!patname(i->node, false) || complexparams(i->next));
}

node *fnexpr(char *id, toktype delim) {
    location    loc = getloc();
    fnrules     *rules = ((void) want(TMORE), getfnrules(delim));
    bool        complex = rules->next;

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

node *conslist(nodes *vals, location loc) {
    if (!vals) return enil(loc);
    return econs(vals->node->loc, vals->node, conslist(vals->next, loc));
}

node *aexpr(bool required) {
    location    loc = getloc();
    if (!required && peek(TID) && peek_infix()) return false; // Avoid operator.
    if (want(TINT)) return elit(loc, theint(tokint), inttype);
    if (want(TCHAR)) return elit(loc, thechar(tokint), chartype);
    if (want(TSTRING)) return elit(loc, thestr(tokstr), strtype);
    if (want(TID)) return evar(loc, tokstr->chars);
    if (want(TLPAREN)) {
        nodes *vals = csv(TRPAREN);
        return vals && !vals->next? vals->node: etup(loc, vals);
    }
    if (want(TLBRACE)) return conslist(csv(TRBRACE), sloc);
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
            else if (i->id == and_id)
                lhs = eif(loc, lhs, iexpr(i->rhs), elit(loc, _false, booltype));
            else if (i->id == or_id)
                lhs = eif(loc, lhs, elit(loc, _true, booltype), iexpr(i->rhs));
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
    char    *id = patname(lhs, true);
    node    *rhs = want(TEQUAL)? expr(): fnexpr(id, TEQUAL);
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

node *letexpr(bool rec, node *body, bool need_body) {
    location loc = getloc();
    nodes   *decs = required_sequence(dec);
    if (!body && need_body) body = (need(TIN), expr());
    return rec? erec(loc, decs, body): elet(loc, decs, body);
}

node *_expr(void) {
    location loc = getloc();
    if (want(TLET))
        return letexpr(want(TREC), 0, true);
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
    while (want(TTYPING)) {
        senv    *old = all_types;
        e = etyping(e->loc, e, typeexpr());
        all_types = old;
    }
    while (want(TAS)) {
        location loc = getloc();
        char    *id = (need(TID), tokstr->chars);
        e = eas(loc, e, id);
    }
    return want(TSEMI)? eseq(e->loc, e, expr()): e;
}

void infixdec(bool left_assoc) {
    int     lhs = (need(TINT), tokint);
    while (want(TID))
        infixes = infix(tokstr->chars, lhs, lhs + left_assoc, infixes);
}

void datacondec(type *dt) {
    char    *id = (need(TID), tokstr->chars);
    if (lookup(constrs, id, 0))
        syntax("constructor redefined: %s", id);
    senv    *old = all_types;
    type    *type = peek(TLPAREN)? fntype(typeexpr(), dt): dt;
    all_types = old;
    constrs = senv(id, type, newdata(id, noval), constrs);
}

void datatypedec(void) {
    type    *arg = 0;
    char    *arg_id = 0;
    if (want(TLPAREN)) {
        arg_id = (need(TID), tokstr->chars);
        arg = typevar();
        need(TRPAREN);
    }
    type    *dt = regular(arg, (need(TID), tokstr->chars));

    if (lookup(all_types, dt->id, 0))
        syntax("type redefined: %s", dt->id);

    // Define type and possibly arg.
    all_types = senv(dt->id, dt, noval, all_types);
    senv    *permanent = all_types;
    if (arg) all_types = senv(arg_id, arg, noval, all_types);

    need(TEQUAL);
    want(TBAR);
    do datacondec(dt); while (want(TBAR));
    all_types = permanent;
}

node **script(node **ptr, senv **env) {
    while (!want(TEOF))
        if (want(TINFIXL)) infixdec(true);
        else if (want(TINFIXR)) infixdec(false);
        else if (want(TDATATYPE)) datatypedec();
        else if (want(TSEMI)) { }
        else if (want(TLET)) {
            *ptr = letexpr(want(TREC), 0, false);
            ptr = &(*ptr)->let.body;
        }
        else syntax("need top-level statement");

    // Add the constructors defined here to the environment.
    for (senv *i = constrs; i; i = i->next)
        if (!lookup(*env, i->id, 0))
            *env = senv(i->id, i->type, i->val, *env);
    return ptr;
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

char *writetype(type *type, int *uid) {
    char    buf[4 * 1024];
    int     _ignored = 0;
    if (!uid) uid = &_ignored;
    rename_typevars(type, uid);
    char *end = _writetype(buf, type, false);
    return newstr(end - buf, buf)->chars;
}

// Assert that two types are equal. Raise an error otherwise.
type *unify(node *offender, type *want, type *got) {
    if (!unifies(want, got)) {
        int     uid = 0;
        char    *w = writetype(want, &uid);
        char    *g = writetype(got, &uid);
        semantic(offender, "type mismatch:\nwant: %s\ngot:  %s", w, g);
    }
    return prune(want);
}

int countsymbols(senv *base, senv *last) {
    return base == last? 0: 1 + countsymbols(base, last->next);
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
    case EAS:
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

    case EAS:
        t = typevar();
        *env = senv(pat->as.id, t, noval, *env);
        nongenerics = types(t, nongenerics);
        return unify(pat, checkpat(env, pat->as.e), t);

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

        expr->let.ndrop = countsymbols(env, local);

        t = check(local, expr->let.body);
        nongenerics = oldng;
        return t;

    case EAS: semantic(expr, "@ cannot be used in expression");

    case EREC:
        // Define all l.h.s. functions names.
        // They are all non-generic until the body.
        for (nodes *i = expr->let.decs; i; i = i->next) {
            if (!patname(i->node->lhs, true) || i->node->rhs->form != EFN)
                semantic(i->node->lhs, "let rec only defines functions");
            t = typevar();
            nongenerics = types(t, nongenerics);
            local = senv(patname(i->node->lhs, true), t, noval, local);
        }

        // Check all r.h.s. and apply restrictions from their bodies.
        for (nodes *i = expr->let.decs; i; i = i->next) {
            t = lookup(local, patname(i->node->lhs, true), 0)->type;
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
            i->node->ndrop = countsymbols(env, local);
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







void compile(node *expr, bool is_tail);

int addconst(value val) {
    for (int i = 0; i < nconstants; i++)
        if (equal(constants[i], val)) return i;
    constants[nconstants] = val;
    return nconstants++;
}

// Patch chain of branches to go to the current ec.
void patch(int pc) {
    int next = code[pc];
    code[pc] = ec;
    if (next) patch(next);
}

void compilepat(node *pat) {
    switch (pat->form) {
    case ELIT:      emiti(IPEQ, addconst(pat->lit.val)); break;
    case ENIL:      emiti(IPEQ, addconst(nil)); break;
    case EVAR:      if (pat->id == ignore_id) emit(IPOP); else emit(ILET); break;
    case EAS:       emit(IAS); compilepat(pat->as.e); break;
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

void _compile(node *expr, bool is_tail) {
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

    case EAS: return;

    case ELET:
        for (nodes *i = expr->let.decs; i; i = i->next) {
            compile(i->node->rhs, false);
            if (!patname(i->node->lhs, true)) emit(ILAST);
            compilepat(i->node->lhs);
        }
        compile(expr->let.body, is_tail);
        if (!is_tail) emiti(IDROP, expr->let.ndrop);
        return;

    case EREC:
        for (nodes *i = expr->let.decs; i; i = i->next)
            compile(i->node->rhs, false),
            n++;
        emiti(IREC, n);
        compile(expr->let.body, is_tail);
        if (!is_tail) emiti(IDROP, n);
        return;

    case ECASE:
        compile(expr->_case.subject, false);
        for (nodes *i = expr->_case.rules; i; i = i->next) {
            int after_rule = i->next? emiti(IPAT, 0): emit(ILAST);
            compilepat(i->node->lhs);
            compile(i->node->rhs, is_tail);
            if (!is_tail) emiti(IDROP, i->node->ndrop);
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
        emit(IDREF);
        if (is_tail) emit(IRET);
        return;
    }
    semantic(expr, "UNCOMPILED");
}

void compile(node *expr, bool is_tail) {
    int base = ec;
    _compile(expr, is_tail);
    // Mark instruction locations if they aren't already set.
    for (int i = base; i < ec; i++)
        if (!codeloc[i].name) codeloc[i] = expr->loc;
}


void listing(int from, int to) {
    for (int i = from; i < to; i++) {
        location    loc = codeloc[i];
        char        locbuf[1024];
        sprintf(locbuf, "%s:%d:%d", loc.name, loc.line, loc.col);

        int op = code[i];
        if (op < 0 || op >= NINSTR)
            fatal(loc, "INVALID_OPCODE: %d\n", op);

        printf("%-32s %06X %-4s", locbuf, i, instructions[op].name);
        if (instructions[op].arg) printf(" %04X", code[++i]);
        puts("");
    }
}


value substr(string *s, int i, int j, char **exn) {
    if (i < 0) i += s->len;
    if (j < 0) j += s->len + 1;
    if (i < 0 || i > s->len) { *exn = "OUT OF BOUNDS"; return noval; }
    if (j < 0 || j > s->len) { *exn = "OUT OF BOUNDS"; return noval; }
    if (j < i) { *exn = "INDEXES CROSS"; return noval; }
    return i == j? empty_str: thestr(newstr(j - i, s->chars + i));
}

value findstr(string *big, int after, string *small) {
    if (small->len == 0) return some(theint(0));
    if (big->len < small->len) return none;
    int limit = big->len - small->len + 1;
    for (int i = after; i < limit; ) {
        if (!memcmp(big->chars + i, small->chars, small->len))
            return some(theint(i));
        i++;
        while (i < limit && big->chars[i] != *small->chars) i++;
    }
    return none;
}

value join(value list) {
    int     len = 0;
    for (value i = list; iscons(i); i = tl(i)) len += size(hd(i));
    string  *str = newstr(len, 0);
    int     base = 0;
    for (value i = list; iscons(i); i = tl(i)) {
        memcpy(str->chars + base, chars(hd(i)), size(hd(i)));
        base += size(hd(i));
    }
    return thestr(str);
}

value implode(value list) {
    int     len = 0;
    for (value i = list; iscons(i); i = tl(i)) len++;
    string  *str = newstr(len, 0);
    char    *ptr = str->chars;
    for (value i = list; iscons(i); i = tl(i)) *ptr++ = charval(hd(i));
    return thestr(str);
}

// Scan stack for return addresses and print them.
void backtrace(value *base, value *top) {
    for (value *i = top; i >= base; i--) {
        if (!isnoval((*i))) continue;
        int     vc = (int*) hackval(*i) - code;
        if (0 <= vc && vc <= ec) printloc(codeloc[vc]);
    }
}

#define PUSH(X) do { if (s >= endofstack) goto overflow; *++s = (X); } while (0)
#define XCHG(N,X) { value XCHG_TMP = X; s -= N - 1; *s = XCHG_TMP; }

value eval(void) {
    int     *c = code;
    value   *stack = malloc(stack_size * sizeof *stack);
    value   *endofstack = stack + stack_size;
    value   *s = stack - 1;
    values  *e = 0;
    values  *var;
    value   a, b;
    int     n;
    bool    is_tail = false;
    char    *exn;
    struct pat { value subject, *s; values *e; int *c; } p = {noval, 0, 0, 0};

    while (true)
    // while (listing(c-code, c-code+1), true)
    switch ((opcode) *c++) {
    case IHLT:      if (s != stack)
                        fatal(codeloc[c - code], "INVALID_STACK: %ld",
                            s - stack + 1);
                    return *s;
    case ILIT:      PUSH(constants[*c++]); break;
    case IVAR:      for (var = e, n = *c++; n--; var = var->next) {}
                    PUSH(var->val);
                    break;
    case ITUP:      n = *c++; XCHG(n, newtup(n, s - n + 1)); break;
    case INIL:      PUSH(nil); break;
    case ICONS:     XCHG(2, cons(s[-1], s[0])); break;
    case ICLOS:     PUSH(newclos(*c++, e)); break;
    case IRET:      goto ret;
    case ITAIL:
    case IAPP:      is_tail = c[-1] == ITAIL;
                    a = *s--;
                    b = *s--;
                    if (isdata(b)) { // Apply data constructor.
                        PUSH(newdata(datacon(b), a));
                        if (is_tail) goto ret;
                    }
                    else if (closval(b)->remain > 1) { // Partially applied.
                        struct clos *f = closval(b);
                        PUSH(newnative(values(a, f->e), f->arity,
                            f->remain - 1, f->op));
                        if (is_tail) goto ret;
                    }
                    else if (closval(b)->op) // Fully applied native.
                        goto native;
                    else {
                        if (!is_tail) {
                            PUSH(hack(e));
                            PUSH(hack(c));
                        }
                        c = code + closval(b)->c;
                        e = values(a, closval(b)->e);
                    }
                    break;
    case IPOP:      s--; break;
    case IAS:       e = values(*s, e); break;
    case ILET:      e = values(*s--, e); break;
    case IREC:      n = *c;
                    for (int i = n; i--; ) e = values(s[-i], e);
                    for (values *i = e; n--; i = i->next)
                        closval(i->val)->e = e;
                    s -= *c++;
                    break;
    case IDROP:     n = *c++; while (n--) e = e->next; break;
    case IJMP:      c = code + *c; break;
    case IBRF:      c = dataval(*s--) != dataval(_true)? code + *c: c + 1;
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
    case IPAT:      p = (struct pat) {*s, s, e, code + *c++}; break;
    case IDREF:     XCHG(1, dataarg(*s)); break;

    ret:
        a = *s--;
        c = hackval(*s--);
        e = hackval(*s--);
        PUSH(a);
        continue;

    native:
        // Handle native operations.
        // The last arg is in `a`. The function is in `b`.
        // Ops should clean up their args.
        n = closval(b)->arity - 1;
        for (values *i = closval(b)->e; n--; i = i->next)
            PUSH(i->val);

        switch ((native_op) closval(b)->op) {
        case NCONS:     XCHG(1, cons(*s, a)); break;
        case NADD:      XCHG(1, theint(intval(*s) + intval(a))); break;
        case NSUB:      XCHG(1, theint(intval(*s) - intval(a))); break;
        case NMUL:      XCHG(1, theint(intval(*s) * intval(a))); break;
        case NDIV:      XCHG(1, theint(intval(*s) / intval(a))); break;
        case NREM:      XCHG(1, theint(intval(*s) % intval(a))); break;
        case NLT:       XCHG(1, intval(*s) < intval(a)? _true: _false); break;
        case NGT:       XCHG(1, intval(*s) > intval(a)? _true: _false); break;
        case NLE:       XCHG(1, intval(*s) <= intval(a)? _true: _false); break;
        case NGE:       XCHG(1, intval(*s) >= intval(a)? _true: _false); break;
        case NEQ:       XCHG(1, equal(*s, a)? _true: _false); break;
        case NNE:       XCHG(1, equal(*s, a)? _false: _true); break;
        case NPR:       if (valtype(a) == STRING) fwrite(chars(a), 1, size(a), stdout);
                        else if (valtype(a) == CHAR) putchar(charval(a));
                        else printval(a);
                        PUSH(a);
                        break;
        case NSET:      dataval(*s)->arg = a; XCHG(1, a); break;
        case NSIZE:     PUSH(theint(size(a))); break;
        case NCHAT:     n = intval(a);
                        if (n < 0) n += size(*s);
                        if (n < 0 || n >= size(*s))
                            fatal(codeloc[c - 1 - code], "OUT OF BOUNDS");
                        XCHG(1, thechar(chars(*s)[n]));
                        break;
        case NSUBS:     exn = 0;
                        a = substr(strval(*s), intval(s[-1]), intval(a), &exn);
                        if (exn) goto exception;
                        XCHG(2, a);
                        break;
        case NFINDS:    XCHG(2, findstr(strval(*s), intval(s[-1]), strval(a))); break;
        case NJOIN:     PUSH(join(a)); break;
        case NIMPLODE:  PUSH(implode(a)); break;
        case NORD:      PUSH(theint(charval(a))); break;
        case NCHR:      PUSH(thechar(intval(a) % 256)); break;
        case NRDF:      a = thestr(readfile(chars(a)));
                        PUSH(strval(a)? newdata(some_id, a): none);
                        break;
        case NEXIT:     exit(intval(a));
        }
        if (is_tail) goto ret;
        continue;

    // Reject a value in pattern matching.
    // Before attempting a match, p is set. If there is a c value, jump there.
    // Otherwise, the pattern fails.
    reject:
        if (p.c) { s = p.s, e = p.e, c = p.c, *s = p.subject; continue; }
        fputs("OFFENDING VALUE: ", stdout), printval(p.subject), puts("");
        printloc(codeloc[c - 1 - code]);
        backtrace(stack, s);
        fatal(codeloc[c - 1 - code], "FAILED PATTERN");

    // The stack has overflown. Exit.
    overflow:
        backtrace(stack, s);
        fatal(codeloc[c - 1 - code], "STACK OVERFLOW");

    // An exception occured in an external function.
    exception:
        backtrace(stack, s);
        fatal(codeloc[c - 1 - code], exn);
    }
}

// Define a native function.
// Pass list of param types and return type then NULL.
senv *native(senv *env, uint8_t op, char *id, ...) {
    va_list ap;
    va_start(ap, id);
    int     n;
    types   *tmp = 0;
    type    *out;
    for (n = 0; (out = va_arg(ap, type*)); n++)
        tmp = types(out, tmp);
    n--; // Last type was the return type not a param.
    out = tmp->type;
    for (tmp = tmp->next; tmp; tmp = tmp->next)
        out = fntype(tmp->type, out);
    return senv(cstr(id), out, newnative(0, n, n, op), env);
}

senv *initialize(senv *e) {
    code        = malloc(code_size * sizeof *code);
    codeloc    = malloc(code_size * sizeof *codeloc);

    // Common strings.
    list_id     = cstr("list");
    cons_id     = cstr(":");
    ref_id      = cstr("ref");
    ignore_id   = cstr("_");
    and_id      = cstr("&&");
    or_id       = cstr("||");
    opt_id      = cstr("option");
    some_id     = cstr("SOME");
    char *true_id = cstr("true");
    char *false_id = cstr("false");

    // Intern tokens.
    for (int i = 0; tokname[i]; i++) tokname[i] = cstr(tokname[i]);

    // Common Values.
    unit        = newtup(0, 0);
    empty_str   = thestr(intern(0, 0));
    none        = newdata(cstr("NONE"), noval);

    // Define basis types.
    type *a = typevar();
    type *i = inttype     = regular(0, cstr("int"));
    type *c = chartype    = regular(0, cstr("char"));
    type *s = strtype     = regular(0, cstr("string"));
    type *b = booltype    = regular(0, cstr("bool"));
    all_types = senv(i->id, i, noval, all_types);
    all_types = senv(c->id, c, noval, all_types);
    all_types = senv(s->id, s, noval, all_types);
    all_types = senv(b->id, b, noval, all_types);
    all_types = senv(list_id, listtype(a), noval, all_types);
    all_types = senv(ref_id, reftype(a), noval, all_types);
    all_types = senv(opt_id, opttype(a), noval, all_types);

    // Basis data constructors.
    _false = newdata(false_id, noval);
    _true = newdata(true_id, noval);
    value _ref = newdata(ref_id, noval);
    value _some = newdata(some_id, noval);
    constrs = senv(false_id, b, _false, constrs);
    constrs = senv(true_id, b, _true, constrs);
    constrs = senv(ref_id, fntype(a, reftype(a)), _ref, constrs);
    constrs = senv(datacon(none), opttype(a), none, constrs);
    constrs = senv(some_id, fntype(a, opttype(a)), _some, constrs);

    // Basis functions.
    e = native(e, NCONS, ":", a, listtype(a), listtype(a), NULL);
    e = native(e, NADD, "+", i, i, i, NULL);
    e = native(e, NSUB, "-", i, i, i, NULL);
    e = native(e, NMUL, "*", i, i, i, NULL);
    e = native(e, NDIV, "/", i, i, i, NULL);
    e = native(e, NREM, "rem", i, i, i, NULL);
    e = native(e, NLT, "<", i, i, b, NULL);
    e = native(e, NGT, ">", i, i, b, NULL);
    e = native(e, NLE, "<=", i, i, b, NULL);
    e = native(e, NGE, ">=", i, i, b, NULL);
    e = native(e, NEQ, "==", a, a, b, NULL);
    e = native(e, NNE, "<>", a, a, b, NULL);
    e = native(e, NPR, "pr", a, a, NULL);
    e = native(e, NSET, ":=", reftype(a), a, a, NULL);
    e = native(e, NSIZE, "size", s, i, NULL);
    e = native(e, NCHAT, "char_at", s, i, c, NULL);
    e = native(e, NSUBS, "substr", s, i, i, s, NULL);
    e = native(e, NFINDS, "findstr", s, i, s, opttype(i), NULL);
    e = native(e, NJOIN, "join", listtype(s), s, NULL);
    e = native(e, NIMPLODE, "implode", listtype(c), s, NULL);
    e = native(e, NORD, "ord", c, i, NULL);
    e = native(e, NCHR, "chr", i, c, NULL);
    e = native(e, NRDF, "readfile", s, opttype(s), NULL);
    e = native(e, NEXIT, "exit", i, a, NULL);

    return e;
}


int main(int argc, char **argv) {
    (void) argc;

    setvbuf(stdout, 0, _IONBF, 0);

    senv    *basis = initialize(0);
    node    *e = 0;
    node    **eptr = &e;

    opensrc("boot.ml");
    eptr = script(eptr, &basis);
    opensrc(argv[1]);
    eptr = script(eptr, &basis);
    *eptr = etup(sloc, 0); // The final result of the program.

    check(basis, e);
    compile(e, false);
    emit(IHLT);
    // puts("\nLISTING");
    // listing(0, ec);
    eval();
}
