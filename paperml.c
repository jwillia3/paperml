#define NARG 8

#include <assert.h>
#include <ctype.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <fcntl.h>
#include <unistd.h>

#define copy(N, X) memcpy(malloc((N) * sizeof *(X)), (X), (N) * sizeof *(X))
#define new(TYPE, ...) (TYPE*) copy(1, (&(TYPE) { __VA_ARGS__ }))

typedef struct { int len; char txt[]; } string;
typedef struct { string *name; int ln, col; } t_loc;

typedef struct t_type t_type;
typedef struct t_exp t_exp;
typedef struct val val;

typedef enum {
    Teof, Tint, Tchar, Tstring, Tid, Tder, Tlparen, Trparen,
    Tlbrace, Trbrace, Tlcurly, Trcurly, Tcomma, Tdot, Tsemi,
    Tfn, Toper, Tarrow, Tty, Tlarrow, Tequal, Tas, Tand, Tcase,
    Tdatatype, Tdef, Telse, Tendc, Tendf, Tendw, Texcept, Texception,
    Tif, Tin, Tinfixl, Tinfixr, Tlet, Tor, Trec, Tthen, Twith, Twhere,
    Tbar,
} t_tok;

char *tokn[] = {
    "eof", "int", "char", "string", "id", "!", "(", ")", "[", "]",
    "{", "}", ",", ".", ";", "\\", "`", "->", "::", "<-", "=", "@",
    "and", "case", "datatype", "def", "else", "endc", "endf",
    "endw", "except", "exception", "if", "in", "infixl", "infixr",
    "let", "or", "rec", "then", "with", "where", "|",
};

char *sym_chars = "%&$+-/:<=>?@~^|*";

typedef enum hostnum {
    Oadd, Osub, Omul, Odiv, Orem, Opow, Olt, Ogt, Ole, Oge, Oeq, One,
    Oord, Ochr, Ocharcmp, Oimplode, Ochartostr,Ocat, Ojoin, Ostrlen,
    Ocharat, Ostrsplice, Osubstr, Ostrcmp, Osubstrcmp, Ofindsubstr,
    Ofindchar, Oatoi, Oitoa, Oset, Oexit, Oprn, Osrand, Orand,
    Ogetenviron, Osysopen, Osysclose, Osysread, Osyswrite,
} hostnum;

struct hostspec {
    char *id;
    char *typespec;
    int arity;
    t_type *type;
}
hosts[] = {
    [Oadd] = {"+", "int->int->int", 2, 0},
    [Osub] = {"-", "int->int->int", 2, 0},
    [Omul] = {"*", "int->int->int", 2, 0},
    [Odiv] = {"/", "int->int->int", 2, 0},
    [Orem] = {"rem", "int->int->int", 2, 0},
    [Opow] = {"**", "int->int->int", 2, 0},
    [Olt] = {"<", "int->int->bool", 2, 0},
    [Ogt] = {">", "int->int->bool", 2, 0},
    [Ole] = {"<=", "int->int->bool", 2, 0},
    [Oge] = {">=", "int->int->bool", 2, 0},
    [Oeq] = {"==", "a->a->bool", 2, 0},
    [One] = {"<>", "a->a->bool", 2, 0},
    [Oord] = {"ord", "char->int", 1, 0},
    [Ochr] = {"chr", "int->char", 1, 0},
    [Ocharcmp] = {"charcmp", "char->char->int", 2, 0},
    [Oimplode] = {"implode", "[char]->string", 1, 0},
    [Ochartostr] = {"chartostr", "char->string", 1, 0},
    [Ocat] = {"^", "string->string->string", 2, 0},
    [Ojoin] = {"join", "[string]->string", 1, 0},
    [Ostrlen] = {"strlen", "string->int", 1, 0},
    [Ocharat] = {"charat", "string->int->char", 2, 0},
    [Ostrsplice] = {"strsplice", "string->int->int->[string]->string", 4, 0},
    [Osubstr] = {"substr", "string->int->int->string", 3, 0},
    [Ostrcmp] = {"strcmp", "string->string->int", 2, 0},
    [Osubstrcmp] = {"substrcmp", "string->int->string->int->int->int", 5, 0},
    [Ofindsubstr] = {"findsubstr", "string->int->string->int->int->int option", 5, 0},
    [Ofindchar] = {"findchar'", "string->int->char->int option", 3, 0},
    [Oatoi] = {"atoi", "string->int", 1, 0},
    [Oitoa] = {"itoa", "int->string", 1, 0},
    [Oset] = {":=", "a ref->a->a", 2, 0},
    [Oexit] = {"exit", "int->a", 1, 0},
    [Oprn] = {"prn", "a->a", 1, 0},
    [Osrand] = {"srand", "int->()", 1, 0},
    [Orand] = {"rand", "()->int", 1, 0},
    [Ogetenviron] = {"getenviron", "()->(string,string) list", 1, 0},
    [Osysopen] = {"sysopen", "string->int->int->int", 3, 0},
    [Osysclose] = {"sysclose", "int->int", 1, 0},
    [Osysread] = {"sysread", "int->int->string", 2, 0},
    [Osyswrite] = {"syswrite", "int->string->int", 2, 0},
};

struct val {
    enum { Int, Char, String, List, Tuple, Data, Record, Fn, Host } form;
    union {
        int i;
        unsigned char c;
        string *str;
        struct list *list;
        struct tup *tup;
        struct data *data;
        struct rec *rec;
        struct fn *fn;
        struct host *host;
    };
};

struct list { val hd; struct list *tl; };
struct tup { int n; val xs[]; };
struct data { string *con; val val; };
struct rec { int n; string **fs; val xs[]; };
struct fn { string *id; t_exp *lhs, *rhs; struct t_env *env;};
struct host {
    hostnum op;
    string *id;
    int n;
    int arity;
    val xs[];
};

struct t_type {
    enum { Type, Typevar, Polyvar, TupType, RecType, FnType } form;
    int n;
    int level;
    bool open;
    string *id;
    string **fs;
    t_type *ts[NARG];
};

typedef struct t_type_swap {
    t_type *from;
    t_type *to;
    struct t_type_swap *next;
} t_type_swap;

typedef struct t_openrec {
    t_exp *e;
    t_type *type;
    struct t_openrec *next;
} t_openrec;

#define elit(LOC, LIT) new(t_exp, Elit, LOC, .lit=LIT)
#define evar(LOC, ID) new(t_exp, Evar, LOC, .var=ID)
#define elist(LOC, HD, TL) new(t_exp, Elist, LOC, .list={HD, TL})
#define etup(LOC, N, ES) new(t_exp, Etup, LOC, .tup={N, ES})
#define erec(LOC, N, FS, ES) new(t_exp, Erec, LOC, .rec={N, FS, ES})
#define eder(LOC, E) new(t_exp, Eder, LOC, .der=E)
#define efn(LOC, ID, PAR, E) new(t_exp, Efn, LOC, .fn={ID, PAR, E})
#define edot(LOC, E, ID) new(t_exp, Edot, LOC, .dot={E, ID})
#define ewith(LOC, E, MODS) new(t_exp, Ewith, LOC, .with={E, MODS})
#define eapp(LOC, F, X) new(t_exp, Eapp, LOC, .app={F, X})
#define ecase(LOC, E, RS) new(t_exp, Ecase, LOC, ._case={E, RS})
#define eif(LOC, C, T, F) new(t_exp, Eif, LOC, ._if={C, T, F})
#define elet(LOC, ID, RHS, E) new(t_exp, Elet, LOC, .let={ID, RHS, E})
#define eletrec(LOC, DS, E) new(t_exp, Eletrec, LOC, .letrec={DS, E})
#define ety(LOC, E, T) new(t_exp, Ety, LOC, .ty={E, T})
#define eseq(LOC, E, F) new(t_exp, Eseq, LOC, .seq={E, F})
#define eas(LOC, E, ID) new(t_exp, Eas, LOC, .as={E, ID})
#define eexception(LOC, ID, E) new(t_exp, Eexception, LOC, .exception={ID, E})
#define eexcept(LOC, E, RS) new(t_exp, Eexcept, LOC, .except={E, RS})

struct t_exp {
    enum {
        Elit, Evar, Elist, Etup, Erec, Eder, Efn, Edot, Ewith, Eapp, Ety, Elet,
        Eletrec, Ecase, Eif, Eseq, Eas, Eexcept, Eexception,
    } form;

    t_loc loc;

    union {
        val lit;
        string *var;
        struct { int n; t_exp **es; } tup;
        struct { int n; string **fs; t_exp **es; } rec;
        struct { t_exp *lhs, *rhs; } list;
        struct { string *id; t_exp *par, *e; } fn;
        t_exp *der;
        struct { t_exp *e; string *id; } dot;
        struct { t_exp *e, *mods; } with;
        struct { t_exp *e; string *id; } as;
        struct { t_exp *lhs, *rhs; } app;
        struct { t_exp *e; t_type *type; } ty;
        struct { t_exp *lhs, *rhs, *e; } let;
        struct { struct t_decs *ds; t_exp *e; } letrec;
        struct { t_exp *e; struct t_rules *rs; } _case;
        struct { t_exp *c, *t, *f; } _if;
        struct { t_exp *lhs, *rhs; } seq;
        struct { t_exp *e; struct t_erules *rs; } except;
        struct { string *id; t_exp *e; } exception;
    };
};

typedef struct t_decs {
    string *id;
    t_exp *rhs;
    struct t_decs *next;
} t_decs;

typedef struct t_rules {
    t_exp *lhs;
    t_exp *guard;
    t_exp *rhs;
    struct t_rules *next;
} t_rules;

typedef struct t_erules {
    string *id;
    t_exp *arg;
    t_exp *rhs;
    struct t_erules *next;
} t_erules;

typedef struct t_clauses {
    t_loc loc;
    t_exp **ps;
    t_exp *guard;
    t_exp *body;
    struct t_clauses *next;
} t_clauses;

typedef struct t_op {
    string *id;
    int lhs;
    int rhs;
    struct t_op *next;
} t_op;

typedef struct t_sym {
    string *id;
    t_type *type;
    val val;
    struct t_sym *next;
} t_sym;

typedef struct t_env {
    string *id;
    val val;
    struct t_env *next;
} t_env;

int ninterns;
string *interns[65536];
string *strings[256];
string *empty_string;
string *any_id;
string *cons_id;
string *ref_id;
string *some_id;
string *match_id;
string *division_id;
string *exponent_id;
string *index_id;
string *size_id;
string *empty_id;

t_loc tokl;
char source[65536];
char tokb[sizeof source];
char *srcp;
char *linep;
bool peeked;
t_tok tok;
int toki;
string *toks;
t_op *ops;
t_op *forced_op;
t_op *and_op;
t_op *or_op;
t_op *as_op;

int global_uid;
int let_level;
t_openrec *openrecs;

t_sym *all_types;
t_sym *all_cons;
t_sym *all_exns;

t_type *unittype;
t_type *booltype;
t_type *inttype;
t_type *chartype;
t_type *stringtype;
t_type *listtype;
t_type *reftype;
t_type *opttype;

val unit;
val nil;
val falsev;
val truev;
val none;

jmp_buf *exn_buf;
string *exn_id;
val exn_arg;
t_loc exn_loc;

uint32_t randst[] = {
    123456789, 362436069, 521288629, 88675123, 5783321, 6615241
};
uint32_t randseed = 362437;


void *print(char *msg, ...);
void *printexp(t_exp *e, bool paren);
void *printval(val x, bool paren);
void *printtype(t_type *t, bool paren);
t_type *nametvs(t_type *t, int *uid);
t_exp *expr(void);
t_type *ty(void);
t_exp *clauses2fn(t_loc loc, string *id, int nps, int nclauses, t_clauses *cs);

/*

    General.

*/

string *mkstr(char *txt, int len) {
    if (len < 0) len = strlen(txt);

    string *str = malloc(sizeof *str + len + 1);
    str->len = len;
    if (txt) memcpy(str->txt, txt, len);
    str->txt[len] = 0;
    return str;
}

string *intern(char *txt, int len) {
    if (len < 0) len = strlen(txt);

    for (int i = 0; i < ninterns; i++)
        if (interns[i]->len == len && !memcmp(interns[i]->txt, txt, len))
            return interns[i];

    return interns[ninterns++] = mkstr(txt, len);
}

t_sym *find(string *id, t_sym *list) {
    for (t_sym *i = list; i; i = i->next)
        if (i->id == id) return i;
    return 0;
}

t_sym *define_val(t_sym **listp, string *id, t_type *type, val val) {
    return *listp = new(t_sym, id, type, val, *listp);
}

t_sym *define(t_sym **listp, string *id, t_type *type) {
    return define_val(listp, id, type, unit);
}

void new_order(int n, string **old, string **new, int *order) {
    for (int i = 0; i < n; i++) {
        int index = 0;
        while (new[index] != old[i]) index++;
        order[i] = index;
    }
}

int cmpstr(const void *a, const void *b) {
    return strcmp((*(const string**) a)->txt, (*(const string**) b)->txt);
}

void *printesc(unsigned char c) {
    switch (c) {
    case '\a': return print("\\a");
    case '\b': return print("\\b");
    case '\x1b': return print("\\e");
    case '\f': return print("\\f");
    case '\n': return print("\\n");
    case '\r': return print("\\r");
    case '\t': return print("\\t");
    case '\v': return print("\\v");
    case '\"': return print("\\\"");
    case '\\': return print("\\\\");
    default:
        if (c < 32) printf("\\x%02x", c);
        else putchar(c);
    }
    return 0;
}

void vprint(char *msg, va_list ap) {
    int uid = 0;

    for (char *s = msg; *s; s++)
        if (*s != '%') putchar(*s);
        else
            switch (*++s) {

            case 'd': printf("%d", va_arg(ap, int)); break;

            case 'e': printexp(va_arg(ap, t_exp*), false); break;
            case 'E': printexp(va_arg(ap, t_exp*), true); break;

            case 'l':
                {
                    t_loc loc = va_arg(ap, t_loc);
                    print("%S:%d:%d", loc.name, loc.ln, loc.col);
                    break;
                }

            case 's': fputs(va_arg(ap, char*), stdout); break;

            case 'S':
                {
                    string *str = va_arg(ap, string*);
                    fwrite(str->txt, 1, str->len, stdout);
                    break;
                }

            case 't': printtype(nametvs(va_arg(ap, t_type*), &uid), false); break;
            case 'T': printtype(nametvs(va_arg(ap, t_type*), &uid), true); break;

            case 'v': printval(va_arg(ap, val), false); break;
            case 'V': printval(va_arg(ap, val), true); break;

            case '*':
                {
                    char *msg = va_arg(ap, char*);
                    vprint(msg, *va_arg(ap, va_list*));
                    break;
                }
            }
}

void *print(char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    vprint(msg, ap);
    va_end(ap);
    return 0;
}

void *error(t_loc loc, char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    print("error %l: %*\n", loc, msg, &ap);
    exit(1);
}

/*

    Values.

*/

#define newint(I) ((val) {Int, .i=I})
#define newchar(C) ((val) {Char, .c=C})
#define newstr(STR) ((val) {String, .str=STR})
#define newlist(LS) ((val) {List, .list=LS})
#define cons(HD, TL) newlist(new(struct list, HD, TL))

val newdata(string *con, val x) {
    return (val) { Data, .data=new(struct data, con, x) };
}

val newtup(int n, val *xs) {
    struct tup *tup = malloc(sizeof *tup + n * sizeof *xs);
    tup->n = n;
    if (xs) memcpy(tup->xs, xs, n * sizeof *xs);
    return (val) { Tuple, .tup=tup };
}

val newrec(string **fs, int n, val *xs) {
    struct rec *rec = malloc(sizeof *rec + n * sizeof *xs);
    rec->fs = fs;
    rec->n = n;
    if (xs) memcpy(rec->xs, xs, n * sizeof *xs);
    return (val) { Record, .rec=rec };
}

val newfn(string *id, t_exp *lhs, t_exp *rhs, t_env *env) {
    return (val) { Fn, .fn=new(struct fn, id, lhs, rhs, env) };
}

val newhost(hostnum op, string *id, int n, int arity, val *xs) {
    struct host *host = malloc(sizeof *host + n * sizeof *xs);
    host->op = op;
    host->id = id;
    host->n = n;
    host->arity = arity;
    if (xs) memcpy(host->xs, xs, n * sizeof *xs);
    return (val) { Host, .host=host };
}

bool isunit(val x) {
    return x.form == Tuple && x.tup->n == 0;
}

void *printval(val x, bool paren) {
    if (paren)
        switch (x.form) {
        case Int:
        case Char:
        case String:
        case List:
        case Tuple:
        case Data:
        case Record:
        case Host:
            break;

        case Fn:
            return print("(%v)", x);
        }

    switch (x.form) {
    case Int: return print("%d", x.i);
    case Char:
        if (x.c == '\'') return print("'\\''");
        else if (x.c == '\"') return print("'\"'");
        else {
            print("'");
            printesc(x.c);
            return print("'");
        }

    case String:
        print("\"");
        for (int i = 0; i < x.str->len; i++)
            printesc(x.str->txt[i]);
        return print("\"");

    case List:
        print("[");
        for (struct list *i = x.list; i; i = i->tl)
            print(i->tl? "%v, ": "%v", i->hd);
        return print("]");

    case Tuple:
        print("(");
        for (int i = 0; i < x.tup->n; i++)
            print(i? ", %v": "%v", x.tup->xs[i]);
        return print(")");

    case Data:
        if (isunit(x.data->val)) return print("%S", x.data->con);

        if (paren) return print("(%v)", x);

        return print("%S %V", x.data->con, x.data->val);

    case Record:
        print("{");
        for (int i = 0; i < x.rec->n; i++)
            print(i? ", %S=%v": "%S=%v", x.rec->fs[i], x.rec->xs[i]);
        return print("}");

    case Fn: return print(x.fn->id? "%S": "#fn", x.fn->id);

    case Host: return print("%S", x.host->id);
    }
}

/*
    Types.
*/
t_type *typecon(string *id, int n, t_type **ts) {
    t_type *type = new(t_type, Type, .id=id, .n=n);
    memcpy(type->ts, ts, n * sizeof *ts);
    return type;
}

t_type *typevar(string *id) {
    return new(t_type, Typevar, .id=id, .level=let_level);
}

t_type *polyvar(string *id) {
    return new(t_type, Polyvar, .id=id);
}

t_type *tuptype(int n, t_type **ts) {
    t_type *type = new(t_type, TupType, .n=n);
    memcpy(type->ts, ts, n * sizeof *ts);
    return type;
}

t_type *rectype(int n, string **fs, t_type **ts, bool open) {
    t_type *type = new(t_type, RecType, .n=n, .fs=fs, .open=open);
    memcpy(type->ts, ts, n * sizeof *ts);
    return type;
}

t_type *fntype(t_type *lhs, t_type *rhs) {
    return new(t_type, FnType, .n=2, .ts={lhs, rhs});
}

t_type *follow(t_type *t) {
    return t->form == Typevar && *t->ts? follow(*t->ts): t;
}

bool has_typevar(t_type *t) {
    t = follow(t);

    if (t->form == Typevar || t->form == Polyvar) return true;

    for (int i = 0; i < t->n; i++)
        if (has_typevar(t->ts[i])) return true;

    return false;
}

// Name typevars before printing.
t_type *nametvs(t_type *t, int *uid) {
    t = follow(t);

    if ((t->form == Typevar || t->form == Polyvar) && t->id == 0) {
        char buf[32];
        snprintf(buf, sizeof buf, "t_%d", ++*uid);
        t->id = intern(buf, -1);
    }

    for (int i = 0; i < t->n; i++) nametvs(t->ts[i], uid);

    return t;
}

// Call nametvs() all types before printing.
void *printtype(t_type *t, bool paren) {

    t = follow(t);

    switch (t->form) {

    case Type:
        if (t->id == listtype->id) return print("[%t]", t->ts[0]);

        if (t->n) {
            print(t->ts[0]->n > 1? "(%t) ": "%T ", t->ts[0]);

            for (int i = 1; i < t->n; i++)
                print(t->ts[i]->n > 1? "and (%t) ": "and %T ", t->ts[i]);
        }

        return print("%S", t->id);

    case Typevar: return print("%S", t->id);
    case Polyvar: return print("*%S", t->id);

    case TupType:
        print("(");
        for (int i = 0; i < t->n; i++)
            print(i? ", %t": "%t", t->ts[i]);
        return print(")");

    case RecType:
        print("{");
        for (int i = 0; i < t->n; i++)
            print(i? ", %S::%t": "%S::%t", t->fs[i], t->ts[i]);
        if (t->open) print(t->n? ", _": "_");
        return print("}");

    case FnType:
        if (paren) return print("(%t)", t);
        {
            t_type *u = follow(t->ts[1]);

            return print(u->form == FnType? "%T -> %t": "%T -> %T",
                t->ts[0], t->ts[1]);
        }
    }
}

/*

    Parsing.
    Call opensrc() to open source code.
    next() parses a token.
    peek(t) returns true if the next token is t.
    want(t) consumes the next token if t.
    need(t) consumes t or raises an error.
    expr() etc recognise grammar.
*/

void setsrc(char *name, char *text) {
    tokl = (t_loc) { intern(name, -1), 1, 1 };
    srcp = linep = source;
    peeked = false;
    strcpy(source, text);
}

bool opensrc(char *path) {
    setsrc(path, "");

    FILE *file = fopen(path, "rb");
    if (!file)
        error(tokl, "cannot open source");

    source[fread(source, 1, sizeof source, file)] = 0;
    fclose(file);
    return file != 0;
}

int chr(void) {

    if (!*srcp) return 0;

    if (*srcp != '\\') return *srcp++;

    switch ((srcp += 2)[-1]) {
    case 'a':   return '\a';
    case 'b':   return '\b';
    case 'e':   return '\x1b';
    case 'f':   return '\f';
    case 'n':   return '\n';
    case 'r':   return '\r';
    case 't':   return '\t';
    case 'v':   return '\v';
    case 'x':
        {
            if (!srcp[0] || !srcp[1]) error(tokl, "unfinished escape");
            char tmp[3] = {srcp[0], srcp[1], 0};
            char *end;
            int c = strtol(tmp, &end, 16);
            if (*end) error(tokl, "invalid hex escape: \\x%s", tmp);
            srcp += 2;
            return c;
        }
    default: return srcp[-1];
    }

}

t_tok next(void) {

    if (peeked) {
        peeked = false;
        return tok;
    }

    while (true)
        if (*srcp == '\n') {
            linep = ++srcp;
            tokl.ln++;
        }
        else if (isspace(*srcp)) srcp++;
        else if (*srcp == '#')
            while (*srcp && *srcp != '\n') srcp++;
        else break;

    tokl.col = srcp - linep + 1;

    if (!*srcp) return tok = Teof;

    if (isdigit(srcp[*srcp == '-'])) {
        toki = strtol(srcp, &srcp, 10);
        return tok = Tint;
    }

    for (int i = Tder; i <= Toper; i++)
        if (*srcp == tokn[i][0]) {
            srcp++;
            return tok = i;
        }

    if (*srcp == '\'') {
        if (*++srcp == '\'') error(tokl, "empty char");
        toki = chr();
        if (*srcp++ != '\'') error(tokl, "unclosed char");
        return tok = Tchar;
    }

    if (*srcp == '"') {
        char *o = tokb;
        srcp++;

        while (*srcp)
            if (*srcp == '"') {
                srcp++;
                toks = intern(tokb, o - tokb);
                return tok = Tstring;
            }
            else if (!*srcp) error(tokl, "unclosed string");
            else *o++ = chr();
    }

    char *base = srcp;
    while (isalnum(*srcp) || *srcp == '_' || *srcp == '\'') srcp++;

    if (srcp == base)
        while (*srcp && strchr(sym_chars, *srcp)) srcp++;

    if (srcp == base) error(tokl, "invalid token");

    toks = intern(base, srcp - base);

    for (int i = Tder; i <= Tbar; i++)
        if (toks->txt == tokn[i]) return tok = i;

    return tok = Tid;
}

bool peek(t_tok t) {
    peeked = next();
    return t == tok;
}

bool want(t_tok t) {
    peeked = !peek(t);
    return !peeked;
}

string *need(t_tok t) {
    if (!want(t)) error(tokl, "need %s", tokn[t]);
    return toks;
}

t_exp *aexp(bool required);

void *printexp(t_exp *e, bool paren) {

    if (paren) {
        switch (e->form) {
        case Elit:
        case Evar:
        case Etup:
        case Erec:
        case Eder:
            break;

        case Elist:
        case Ewith:
        case Efn:
        case Eapp:
        case Edot:
        case Ety:
        case Elet:
        case Eletrec:
        case Ecase:
        case Eif:
        case Eseq:
        case Eas:
        case Eexcept:
        case Eexception:
            return print("(%e)", e);
        }
    }

    switch (e->form) {

    case Elit: return print("%v", e->lit);

    case Evar: return print("%S", e->var);

    case Elist:
        return print(e->list.rhs->form == Elist? "%E:%e": "%E:%E",
            e->list.lhs, e->list.rhs);

    case Etup:
        print("(");
        for (int i = 0; i < e->tup.n; i++)
            print(i? ", %e": "%e", e->tup.es[i]);
        return print(")");

    case Erec:
        print("{");
        for (int i = 0; i < e->rec.n; i++)
            print(i? ", %S = %e": "%S = %e", e->rec.fs[i], e->rec.es[i]);
        return print("}");

    case Eder: return print(e->der->form == Eder? "! %E": "!%E", e->der);

    case Ewith: return print("%E with %e", e->with.e, e->with.mods);

    case Efn: return print("\\%E-> %e", e->fn.par, e->fn.e);

    case Eapp:
        if (e->app.lhs->form == Eapp)
            return print("%e %E", e->app.lhs, e->app.rhs);
        return print("%E %E", e->app.lhs, e->app.rhs);

    case Edot: return print("%E.%S", e->dot.e, e->dot.id);

    case Ety: return print("%E :: %t", e->ty.e, e->ty.type);

    case Elet:
        return print("let %E = %e in %e", e->let.lhs, e->let.rhs, e->let.e);

    case Eletrec:
        print("let");
        for (t_decs *i = e->letrec.ds; i; i = i->next)
            print(" rec %S = %e", i->id, i->rhs);
        return print(" in %e", e->letrec.e);

    case Ecase:
        print("case %E", e->_case.e);
        for (t_rules *i = e->_case.rs; i; i = i->next)
            print(" | %e -> %E", i->lhs, i->rhs);
        return 0;

    case Eif:
        return print("if %e then %e else %e", e->_if.c, e->_if.t, e->_if.f);

    case Eseq: return print("%e; %e", e->seq.lhs, e->seq.rhs);

    case Eas: return print("%e @ %S", e->as.e, e->as.id);

    case Eexcept:
        print("%e except");
        for (t_erules *r = e->except.rs; r; r = r->next)
            print(" | %S with %e -> %e", r->id, r->arg, r->rhs);
        return 0;

    case Eexception:
        return print("exception %S with %e", e->exception.id, e->exception.e);
    }
}

string *gensym(void) {
    char buf[32];
    snprintf(buf, sizeof buf, "$%d", global_uid++);
    return intern(buf, -1);
}

t_type *aty(bool required) {

    switch (next()) {

    case Tid:
        {
            t_sym *sym = find(toks, all_types);
            t_type *con = sym? sym->type: 0;
            if (!con) error(tokl, "undefined type: %S", toks);
            if (con->form == Type && con->n != 0)
                error(tokl, "type needs args: %S", toks);
            return con;
        }

    case Tlparen:
        {
            t_type *ts[NARG];
            int n = 0;

            do {
                if (peek(Trparen)) break;

                if (n == NARG) error(tokl, "overflown type tuple");

                ts[n++] = ty();
            } while (want(Tcomma));
            need(Trparen);

            if (n == 0) return unittype;
            if (n == 1) return ts[0];
            return tuptype(n, ts);
        }

    case Tlbrace:
        {
            t_type *t = ty();
            need(Trbrace);
            return typecon(listtype->id, 1, &t);
        }

    case Tlcurly:
        {
            string *fs[NARG];
            t_type *ts[NARG];
            string *ofs[NARG];
            t_type *ots[NARG];
            int order[NARG];
            int n = 0;

            do {
                if (peek(Trcurly)) break;

                if (n == NARG) error(tokl, "overflown type tuple");

                if (peek(Tid) && toks == any_id)
                    error(tokl, "cannot use _ in type");

                fs[n] = need(Tid);
                ts[n++] = (need(Tty), ty());
            } while (want(Tcomma));
            need(Trcurly);

            memcpy(ofs, fs, n * sizeof *fs);
            qsort(ofs, n, sizeof *ofs, cmpstr);

            new_order(n, fs, ofs, order);
            for (int i = 0; i < n; i++)
                ots[order[i]] = ts[i];

            return rectype(n, copy(n, ofs), ots, false);
        }

    default:
        peeked = true;
        if (required) error(tokl, "need type");
        return 0;
    }

}

t_type *ty(void) {
    t_type *t = aty(true);

    while (peek(Tand)) {
        t_type *ts[NARG] = {t};
        int n = 1;

        while (want(Tand))
            if (n == NARG) error(tokl, "overflow type args");
            else ts[n++] = aty(true);

        string *id = need(Tid);
        t_sym *sym = find(id, all_types);
        t_type *con = sym? sym->type: 0;

        if (!con) error(tokl, "undefined type: %S", id);
        if (con->n != n) error(tokl, "wrong number of args to type: %S", id);

        t = typecon(id, n, ts);
    }

    while (want(Tid)) {
        string *id = toks;
        t_sym *sym = find(id, all_types);
        t_type *con = sym? sym->type: 0;

        if (!con) error(tokl, "undefined type: %S", id);
        if (con->n != 1) error(tokl, "wrong number of args to type: %S", id);

        t = typecon(id, 1, &t);
    }

    return want(Tarrow)? fntype(t, ty()): t;
}

t_op *infix(int lvl) {

    if (peek(Toper) && (lvl == 10 || lvl < 0)) return forced_op;
    if (peek(Tand) && (lvl == 3 || lvl < 0)) return and_op;
    if (peek(Tor) && (lvl == 2 || lvl < 0)) return or_op;
    if (peek(Tas) && (lvl == 1 || lvl < 0)) return as_op;

    if (peek(Tid))
        for (t_op *i = ops; i; i = i->next)
            if (toks == i->id && (i->lhs == lvl || lvl < 0)) return i;
    return 0;
}

t_exp *listexp(void) {
    t_loc loc = tokl;

    if (want(Trbrace)) return elit(loc, nil);

    t_exp *hd = expr();
    t_exp *tl = want(Tcomma)? listexp(): 0;

    if (!tl) need(Trbrace), tl = elit(loc, nil);
    return elist(loc, hd, tl);
}

t_exp *recexp(void) {
    t_loc loc = tokl;
    string *fs[NARG];
    t_exp *es[NARG];
    int n = 0;

    do {
        if (peek(Trcurly)) break;

        if (n == NARG) error(tokl, "overflown fields");

        string *id = need(Tid);
        t_loc loc = tokl;

        fs[n] = id;

        if (want(Tequal))
            es[n] = expr();
        else {
            t_sym *sym = find(id, all_cons);
            es[n] = sym? elit(loc, sym->val): evar(loc, id);
        }

        n++;
    } while (want(Tcomma));
    need(Trcurly);

    return erec(loc, n, copy(n, fs), copy(n, es));
}

t_exp *aexp(bool required) {
    t_loc loc = tokl;
    t_exp *e;

    // Do not consume infix as arg.
    if (!required && peek(Tid) && infix(-1)) return 0;

    switch (next()) {

    case Tint: e = elit(loc, newint(toki)); break;
    case Tchar: e = elit(loc, newchar(toki)); break;
    case Tstring: e = elit(loc, newstr(toks)); break;
    case Tid:
        {
            t_sym *sym = find(toks, all_cons);
            e = sym? elit(loc, sym->val): evar(loc, toks);
            break;
        }

    case Tlparen:
        {
            t_exp *es[NARG];
            int n = 0;

            do {
                if (peek(Trparen)) break;

                if (n == NARG) error(loc, "overflown tuple");

                es[n++] = expr();
            } while (want(Tcomma));
            need(Trparen);

            e =
                n == 0? elit(loc, unit):
                n == 1? es[0]:
                etup(loc, n, copy(n, es));
            break;
        }

    case Tlbrace: return listexp();

    case Tlcurly:
        e = recexp();
        break;

    case Tder:
        e = eder(loc, aexp(true));
        break;

    case Tfn:
        {
            t_clauses *cs = 0;
            int ncs = 0;
            int np = 0;

            do {
                t_loc loc = tokl;
                t_exp *ps[NARG];
                int n = 0;

                while ((e = aexp(false)))
                    if (n == NARG) error(e->loc, "overflown args");
                    else ps[n++] = e;

                t_exp *guard = want(Tif)? expr(): 0;

                if (ncs == 0) np = n;

                if (n == 0) error(loc, "no parameters given");

                if (n != np) error(loc, "case arity does not match");

                need(Tarrow);
                t_exp *body = expr();

                cs = new(t_clauses, loc, copy(n, ps), guard, body, cs);
                ncs++;
            } while (want(Tbar));

            e = clauses2fn(loc, 0, np, ncs, cs);

            break;
        }

    default:
        peeked = true;
        if (required) error(tokl, "need expression");
        return 0;
    }

    while (true) {
        loc = tokl;

        if (want(Tdot)) e = edot(loc, e, need(Tid));
        else if (want(Twith)) need(Tlcurly), e = ewith(loc, e, recexp());
        else break;
    }

    return e;
}

t_exp *iexp(int lvl) {
    if (lvl == 11) {
        t_exp *e = aexp(true);
        t_exp *x;
        while ((x = aexp(false)))
            e = eapp(e->loc, e, x);
        return e;
    }

    t_exp *e = iexp(lvl + 1);

    t_op *op;

    while ((op = infix(lvl))) {
        t_tok tok = next();
        t_loc loc = tokl;
        string *id = tok == Toper? need(Tid): op->id;
        t_sym *sym = find(id, all_cons);
        t_exp *f = sym? elit(loc, sym->val): evar(loc, id);
        t_exp *rhs = iexp(op->rhs);

        if (op == and_op) e = eif(loc, e, rhs, elit(loc, falsev));
        else if (op == or_op) e = eif(loc, e, elit(loc, truev), rhs);
        else if (id == cons_id) e = elist(loc, e, rhs);
        else if (op == as_op) {
            if (rhs->form != Evar) error(rhs->loc, "need id");
            e = eas(loc, e, rhs->var);
        }
        else e = eapp(loc, eapp(loc, f, e), rhs);
    }

    return e;
}

void set_let_body(t_exp **let, t_exp *body) {
    t_exp **p = let;
    while (*p)
        if ((*p)->form == Elet) p = &(*p)->let.e;
        else if ((*p)->form == Eletrec) p = &(*p)->letrec.e;
        else break;
    *p = body;
}

int app2params(t_exp *lhs, t_exp **ps, t_exp **f) {

    if (lhs->form != Eapp) return -1;

    int n = 0;
    t_exp *i = lhs;
    for ( ; i->form == Eapp; i = i->app.lhs)
        if (n == NARG) error(i->loc, "overflown params");
        else ps[NARG - ++n] = i->app.rhs;

    if (i->form != Evar) return -1;

    memmove(ps, ps + NARG - n, n * sizeof *ps);

    *f = i;
    return n;
}

t_exp *clauses2fn(t_loc loc, string *id, int nps, int nclauses, t_clauses *cs) {
    if (nclauses == 1) {
        t_exp *body = cs->body;

        for (int i = nps; i--; )
            body = efn(loc, id, cs->ps[i], body);
        return body;
    }
    else {
        t_rules *rs = 0;

        for ( ; cs; cs = cs->next)
            rs = new(t_rules,
                    nps == 1? cs->ps[0]: etup(cs->loc, nps, cs->ps),
                    cs->guard,
                    cs->body,
                    rs);

        t_exp **ps = malloc(nps * sizeof *ps);

        for (int i = 0; i < nps; i++)
            ps[i] = evar(loc, gensym());

        t_exp *subject = nps == 1? *ps: etup(loc, nps, ps);
        t_exp *body = ecase(loc, subject, rs);

        for (int i = nps; i--; )
            body = efn(loc, id, ps[i], body);
        return body;
    }
}

void dec(t_loc loc, t_exp **lhsp, t_exp **rhsp) {
    t_clauses *cs = 0;
    int nclauses = 0;
    string *canon_id = 0;
    int canon_n = 0;

    do {
        t_loc loc = tokl;
        t_exp *ps[NARG];
        t_exp *lhs = expr();
        t_exp *guard = want(Tif)? expr(): 0;
        t_exp *rhs = (need(Tequal), expr());
        t_exp *f;

        int n = app2params(lhs, ps, &f);

        if (n == -1) {

            if (nclauses || guard)
                error(loc, "clause does not define function");

            *lhsp = lhs;
            *rhsp = rhs;
            return;
        }

        if (nclauses == 0) {
            canon_n = n;
            canon_id = f->var;
        }

        if (f->var != canon_id) error(loc, "case name does not match");
        if (n != canon_n) error(loc, "case arity does not match");

        cs = new(t_clauses, loc, copy(n, ps), guard, rhs, cs);
        nclauses++;

    } while (want(Tbar));

    *lhsp = evar(loc, canon_id);
    *rhsp = clauses2fn(loc, canon_id, canon_n, nclauses, cs);
}

t_exp *let(t_tok cont) {
    t_loc loc = tokl;
    t_exp *e;

    if (peek(Trec)) {
        t_decs *rs = 0;
        t_decs **p = &rs;
        while (want(Trec)) {
            t_exp *lhs, *rhs;
            dec(tokl, &lhs, &rhs);

            if (lhs->form != Evar || rhs->form != Efn)
                error(rhs->loc, "rec only defines functions");

            *p = new(t_decs, lhs->var, rhs, 0);
            p = &(*p)->next;
        }
        e = eletrec(loc, rs, 0);
    }
    else {
        t_exp *lhs, *rhs;
        dec(tokl, &lhs, &rhs);
        e = elet(loc, lhs, rhs, 0);
    }

    if (want(cont)) set_let_body(&e, let(cont));

    return e;
}

t_exp *expr(void) {
    t_loc loc = tokl;
    t_exp *e;

    switch (next()) {

    case Tif:
        {
            t_exp *c = expr();
            t_exp *t = (need(Tthen), expr());
            t_exp *f = (need(Telse), expr());
            e = eif(loc, c, t, f);
            break;
        }

    case Tcase:
        {
            t_exp *sub = expr();
            t_rules *rs = 0;
            t_rules **p = &rs;

            while (want(Tbar)) {
                t_exp *lhs = expr();
                t_exp *guard = want(Tif)? expr(): 0;
                t_exp *rhs = (need(Tarrow), expr());
                *p = new(t_rules, lhs, guard, rhs, 0);
                p = &(*p)->next;
            }
            want(Tendc);
            e = ecase(loc, sub, rs);
            break;
        }

    case Tlet:
        {
            e = let(Tlet);
            set_let_body(&e, (need(Tin), expr()));
            break;
        }

    case Texception:
        {
            string *exn = need(Tid);
            t_exp *arg = want(Twith)? aexp(true): elit(loc, unit);
            e = eexception(loc, exn, arg);
            break;
        }

    default:
        peeked = true;
        e = iexp(0);
    }

    while (true) {
        loc = tokl;

        if (want(Twhere)) {
            want(Tdef);
            t_exp *decs = let(Tdef);
            set_let_body(&decs, e);
            want(Tendw);
            e = decs;
        }

        else if (want(Tty)) e = ety(loc, e, ty());

        else if (want(Tsemi)) e = eseq(loc, e, expr());

        else if (want(Texcept)) {
            t_erules *rs = 0;
            t_erules **p = &rs;

            want(Tbar);

            do {
                string *id = need(Tid);
                t_exp *arg = want(Twith)? aexp(true): elit(tokl, unit);
                t_exp *rhs = (need(Tarrow), expr());
                *p = new(t_erules, id, arg, rhs, 0);
                p = &(*p)->next;
            } while (want(Tbar));

            e = eexcept(loc, e, rs);
        }

        else break;
    }

    return e;
}

void infix_dec(bool lassoc) {
    int lvl = (need(Tint), toki);
    while (want(Tid))
        ops = new(t_op, toks, lvl, lvl + lassoc, ops);
}

void datatype_dec(void) {
    string *id = need(Tid);
    t_type *ts[NARG];
    int n = 0;

    if (find(id, all_types)) error(tokl, "redefining type: %S", id);

    if (want(Twith)) {
        while (want(Tid))
            if (n == NARG) error(tokl, "overflown type params");
            else ts[n++] = polyvar(toks);
    }

    if (want(Tty)) {
        for (int i = 0; i < n; i++) define(&all_types, ts[i]->id, ts[i]);
        t_type *t = ty();
        for (int i = 0; i < n; i++) all_types = all_types->next;
        define(&all_types, id, t);
    }
    else {
        need(Tequal);
        want(Tbar);

        t_type *dt = typecon(id, n, ts);

        define(&all_types, id, dt);
        for (int i = 0; i < n; i++) define(&all_types, ts[i]->id, ts[i]);

        do {

            string *id = need(Tid);

            if (find(id, all_cons)) error(tokl, "redefining con: %S", id);

            t_type *arg = aty(false);

            t_type *t = arg? fntype(arg, dt): dt;

            define_val(&all_cons, id, t, newdata(id, unit));

        } while (want(Tbar));

        for (int i = 0; i < n; i++) all_types = all_types->next;
    }
}

void exn_dec(void) {
    string *id = need(Tid);

    if (find(id, all_exns)) error(tokl, "redefining exception: %S", id);

    t_loc loc = tokl;
    t_type *t = want(Twith)? ty(): unittype;

    if (has_typevar(t)) error(loc, "exceptions cannot have typevars");

    define(&all_exns, id, t);
}

void top(t_exp **e) {
    while (!peek(Teof))
        if (want(Tinfixl)) infix_dec(true);
        else if (want(Tinfixr)) infix_dec(false);
        else if (want(Tdatatype)) datatype_dec();
        else if (want(Texception)) exn_dec();
        else if (want(Tlet)) set_let_body(e, let(Tlet));
        else error(tokl, "need top-level declaration");
}

/*

    Type inferencing.
    This is structured much like [Cardelli] except instead of
    non-generic list, it generalises typevars into polytypes
    as discussed in [Imai] on Didier Remy's implementation of
    Ocaml.

    Open records are an original work. They basically act like
    monomorphic typevars with required fields. Whenever one
    is introduced, it is tracked and must be unified with a
    non-open record before control gets back to the top level.


    Cardelli, Luca. Basic Polymorphic Typechecking.
        http://lucacardelli.name/Papers/BasicTypechecking.pdf

    Imai, Keigo. How OCaml type checker works
        https://okmij.org/ftp/ML/generalization.html
*/

bool unifies(t_type *t, t_type *u);

bool occurs_in(t_type *tv, t_type *type) {
    type = follow(type);

    if (type->form == Typevar) return tv == type;

    for (int i = 0; i < type->n; i++)
        if (occurs_in(tv, type->ts[i])) return true;

    return false;
}

t_type *fresh(t_type *type, t_type_swap **swaps) {
    t_type_swap *ignore = 0;
    if (!swaps) swaps = &ignore;

    type = follow(type);

    switch (type->form) {

    case Polyvar:
        {
            for (t_type_swap *i = *swaps; i; i = i->next)
                if (i->from == type) return i->to;

            *swaps = new(t_type_swap, type, typevar(0), *swaps);
            return (*swaps)->to;
        }

    case Typevar: return type;

    case Type:
    case TupType:
    case RecType:
    case FnType:
        {
            if (type->n == 0) return type;

            // Do not instantiate open records.
            if (type->form == RecType && type->open) return type;

            t_type *clone = malloc(sizeof *type);
            *clone = *type;

            for (int i = 0; i < clone->n; i++)
                clone->ts[i] = fresh(clone->ts[i], swaps);
            return clone;
        }
    }
}

t_type *generalise(t_type *type) {
    type = follow(type);

    if (type->form == Typevar) {
        if (type->level > let_level)
            *type = *polyvar(0);
        return type;
    }

    if (type->form == RecType && type->open) return type;

    for (int i = 0; i < type->n; i++)
        type->ts[i] = generalise(type->ts[i]);
    return type;
}

/*
    Merge fields from a and b.
    If subset is true, b must have all fields in a.
*/
int merge_fields(
    int na, string **afs, t_type **ats,
    int nb, string **bfs, t_type **bts,
    string **outfs, t_type **outts,
    bool subset)
{
    string **enda = afs + na;
    string **endb = bfs + nb;
    string **base = outfs;

    while (afs < enda && bfs < endb) {

        int rel = strcmp((*afs)->txt, (*bfs)->txt);

        if (rel == 0) { // Both have the same field.
            if (!unifies(*ats, *bts)) return -1; // Different type.

            *outfs++ = *afs++;
            *outts++ = *ats++;
            bfs++;
            bts++;
        }
        else if (rel < 0) // Insert a field from a.
            if (subset) return -1; // B didn't have it so fail.
            else {
                *outfs++ = *afs++;
                *outts++ = *ats++;
            }
        else { // Insert a field from b.
            *outfs++ = *bfs++;
            *outts++ = *bts++;
        }
    }

    if (afs < enda && subset) return -1; // Fields remain in a but none in B.

    while (afs < enda) {
        *outfs++ = *afs++;
        *outts++ = *ats++;
    }

    while (bfs < endb) {
        *outfs++ = *bfs++;
        *outts++ = *bts++;
    }

    if (outfs - base > NARG) return -1; // Overflown capacity.

    return outfs - base;
}

void update_let_level(t_type *type, int level) {

    if (type->form == Typevar)
        if (level < type->level) type->level = level;

    for (int i = 0; i < type->n; i++)
        update_let_level(type->ts[i], level);
}

bool unifies(t_type *t, t_type *u) {
    t = follow(t);
    u = follow(u);

    if (t->form == Typevar)
        if (t == u) return true;
        else if (occurs_in(t, u)) return false;
        else {
            update_let_level(u, t->level);
            *t->ts = u;
            return true;
        }

    else if (u->form == Typevar) return unifies(u, t);

    else if (t->form != u->form) return false;
    else if (t->id != u->id) return false;
    else {

        if (t->form == RecType) {

            if (t->open) {
                /*
                    Merge open fields and point them to a common new rec type.
                    The left is always open.
                    If the right is open, the result is open.
                    If closed, it must have all the fields of the left.
                */
                string *newfs[NARG * 2];
                t_type *newts[NARG * 2];
                int n = merge_fields(
                    t->n, t->fs, t->ts,
                    u->n, u->fs, u->ts,
                    newfs, newts, !u->open);

                if (n < 0) return false;

                t_type *newtype = rectype(n, copy(n, newfs), newts, u->open);

                *t = *typevar(0);
                *u = *typevar(0);
                *t->ts = newtype;
                *u->ts = newtype;
                return true;
            }

            else if (u->open) return unifies(u, t);

            for (int i = 0; i < t->n; i++)
                if (t->fs[i] != u->fs[i]) return false;
        }

        if (t->n != u->n) return false;

        for (int i = 0; i < t->n; i++)
            if (!unifies(t->ts[i], u->ts[i])) return false;
        return true;
    }
}

t_type *unify(t_loc loc, t_type *want, t_type *got) {
    if (!unifies(want, got)) {
        int uid = 0;
        nametvs(want, &uid);
        nametvs(got, &uid);
        error(loc, "type mismatch\nwant: %t\ngot : %t", want, got);
    }
    return got;
}

t_type *littype(val x) {
    switch (x.form) {
    case Int: return inttype;
    case Char: return chartype;
    case String: return stringtype;
    case List: assert(x.list == 0); return listtype;
    case Data: return find(x.data->con, all_cons)->type;
    case Tuple: assert(x.tup->n == 0); return unittype;
    case Record: assert(!"records are never literal"); return 0;
    case Fn: assert(!"functions are never literal"); return 0;
    case Host: return fresh(hosts[x.host->op].type, 0);
    }
}

bool nonexpansive(t_exp *e) {
    switch (e->form) {
    case Elit:
    case Evar:
    case Efn:
        return true;

    case Elist: return nonexpansive(e->list.lhs) && nonexpansive(e->list.rhs);

    case Etup:
        for (int i = 0; i < e->tup.n; i++)
            if (!nonexpansive(e->tup.es[i])) return false;
        return true;

    case Erec:
        for (int i = 0; i < e->rec.n; i++)
            if (!nonexpansive(e->rec.es[i])) return false;
        return true;

    case Eder: return nonexpansive(e->der);
    case Edot: return nonexpansive(e->dot.e);
    case Ewith: return nonexpansive(e->with.e) && nonexpansive(e->with.mods);

    case Eapp:
        {
            t_exp *tmp = e;
            while (tmp->form == Eapp && nonexpansive(tmp->app.rhs))
                tmp = tmp->app.lhs;
            if (tmp->form != Elit || tmp->lit.form != Data) return false;
            return tmp->lit.data->con != ref_id;
        }

    case Ety: return nonexpansive(e->ty.e);

    case Elet:
    case Eletrec:
    case Ecase:
    case Eif:
    case Eseq:
    case Eas:
    case Eexception:
    case Eexcept:
        return false;
    }
}

t_type *checkpat(t_exp *e, t_sym **env) {
    t_type *t, *u;

    switch (e->form) {

    case Elit: return fresh(littype(e->lit), 0);

    case Evar:

        if (e->var == any_id) return typevar(0);

        return define(env, e->var, typevar(0))->type;

    case Eas:
        t = checkpat(e->as.e, env);

        if (e->as.id != any_id) define(env, e->as.id, t);

        return t;

    case Elist:
        t = checkpat(e->list.lhs, env);
        u = checkpat(e->list.rhs, env);
        return unify(e->list.rhs->loc, typecon(listtype->id, 1, &t), u);

    case Etup:
        {
            t_type *ts[NARG];
            for (int i = 0; i < e->tup.n; i++)
                ts[i] = checkpat(e->tup.es[i], env);
            return tuptype(e->tup.n, ts);
        }

    case Erec:
        {
            string *ofs[NARG];
            t_type *ts[NARG];
            t_type *ots[NARG];
            int order[NARG];
            bool open = false;
            int n = 0;

            for (int i = 0; i < e->rec.n; i++)

                if (e->rec.fs[i] == any_id) {
                    if (open) error(e->loc, "superflous _");

                    else if (e->rec.es[i]->form != Evar ||
                            e->rec.es[i]->var != any_id)
                        error(e->loc, "cannot assign _");

                    else open = true;
                }
                else {
                    ts[n] = checkpat(e->rec.es[i], env);
                    e->rec.fs[n] = e->rec.fs[i];
                    e->rec.es[n] = e->rec.es[i];
                    ofs[n] = e->rec.fs[i];
                    n++;
                }

            e->rec.n = n;
            qsort(ofs, n, sizeof *ofs, cmpstr);
            new_order(n, e->rec.fs, ofs, order);
            for (int i = 0; i < n; i++)
                ots[order[i]] = ts[i];

            for (int i = 1; i < n; i++)
                if (ofs[i] == ofs[i - 1])
                    error(e->loc, "duplicate field: %S", ofs[i]);

            t = rectype(n, copy(n, ofs), ots, open);

            if (open) openrecs = new(t_openrec, e, t, openrecs);

            return t;
        }

    case Eder:
        t = checkpat(e->der, env);
        return typecon(ref_id, 1, &t);

    case Ety: return unify(e->loc, checkpat(e->ty.e, env), e->ty.type);

    case Eapp:
        {
            t_exp *f = e->app.lhs;
            t_exp *x = e->app.rhs;

            if (f->form != Elit || f->lit.form != Data) goto invalid;

            t_type *ft = fresh(find(f->lit.data->con, all_cons)->type, 0);
            t_type *xt = checkpat(x, env);

            ft = follow(ft);

            if (ft->form == FnType)
                unify(x->loc, ft->ts[0], xt);
            else
                unify(f->loc, fntype(xt, typevar(0)), ft);

            return follow(ft)->ts[1];
        }

    case Efn:
    case Edot:
    case Ewith:
    case Elet:
    case Eletrec:
    case Ecase:
    case Eif:
    case Eseq:
    case Eexcept:
    case Eexception:

        invalid:

        return error(e->loc, "invalid pattern: %e", e);
    }
}

t_type *check(t_exp *e, t_sym *env) {
    t_type *t, *u, *v;

    switch (e->form) {
    case Elit: return fresh(littype(e->lit), 0);

    case Evar:
        {
            t_sym *sym = find(e->var, env);
            if (!sym) error(e->loc, "undefined symbol: %S", e->var);

            if (!isunit(sym->val))
                *e = *elit(e->loc, sym->val);

            return fresh(sym->type, 0);
        }

    case Elist:
        t = check(e->list.lhs, env);
        u = check(e->list.rhs, env);
        return unify(e->list.rhs->loc, typecon(listtype->id, 1, &t), u);

    case Etup:
        {
            t_type *ts[NARG];
            for (int i = 0; i < e->tup.n; i++)
                ts[i] = check(e->tup.es[i], env);
            return tuptype(e->tup.n, ts);
        }

    case Erec:
        {
            string *ofs[NARG];
            t_type *ts[NARG];
            t_type *ots[NARG];
            int order[NARG];

            for (int i = 0; i < e->rec.n; i++) {
                if (e->rec.fs[i] == any_id)
                    error(e->rec.es[i]->loc, "_ cannot be used as a field");
                ts[i] = check(e->rec.es[i], env);
            }

            memcpy(ofs, e->rec.fs, e->rec.n * sizeof *ofs);
            qsort(ofs, e->rec.n, sizeof *ofs, cmpstr);
            new_order(e->rec.n, e->rec.fs, ofs, order);
            for (int i = 0; i < e->rec.n; i++)
                ots[order[i]] = ts[i];

            for (int i = 1; i < e->rec.n; i++)
                if (ofs[i] == ofs[i - 1])
                    error(e->loc, "duplicate field: %S", ofs[i]);

            return rectype(e->rec.n, copy(e->rec.n, ofs), ots, false);
        }

    case Eder:
        t = typevar(0);
        u = check(e->der, env);
        unify(e->der->loc, typecon(ref_id, 1, &t), u);
        return t;

    case Efn:
        t = checkpat(e->fn.par, &env);
        u = check(e->fn.e, env);
        return fntype(t, u);

    case Edot:
        t = check(e->dot.e, env);
        u = typevar(0);
        v = rectype(1, copy(1, &e->dot.id), &u, true);

        openrecs = new(t_openrec, e, v, openrecs);

        unify(e->dot.e->loc, v, t);
        return u;

    case Ewith:
        t = check(e->with.e, env);
        u = check(e->with.mods, env);

        u->open = true;
        openrecs = new(t_openrec, e->with.mods, u, openrecs);

        unify(e->with.e->loc, u, t);
        return t;

    case Eapp:
        u = check(e->app.lhs, env);
        v = check(e->app.rhs, env);

        u = follow(u);
        if (u->form == FnType)
            unify(e->app.rhs->loc, u->ts[0], v);
        else
            unify(e->loc, u, fntype(v, typevar(0)));

        return follow(u)->ts[1];

    case Ety: return unify(e->loc, check(e->ty.e, env), e->ty.type);

    case Elet:
        let_level++;

        t = check(e->let.rhs, env);
        u = checkpat(e->let.lhs, &env);
        unify(e->let.rhs->loc, u, t);

        let_level--;
        if (nonexpansive(e->let.rhs)) generalise(t);

        return check(e->let.e, env);

    case Eletrec:
        let_level++;

        for (t_decs *d = e->letrec.ds; d; d = d->next)
            define(&env, d->id, typevar(0));

        for (t_decs *d = e->letrec.ds; d; d = d->next)
            unify(d->rhs->loc,
                check(d->rhs, env),
                find(d->id, env)->type);

        let_level--;
        for (t_decs *d = e->letrec.ds; d; d = d->next)
            generalise(find(d->id, env)->type);

        return check(e->letrec.e, env);

    case Ecase:
        u = check(e->_case.e, env);
        t = typevar(0);

        for (t_rules *r = e->_case.rs; r; r = r->next) {
            t_sym *local = env;
            unify(r->lhs->loc, u, checkpat(r->lhs, &local));
            if (r->guard)
                unify(r->guard->loc, booltype, check(r->guard, local));
            unify(r->rhs->loc, t, check(r->rhs, local));
        }

        return t;

    case Eif:
        unify(e->_if.c->loc, booltype, check(e->_if.c, env));
        t = check(e->_if.t, env);
        return unify(e->_if.f->loc, t, check(e->_if.f, env));

    case Eseq:
        check(e->seq.lhs, env);
        return check(e->seq.rhs, env);

    case Eas:
        return error(e->loc, "@ is not usable as an expression");

    case Eexception:
        {
            t_sym *sym = find(e->exception.id, all_exns);
            if (!sym) error(e->loc, "undefined exception: %S", e->exception.id);
            t = check(e->exception.e, env);
            unify(e->exception.e->loc, sym->type, t);
            return typevar(0);
        }

    case Eexcept:
        t = check(e->except.e, env);
        for (t_erules *r = e->except.rs; r; r = r->next) {

            if (r->id == match_id) {
                if (r->arg->form != Evar || r->arg->var != any_id)
                    error(e->loc, "match can only be handled with arg _");

                u = check(r->rhs, env);
                unify(r->rhs->loc, t, u);
            }
            else {
                t_sym *sym = find(r->id, all_exns);

                if (!sym) error(e->loc, "undefined exception: %S", r->id);

                t_sym *local = env;
                u = checkpat(r->arg, &local);
                unify(r->arg->loc, sym->type, u);

                u = check(r->rhs, local);
                unify(r->rhs->loc, t, u);
            }
        }
        return t;
    }

    return error(e->loc, "UNIMPLEMENTED");
}

void check_open_recs(void) {
    t_type *t;
    while (openrecs && (t = follow(openrecs->type)) && !t->open)
        openrecs = openrecs->next;

    if (openrecs)
        error(openrecs->e->loc, "type of record cannot be determined: %t",
            openrecs->type);
}

/*

    Evaluation.

*/

t_env *to_env(t_sym *syms) {
    return syms? new(t_env, syms->id, syms->val, to_env(syms->next)): 0;
}

val *field(struct rec *rec, string *id) {
    for (int i = 0; i < rec->n; i++)
        if (rec->fs[i] == id) return &rec->xs[i];
    return 0;
}

bool equal(val a, val b) {
    switch (a.form) {
    case Int: return a.i == b.i;
    case Char: return a.c == b.c;

    case String:
        return a.str == b.str ||
            (a.str->len == b.str->len &&
            !memcmp(a.str->txt, b.str->txt, a.str->len));

    case List:
        {
            struct list *i = a.list;
            struct list *j = b.list;

            if (i == j) return true;

            while (i && j && equal(i->hd, j->hd))
                i = i->tl, j = j->tl;

            return !i && !j;
        }

    case Data:
        if (a.data->con == ref_id) return a.data == b.data;

        return a.data == b.data ||
            (a.data->con == b.data->con &&
            equal(a.data->val, b.data->val));

    case Tuple:
        for (int i = 0; i < a.tup->n; i++)
            if (!equal(a.tup->xs[i], b.tup->xs[i])) return false;
        return true;

    case Record:
        for (int i = 0; i < a.rec->n; i++) {
            val bv = *field(b.rec, a.rec->fs[i]);

            if (!equal(a.rec->xs[i], bv)) return false;
        }
        return true;

    case Fn: return a.fn == b.fn;

    case Host: return a.host == b.host;
    }
}

val exception(t_loc loc, string *id, val arg) {
    exn_id = id;
    exn_arg = arg;
    exn_loc = loc;

    if (exn_buf)
        longjmp(*exn_buf, 1);

    print("exception %l: %S with %v\n", loc, id, arg);
    exit(-1);
}

val evalhost(t_loc loc, hostnum op, val *ap) {
    #define A ap[0]
    #define B ap[1]
    #define C ap[2]
    #define D ap[3]
    #define E ap[4]

    switch (op) {
    case Oprn:
        if (ap->form == String) print("%S", ap->str);
        else if (ap->form == Char) putchar(ap->c);
        else print("%v", *ap);
        return *ap;

    case Oadd: return newint(A.i + B.i);
    case Osub: return newint(A.i - B.i);
    case Omul: return newint(A.i * B.i);
    case Odiv: return B.i? newint(A.i / B.i): exception(loc, division_id, A);
    case Orem: return B.i? newint(A.i % B.i): exception(loc, division_id, A);

    case Opow:
        {
            int n = A.i;
            if (B.i == 0) return newint(1);
            if (B.i < 0) return exception(loc, exponent_id, B);
            for (int i = 1; i < B.i; i++) n += n;
            return newint(n);
        }
    case Olt: return A.i < B.i? truev: falsev;
    case Ogt: return A.i > B.i? truev: falsev;
    case Ole: return A.i <= B.i? truev: falsev;
    case Oge: return A.i >= B.i? truev: falsev;
    case Oeq: return equal(A, B)? truev: falsev;
    case One: return equal(A, B)? falsev: truev;
    case Oexit: exit(A.i);
    case Oord: return newint(A.c);
    case Ochr: return newchar(abs(A.i) % 255);
    case Ocharcmp: return newint(A.c - B.c);

    case Oimplode:
        {
            int n = 0;
            for (struct list *i = A.list; i; i = i->tl)
                n++;

            if (n == 0) return newstr(empty_string);
            if (n == 1) return newstr(strings[A.list->hd.c]);

            string *str = mkstr(0, n);
            n = 0;
            for (struct list *i = A.list; i; i = i->tl)
                str->txt[n++] = i->hd.c;
            return newstr(str);
        }

    case Ochartostr:
        return newstr(strings[A.c]);

    case Ocat:
        {
            if (A.str->len == 0) return B;
            if (B.str->len == 0) return A;

            string *str = mkstr(0, A.str->len + B.str->len);
            memcpy(str->txt, A.str->txt, A.str->len);
            memcpy(str->txt + A.str->len, B.str->txt, B.str->len);
            return newstr(str);
        }

    case Ojoin:
        {
            int n = 0;
            for (struct list *i = A.list; i; i = i->tl)
                n += i->hd.str->len;

            if (n == 0) return newstr(empty_string);
            if (A.list->tl == 0) return A.list->hd;

            string *str = mkstr(0, n);
            n = 0;
            for (struct list *i = A.list; i; i = i->tl) {
                memcpy(str->txt + n, i->hd.str->txt, i->hd.str->len);
                n += i->hd.str->len;
            }
            return newstr(str);
        }

    case Ostrlen: return newint(A.str->len);

    case Ocharat:
        {
            string *str = A.str;
            int i = B.i;
            if (i < 0) i += str->len;
            if (i < 0 || i >= str->len) exception(loc, index_id, B);
            return newchar(str->txt[i]);
        }

    case Osubstr:
        {
            string *str = A.str;
            int i = B.i;
            int n = C.i;

            if (i < 0) i += str->len;
            if (n < 0) n = str->len + n - i + 1;

            if (i < 0 || i >= str->len) exception(loc, index_id, B);

            if (n < 0 || i + n > str->len) exception(loc, size_id, C);

            if (n == 0) return newstr(empty_string);

            if (i == 0 && n == str->len) return A;

            return newstr(mkstr(str->txt + i, n));
        }

    case Ostrcmp:
        {
            if (A.str == B.str) return newint(0);

            int n = A.str->len < B.str->len? A.str->len: B.str->len;
            int r = memcmp(A.str->txt, B.str->txt, n);

            return newint(r? r:
                    A.str->len < B.str->len? -1:
                    A.str->len == B.str->len? 0:
                    1);
        }

    case Osubstrcmp:
        {
            string *a = A.str;
            int ai = B.i;
            string *b = C.str;
            int bi = D.i;
            int n = E.i;

            if (ai < 0) ai += a->len;
            if (bi < 0) bi += b->len;
            if (n < 0) n = b->len + n - bi + 1;

            if (ai < 0 || ai > a->len) exception(loc, index_id, B);
            if (bi < 0 || bi > b->len) exception(loc, index_id, D);
            if (ai + n > a->len) exception(loc, size_id, E);
            if (bi + n > b->len) exception(loc, size_id, E);

            if (a == b) return newint(0); // Must come after checks.

            int r = memcmp(a->txt + ai, b->txt + bi, n);
            return newint(r? r: 0);
        }

    case Ofindsubstr:
        {
            string *a = A.str;
            int ai = B.i;
            string *b = C.str;
            int bi = D.i;
            int n = E.i;

            if (ai < 0) ai += a->len;
            if (bi < 0) bi += b->len;
            if (n < 0) n = b->len + n - bi + 1;

            if (ai < 0 || ai > a->len) exception(loc, index_id, B);
            if (bi < 0 || bi > b->len) exception(loc, index_id, D);
            if (bi + n > b->len) exception(loc, size_id, E);

            if (n == 0) exception(loc, empty_id, unit);

            if (n == 1) {
                char *ptr = memchr(a->txt + ai, b->txt[bi], a->len - n + 1);
                return ptr? newdata(some_id, newint(ptr - a->txt)): none;
            }

            char *ptr = a->txt + ai;
            char *end = a->txt + a->len - n + 1;

            while (ptr < end) {

                ptr = memchr(ptr, b->txt[bi], end - ptr);

                if (!ptr) return none;

                if (!memcmp(ptr, b->txt + bi, n))
                    return newdata(some_id, newint(ptr - a->txt));

                ptr++;
            }
            return none;
        }

    case Ofindchar:
        {
            string *src = A.str;
            int i = B.i;

            if (i < 0) i += src->len;
            if (i < 0 || i >= src->len) exception(loc, index_id, B);

            char *ptr = memchr(src->txt + i, C.c, src->len - i + 1);
            return ptr? newdata(some_id, newint(ptr - src->txt)): none;
        }

    case Ostrsplice:
        {
            string *src = A.str;
            int at = B.i;
            int dn = C.i;

            if (at < 0) at += src->len;
            if (dn < 0) dn = src->len + dn - at + 1;

            if (at < 0 || at > src->len) exception(loc, index_id, B);
            if (dn < 0 || at + dn > src->len) exception(loc, size_id, C);

            int m = 0;
            for (struct list *i = D.list; i; i = i->tl)
                m += i->hd.str->len;

            string *str = mkstr(0, src->len - dn + m);
            memcpy(str->txt, src->txt, at);
            memcpy(str->txt + at + m, src->txt + at + dn, src->len - at - dn);

            m = at;
            for (struct list *i = D.list; i; i = i->tl) {
                memcpy(str->txt + m, i->hd.str->txt, i->hd.str->len);
                m += i->hd.str->len;
            }
            return newstr(str);
        }

    case Oatoi: return newint(strtol(A.str->txt, 0, 10));

    case Oitoa:
        {
            char buf[32];
            int len = snprintf(buf, sizeof buf, "%d", A.i);
            return newstr(mkstr(buf, len));
        }

    case Oset: return (A.data->val = B);

    case Ogetenviron:
        {
            extern char **environ;
            val list = nil;
            int n = 0;
            for (char **i = environ; *i; i++) n++;
            for (char **i = environ + n; --i > environ; ) {
                char *spec = *i;
                int keylen = strcspn(spec, "=");
                val key = newstr(intern(spec, keylen));
                val value = spec[keylen]?
                    newstr(intern(spec + keylen + 1, -1)):
                    newstr(empty_string);
                list = cons(newtup(2, (val[]) { key, value }), list.list);
            }
            return list;
        }

    case Osysopen:
        {
            int mode =
                ((C.i / 1000 % 10) << 3*3) +
                ((C.i / 100 % 10) << 3*2) +
                ((C.i / 10 % 10) << 3*1) +
                ((C.i / 1 % 10) << 3*0);
            return newint(open(A.str->txt, B.i, mode));
        }

    case Osysclose: return newint(close(A.i));

    case Osysread:
        {
            static char *buf;
            static int szbuf;

            int sz = B.i;
            if (sz < 0) return newint(-1);

            if (sz > szbuf) {
                szbuf = sz;
                buf = malloc(szbuf);
            }

            int n = read(A.i, buf, sz);

            val out = n > 0?
                newstr(mkstr(buf, n)):
                newstr(empty_string);

            if (szbuf > 64*1024) {
                szbuf = 0;
                free(buf);
            }

            return out;
        }

    case Osyswrite: return newint(write(A.i, B.str->txt, B.str->len));

    case Osrand:
        randseed = A.i;
        return unit;

    case Orand:
        {
            // xorwow algorithm.
            uint32_t t;
            t = (randst[0] ^ (randst[0] >> 2));
            randst[0] = randst[1];
            randst[1] = randst[2];
            randst[2] = randst[3];
            randst[3] = randst[4];
            randst[4] = (randst[4] ^ (randst[4] << 4)) ^ (t ^ (t << 1));
            return newint(abs((int) ((randst[5] += randseed) + randst[4])));
        }
    }
}

t_env *match(t_exp *p, val x, t_env *env) {

    if (!env) return 0;

    switch (p->form) {
    case Elit: return equal(p->lit, x)? env: 0;

    case Evar: return p->var == any_id? env: new(t_env, p->var, x, env);

    case Eas:
        if (!(env = match(p->as.e, x, env))) return 0;

        return new(t_env, p->as.id, x, env);

    case Elist:
        if (!x.list) return 0;

        return match(p->list.rhs, newlist(x.list->tl),
            match(p->list.lhs, x.list->hd, env));

    case Etup:
        for (int i = 0; env && i < p->tup.n; i++)
            env = match(p->tup.es[i], x.tup->xs[i], env);
        return env;

    case Erec:
        for (int i = 0; env && i < p->rec.n; i++) {
            val bv = *field(x.rec, p->rec.fs[i]);
            env = match(p->rec.es[i], bv, env);
        }
        return env;

    case Eder:
        return match(p->der, x.data->val, env);

    case Eapp:
        return p->app.lhs->lit.data->con == x.data->con?
            match(p->app.rhs, x.data->val, env):
            0;
    case Ety:
        return match(p->ty.e, x, env);

    case Efn:
    case Edot:
    case Ewith:
    case Elet:
    case Eletrec:
    case Ecase:
    case Eif:
    case Eseq:
    case Eexcept:
    case Eexception:
        return error(p->loc, "UNREACHABLE");
    }
}

val eval(t_exp *e, t_env *env) {
    val f, x, y;

    tail:

    switch (e->form) {

    case Elit: return e->lit;

    case Evar:
        for (t_env *i = env; i; i = i->next)
            if (i->id == e->var) return i->val;
        return error(e->loc, "%S SHOULD HAVE BEEN DEFINED", e->var), unit;

    case Elist:
        x = eval(e->list.lhs, env);
        return cons(x, eval(e->list.rhs, env).list);

    case Etup:
        x = newtup(e->tup.n, 0);
        for (int i = 0; i < e->tup.n; i++)
            x.tup->xs[i] = eval(e->tup.es[i], env);
        return x;

    case Erec:
        x = newrec(e->rec.fs, e->rec.n, 0);
        for (int i = 0; i < e->rec.n; i++)
            x.rec->xs[i] = eval(e->rec.es[i], env);
        return x;

    case Eder: return eval(e->der, env).data->val;

    case Efn: return newfn(e->fn.id, e->fn.par, e->fn.e, env);

    case Edot: return *field(eval(e->dot.e, env).rec, e->dot.id);

    case Ewith:
        y = eval(e->with.e, env);
        x = newrec(y.rec->fs, y.rec->n, y.rec->xs);
        for (int i = 0; i < e->with.mods->rec.n; i++)
            *field(x.rec, e->with.mods->rec.fs[i]) =
                eval(e->with.mods->rec.es[i], env);
        return x;

    case Eapp:
        f = eval(e->app.lhs, env);
        x = eval(e->app.rhs, env);

        if (f.form == Data) return newdata(f.data->con, x);

        if (f.form == Host) {
            val xs[NARG];
            memcpy(xs, f.host->xs, f.host->n * sizeof *xs);
            xs[f.host->n] = x;

            if (f.host->arity > 1)
                return newhost(f.host->op, f.host->id,
                    f.host->n + 1, f.host->arity - 1,
                    xs);

            return evalhost(e->loc, f.host->op, xs);
        }

        if (!(env = match(f.fn->lhs, x, f.fn->env)))
            exception(e->loc, match_id, x);
        e = f.fn->rhs;
        goto tail;

    case Ety: return eval(e->ty.e, env);

    case Elet:
        x = eval(e->let.rhs, env);
        if (!(env = match(e->let.lhs, x, env)))
            exception(e->loc, match_id, x);
        e = e->let.e;
        goto tail;

    case Eletrec:
        {
            t_env *base = env;
            for (t_decs *d = e->letrec.ds; d; d = d->next)
                env = new(t_env, d->id, eval(d->rhs, env), env);
            for (t_env *i = env; i != base; i = i->next)
                i->val.fn->env = env;
            e = e->letrec.e;
            goto tail;
        }

    case Ecase:
        {
            t_env *base = env;
            x = eval(e->_case.e, env);
            for (t_rules *r = e->_case.rs; r; r = r->next)
                if ((env = match(r->lhs, x, base)) &&
                    (!r->guard ||
                    eval(r->guard, env).data->con == truev.data->con))
                {
                    e = r->rhs;
                    goto tail;
                }
            return exception(e->loc, match_id, x);
        }

    case Eif:
        x = eval(e->_if.c, env);
        e = x.data == truev.data? e->_if.t: e->_if.f;
        goto tail;

    case Eseq:
        eval(e->seq.lhs, env);
        e = e->seq.rhs;
        goto tail;

    case Eexcept:
        {
            jmp_buf *old = exn_buf;
            jmp_buf new;
            exn_buf = &new;

            if (setjmp(new)) {
                exn_buf = old;

                for (t_erules *r = e->except.rs; r; r = r->next) {

                    if (r->id != exn_id) continue;

                    t_env *local = match(r->arg, exn_arg, env);

                    if (!local) continue;

                    e = r->rhs;
                    env = local;
                    goto tail;
                }

                return exception(exn_loc, exn_id, exn_arg);

            }
            else {

                x = eval(e->except.e, env);
                exn_buf = old;
                return x;

            }
        }

    case Eexception:
        return exception(e->loc, e->exception.id, eval(e->exception.e, env));

    case Eas: return unit;
    }

    return error(e->loc, "UNEVALUATED"), unit;
}

int main(int argc, char **argv) {
    (void) argc;

    for (size_t i = 0; i < sizeof tokn / sizeof *tokn; i++)
        tokn[i] = intern(tokn[i], -1)->txt;

    for (int i = 0; i < 256; i++)
        strings[i] = intern((char[]){i}, 1);
    empty_string = intern("", 0);
    any_id = intern("_", -1);
    cons_id = intern(":", -1);
    ref_id = intern("ref", -1);
    some_id = intern("Some", -1);
    match_id = intern("match", -1);
    division_id = intern("division", -1);
    exponent_id = intern("exponent", -1);
    index_id = intern("index", -1);
    size_id = intern("size", -1);
    empty_id = intern("empty", -1);

    t_type *a = polyvar(0);
    unittype = tuptype(0, 0);
    booltype = typecon(intern("bool", -1), 0, 0);
    inttype = typecon(intern("int", -1), 0, 0);
    chartype = typecon(intern("char", -1), 0, 0);
    stringtype = typecon(intern("string", -1), 0, 0);
    listtype = typecon(intern("list", -1), 1, &a);
    reftype = typecon(intern("ref", -1), 1, &a);
    opttype = typecon(intern("option", -1), 1, &a);

    define(&all_types, intern("unit", -1), unittype);
    define(&all_types, booltype->id, booltype);
    define(&all_types, inttype->id, inttype);
    define(&all_types, chartype->id, chartype);
    define(&all_types, stringtype->id, stringtype);
    define(&all_types, listtype->id, listtype);
    define(&all_types, ref_id, reftype);
    define(&all_types, opttype->id, opttype);

    define(&all_exns, match_id, a);

    unit = newtup(0, 0);
    nil = (val) { List, .list=0 };
    falsev = newdata(intern("false", -1), unit);
    truev = newdata(intern("true", -1), unit);
    none = newdata(intern("None", -1), unit);

    define_val(&all_cons, falsev.data->con, booltype, falsev);
    define_val(&all_cons, truev.data->con, booltype, truev);
    define_val(&all_cons, ref_id, fntype(a, reftype), newdata(ref_id, unit));
    define_val(&all_cons, none.data->con, opttype, newdata(none.data->con, unit));
    define_val(&all_cons, some_id, fntype(a, opttype), newdata(some_id, unit));

    forced_op = new(t_op, intern("`", -1), 10, 11, 0);
    and_op = ops = new(t_op, intern("and", -1), 3, 3, ops);
    or_op = ops = new(t_op, intern("or", -1), 2, 2, ops);
    as_op = ops = new(t_op, intern("@", -1), 0, 1, ops);

    t_exp *e = 0;

    char *boot =
        "infixl 10\n"
        "infixl 9                    # `\n"
        "infixr 9 ** of\n"
        "infixl 8 * / rem\n"
        "infixl 7 + - ^\n"
        "infixr 6 :\n"
        "infixl 5 == <> <= >= < >\n"
        "infixr 4 := ++\n"
        "infixr 3                    # and\n"
        "infixr 2                    # or\n"
        "infixl 1 &\n"
        "infixr 0 $\n"
        "exception division with int\n"
        "exception exponent with int\n"
        "exception index with int\n"
        "exception size with int\n"
        "exception empty\n"
    ;
    setsrc("boot", boot);
    top(&e);

    t_sym *senv = 0;

    define_val(&senv, intern("O_RDONLY", -1), inttype, newint(O_RDONLY));
    define_val(&senv, intern("O_WRONLY", -1), inttype, newint(O_WRONLY));
    define_val(&senv, intern("O_RDWR", -1), inttype, newint(O_RDWR));
    define_val(&senv, intern("O_EXCL", -1), inttype, newint(O_EXCL));
    define_val(&senv, intern("O_NONBLOCK", -1), inttype, newint(O_NONBLOCK));
    define_val(&senv, intern("O_CREAT", -1), inttype, newint(O_CREAT));
    define_val(&senv, intern("O_TRUNC", -1), inttype, newint(O_TRUNC));
    define_val(&senv, intern("O_SYNC", -1), inttype, newint(O_SYNC));

    define(&all_types, intern("a", -1), a);
    for (size_t i = 0; i < sizeof hosts / sizeof *hosts; i++) {
        struct hostspec h = hosts[i];
        setsrc(h.id, h.typespec);
        hosts[i].type = ty();
        define_val(&senv, intern(h.id, -1), hosts[i].type,
            newhost(i, intern(h.id, -1), 0, h.arity, 0));
    }
    all_types = all_types->next;

    if (argc == 1) {
        print("usage: %s FILE ARUMENTS\n", argv[1]? argv[1]: "paperml");
        exit(0);
    }

    // Set argv.
    val args = nil;
    for (int i = argc; --i; )
        args = cons(newstr(intern(argv[i], -1)), args.list);
    define_val(&senv, intern("argv", -1),
        typecon(listtype->id, 1, &stringtype), args);

    opensrc("std.al");
    top(&e);

    opensrc(argv[1]);
    top(&e);

    set_let_body(&e, evar(tokl, intern("main", -1)));

    t_type *t = check(e, senv);
    check_open_recs();
    // print("> %e\n", e);
    // print(":: %t\n", t);
    (void) t;
    eval(e, to_env(senv));
}
