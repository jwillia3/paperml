#include <sys/_stdarg.h>
#define NARG 8

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define copy(N, X) memcpy(malloc(N * sizeof *X), X, N * sizeof *X)
#define new(TYPE, ...) (TYPE*) copy(1, (&(TYPE) { __VA_ARGS__ }))

typedef struct { int len; char txt[]; } string;
typedef struct { string *name; int ln, col; } t_loc;

typedef enum {
    Teof, Tint, Tchar, Tstring, Tid, Tlparen, Trparen, Tlbrace, Trbrace,
    Tlcurly, Trcurly, Tcomma, Tdot, Tsemi, Tfn, Toper, Tder, Tarrow, Tty,
    Tequal, Talso, Tand, Tcase, Tdatatype, Telse, Tendc, Tendf, Tendw,
    Tif, Tin, Tinfixl, Tinfixr, Tlet, Tor, Trec, Tthen, Twith, Twhere, Tbar,
} t_tok;

char *tokn[] = {
    "eof", "int", "char", "string", "id", "(", ")", "[", "]",
    "{", "}", ",", ".", ";", "\\", "`", "!", "->", "::", "=", "also",
    "and", "case", "datatype", "else", "endc", "endf", "endw", "if", "in",
    "infixl", "infixr", "let", "or", "rec", "then", "with", "where", "|",
};

char *sym_chars = "!%&$+-/:<=>?@~^|*";

typedef struct val {
    enum { Int, Char, String, Data, Record, Fn } form;
    union {
        int i;
        unsigned char c;
        string *str;
        struct data *data;
        struct rec *rec;
        struct fn *fn;
    };
} val;

struct data { string *con; int n; val xs[]; };
struct rec { int n; string **fs; val xs[]; };

typedef struct t_type t_type;
struct t_type {
    enum { Type, Typevar, Polyvar, RecType, FnType } form;
    int n;
    string *id;
    string **fs;
    t_type *ts[NARG];
};

typedef struct t_exp t_exp;
struct t_exp {
    enum {
        Elit, Eid, Erec, Eder, Efn, Edot, Eapp, Ety, Elet,
        Eletrec, Ecase, Eif,
    } form;

    t_loc loc;

    union {
        val lit;
        string *id;
        struct { int n; string **fs; t_exp **es; } rec;
        struct { string *id; t_exp *par, *e; } fn;
        t_exp *der;
        struct { t_exp *e; string *id; } dot;
        struct { t_exp *lhs, *rhs; } app;
        struct { t_exp *e; t_type *type; } ty;
        struct { t_exp *lhs, *rhs, *e; } let;
        struct { struct rules *ds; t_exp *e; } letrec;
        struct { t_exp *e; struct rules *rs; } _case;
        struct { t_exp *c, *t, *f; } _if;
    };
};

struct rules { t_exp *lhs, *rhs; struct rules *next; };

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

int ninterns;
string *interns[65536];

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

int global_uid;

string *tuplefs[NARG];
string *any_id;

t_sym *all_types;
t_sym *all_cons;

t_type *unittype;
t_type *booltype;
t_type *inttype;
t_type *chartype;
t_type *stringtype;
t_type *listtype;
t_type *reftype;

val unit;
val nil;
val cons;
val falsev;
val truev;


void *print(char *msg, ...);
void *printexp(t_exp *e, bool paren);
void *printval(val x, bool paren);
void *printtype(t_type *t, bool paren);
t_type *nametvs(t_type *t, int *uid);
t_exp *expr(void);
t_type *ty(void);

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

val newdata(string *con, int n, val **xs) {
    struct data *data = malloc(sizeof *data + n * sizeof *xs);
    data->con = con;
    data->n = n;
    if (xs) memcpy(data->xs, xs, n * sizeof *xs);
    return (val){Data, .data=data};
}

val newrec(string **fs, int n, val **xs) {
    struct rec *rec = malloc(sizeof *rec + n * sizeof *xs);
    rec->fs = fs;
    rec->n = n;
    if (xs) memcpy(rec->xs, xs, n * sizeof *xs);
    return (val){Record, .rec=rec};
}

void *printval(val x, bool paren) {
    if (paren)
        switch (x.form) {
        case Int:
        case Char:
        case String:
        case Data:
        case Record:
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

    case Data:
        if (paren && x.data->n) return print("(%v)", x);
        else {
            print("%S", x.data->con);
            for (int i = 0; i < x.data->n; i++)
                print(" %V", x.data->xs[i]);
            return 0;
        }

    case Record:
        if (x.rec->fs == tuplefs) {
            print("(");
            for (int i = 0; i < x.rec->n; i++)
                print(i? ", %v": "%v", x.rec->xs[i]);
            return print(")");
        } else {
            print("{");
            for (int i = 0; i < x.rec->n; i++)
                print(i? "%S=%v": "%S=%v", x.rec->fs[i], x.rec->xs[i]);
            return print("}");
        }

    case Fn:
        return print("#fn");
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
    return new(t_type, Typevar, .id=id);
}

t_type *polyvar(string *id) {
    return new(t_type, Polyvar, .id=id);
}

t_type *rectype(int n, string **fs, t_type **ts) {
    t_type *type = new(t_type, RecType, .n=n, .fs=fs);
    memcpy(type->ts, ts, n * sizeof *ts);
    return type;
}

t_type *fntype(t_type *lhs, t_type *rhs) {
    return new(t_type, FnType, .n=2, .ts={lhs, rhs});
}

t_type *follow(t_type *t) {
    return t->form == Typevar && *t->ts? follow(*t->ts): t;
}

// Name typevars before printing.
t_type *nametvs(t_type *t, int *uid) {
    t = follow(t);

    if (t->form == Typevar && t->id == 0) {
        char buf[32];
        snprintf(buf, sizeof buf, "$%d", *uid++);
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
    case Polyvar: return print("%S", t->id);

    case RecType:
        if (t->fs == tuplefs) {
            print("(");
            for (int i = 0; i < t->n; i++)
                print(i? ", %t": "%t", t->ts[i]);
            return print(")");
        } else {
            print("{");
            for (int i = 0; i < t->n; i++)
                print(i? ", %S::%t": "%S::%t", t->fs[i], t->ts[i]);
            return print("}");
        }

    case FnType:
        if (paren) return print("(%t)", t);

        return print(t->ts[1]->form == FnType? "%T -> %t": "%T -> %T",
            t->ts[0], t->ts[1]);
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

bool opensrc(char *path) {
    tokl = (t_loc) { intern(path, -1), 1, 1 };

    FILE *file = fopen(path, "rb");
    if (!file) return false;

    source[fread(source, 1, sizeof source, file)] = 0;
    fclose(file);

    srcp = linep = source;
    return true;
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

    for (int i = Tlparen; i <= Toper; i++)
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

#define elit(LOC, LIT) new(t_exp, Elit, LOC, .lit=LIT)
#define eid(LOC, ID) new(t_exp, Eid, LOC, .id=ID)
#define erec(LOC, N, FS, ES) new(t_exp, Erec, LOC, .rec={N, FS, ES})
#define eder(LOC, E) new(t_exp, Eder, LOC, .der=E)
#define efn(LOC, ID, PAR, E) new(t_exp, Efn, LOC, .fn={ID, PAR, E})
#define edot(LOC, E, ID) new(t_exp, Edot, LOC, .dot={E, ID})
#define eapp(LOC, F, X) new(t_exp, Eapp, LOC, .app={F, X})
#define ecase(LOC, E, RS) new(t_exp, Ecase, LOC, ._case={E, RS})
#define eif(LOC, C, T, F) new(t_exp, Eif, LOC, ._if={C, T, F})
#define elet(LOC, ID, RHS, E) new(t_exp, Elet, LOC, .let={ID, RHS, E})
#define eletrec(LOC, DS, E) new(t_exp, Eletrec, LOC, .letrec={DS, E})
#define ety(LOC, E, T) new(t_exp, Ety, LOC, .ty={E, T})

t_exp *aexp(bool required);

void *printexp(t_exp *e, bool paren) {

    if (paren) {
        switch (e->form) {
        case Elit:
        case Eid:
        case Erec:
        case Eder:
            break;

        case Efn:
        case Eapp:
        case Edot:
        case Ety:
        case Elet:
        case Eletrec:
        case Ecase:
        case Eif:
            return print("(%e)", e);
        }
    }

    switch (e->form) {

    case Elit: return print("%v", e->lit);

    case Eid: return print("%S", e->id);

    case Erec:
        if (e->rec.fs == tuplefs) {
            print("(");
            for (int i = 0; i < e->rec.n; i++)
                print("%s%e", i? ", ": "", e->rec.es[i]);
            return print(")");
        }
        else {
            print("{");
            for (int i = 0; i < e->rec.n; i++)
                print(i? ", %S = %e": "%S = %e", e->rec.fs[i], e->rec.es[i]);
            return print("}");
        }

    case Eder: return print(e->der->form == Eder? "! %E": "!%E", e->der);

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
        for (struct rules *i = e->letrec.ds; i; i = i->next)
            print(" rec %E = %e", i->lhs, i->rhs);
        return print(" in %e", e->letrec.e);

    case Ecase:
        print("case %E", e->_case.e);
        for (struct rules *i = e->_case.rs; i; i = i->next)
            print(" | %e -> %e", i->lhs, i->rhs);
        return print(" endc");

    case Eif:
        return print("if %e then %e else %e", e->_if.c, e->_if.t, e->_if.f);
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
            if (con->n != 0) error(tokl, "type needs args: %S", toks);
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
            return rectype(n, tuplefs, ts);
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

                fs[n] = need(Tid);
                ts[n++] = (need(Tty), ty());
            } while (want(Tcomma));
            need(Trcurly);

            memcpy(ofs, fs, n * sizeof *fs);
            qsort(ofs, n, sizeof *ofs, cmpstr);

            new_order(n, fs, ofs, order);
            for (int i = 0; i < n; i++)
                ots[order[i]] = ts[i];

            return rectype(n, copy(n, fs), ts);
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

    if (peek(Tid) || peek(Tand) || peek(Tor))
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
    return eapp(loc, eapp(loc, elit(loc, cons), hd), tl);
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
            e = sym? elit(loc, sym->val): eid(loc, toks);
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
                erec(loc, n, tuplefs, copy(n, es));
            break;
        }

    case Tlbrace: return listexp();

    case Tlcurly:
        {
            string *fs[NARG];
            t_exp *es[NARG];
            int n = 0;

            do {
                if (peek(Trcurly)) break;

                if (n == NARG) error(loc, "overflown fields");

                string *id = need(Tid);

                fs[n] = id;

                if (want(Tequal))
                    es[n] = expr();
                else {
                    t_sym *sym = find(id, all_cons);
                    es[n] = sym? elit(loc, sym->val): eid(loc, id);
                }

                n++;
            } while (want(Tcomma));
            need(Trcurly);

            e = erec(loc, n, copy(n, fs), copy(n, es));
            break;
        }

    case Tder:
        e = eder(loc, aexp(true));
        break;

    case Tfn:
        {
            t_exp *params[NARG];
            int n = 0;

            while ((e = aexp(false)))
                if (n == NARG) error(e->loc, "overflown args");
                else params[n++] = e;

            need(Tarrow);
            e = expr();

            for (int i = n - 1; i >= 0; i--)
                e = efn(loc, NULL, params[i], e);

            break;
        }

    default:
        peeked = true;
        if (required) error(tokl, "need expression");
        return 0;
    }

    while (want(Tdot)) {
        loc = tokl;
        e = edot(loc, e, need(Tid));
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
        t_exp *f = sym? elit(loc, sym->val): eid(loc, id);
        t_exp *rhs = iexp(op->rhs);

        if (tok == Tand) e = eif(loc, e, rhs, elit(loc, falsev));
        else if (tok == Tor) e = eif(loc, e, elit(loc, truev), rhs);
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

t_exp *let(t_tok cont) {
    t_loc loc = tokl;
    t_exp *e;

    if (peek(Trec)) {
        struct rules *rs = 0;
        struct rules **p = &rs;
        while (want(Trec)) {
            t_exp *lhs = expr();
            t_exp *rhs = (need(Tequal), expr());
            *p = new(struct rules, lhs, rhs, 0);
            p = &(*p)->next;
        }
        e = eletrec(loc, rs, 0);
    }
    else {
        t_exp *lhs = expr();
        t_exp *rhs = (need(Tequal), expr());
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
            struct rules *rs = 0;
            struct rules **p = &rs;

            while (want(Tbar)) {
                t_exp *lhs = expr();
                t_exp *rhs = (need(Tarrow), expr());
                *p = new(struct rules, lhs, rhs, 0);
                p = &(*p)->next;
            }
            e = ecase(loc, sub, rs);
            break;
        }

    case Tlet:
        {
            e = let(Tlet);
            set_let_body(&e, (need(Tin), expr()));
            break;
        }

    default:
        peeked = true;
        e = iexp(0);
    }

    while (true) {
        loc = tokl;

        if (want(Twhere)) {
            t_exp *decs = let(Talso);
            set_let_body(&decs, e);
            want(Tendw);
            e = decs;
        }

        else if (want(Tty)) e = ety(loc, e, ty());

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
            else ts[n++] = typevar(toks);
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
            t_type *ts[NARG];
            t_type *t;
            int n = 0;

            if (find(id, all_cons)) error(tokl, "redefining con: %S", id);

            while ((t = aty(false)))
                if (n == NARG) error(tokl, "overflown con params");
                else ts[n++] = t;

            t = dt;
            for (int i = n - 1; i >= 0; i--)
                t = fntype(ts[i], t);

            define_val(&all_cons, id, t, newdata(id, 0, 0));

        } while (want(Tbar));

        for (int i = 0; i < n; i++) all_types = all_types->next;
    }

}

void top(t_exp **e) {
    while (!peek(Teof))
        if (want(Tinfixl)) infix_dec(true);
        else if (want(Tinfixr)) infix_dec(false);
        else if (want(Tdatatype)) datatype_dec();
        else if (want(Tlet)) set_let_body(e, let(Tlet));
        else error(tokl, "need top-level declaration");
}

int main(int argc, char **argv) {
    (void) argc;

    for (int i = 0; i < NARG; i++) {
        char buf[8];
        snprintf(buf, sizeof buf, "%d", i);
        tuplefs[i] = intern(buf, -1);
    }

    for (int i = Tder; i <= Tbar; i++)
        tokn[i] = intern(tokn[i], -1)->txt;

    any_id = intern("_", -1);

    t_type *a = polyvar(0);
    unittype = rectype(0, tuplefs, 0);;
    booltype = typecon(intern("bool", -1), 0, 0);
    inttype = typecon(intern("int", -1), 0, 0);
    chartype = typecon(intern("char", -1), 0, 0);
    stringtype = typecon(intern("string", -1), 0, 0);
    listtype = typecon(intern("list", -1), 1, &a);
    reftype = typecon(intern("ref", -1), 1, &a);

    define(&all_types, intern("unit", -1), unittype);
    define(&all_types, booltype->id, booltype);
    define(&all_types, inttype->id, inttype);
    define(&all_types, chartype->id, chartype);
    define(&all_types, stringtype->id, stringtype);
    define(&all_types, listtype->id, listtype);
    define(&all_types, reftype->id, reftype);

    unit = newrec(tuplefs, 0, 0);
    nil = newdata(intern("nil", -1), 0, 0);
    cons = newdata(intern(":", -1), 0, 0);
    falsev = newdata(intern("false", -1), 0, 0);
    truev = newdata(intern("true", -1), 0, 0);

    define_val(&all_cons, nil.data->con, listtype, nil);
    define_val(&all_cons, cons.data->con, listtype, cons);
    define_val(&all_cons, falsev.data->con, booltype, falsev);
    define_val(&all_cons, truev.data->con, booltype, truev);

    forced_op = new(t_op, intern("`", -1), 10, 11, 0);
    ops = new(t_op, intern("and", -1), 3, 3, ops);
    ops = new(t_op, intern("or", -1), 2, 2, ops);
    ops = new(t_op, intern("@", -1), 0, 1, ops);

    if (!opensrc(argv[1])) error(tokl, "cannot open source");

    t_exp *e = 0;
    top(&e);
    set_let_body(&e, erec(tokl, 0, tuplefs, 0));
    print("> %e\n", e);

    puts("done.");
}
