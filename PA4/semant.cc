#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <symtab.h>
#include <map>
#include <set>
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char* curr_filename;

Symbol class__class::get_name()
{
    return name;
}
Symbol class__class::get_parent()
{
    return parent;
}

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

ClassTable* classtable;
SymbolTable<char*, Entry>* symboltable;
SymbolTable<char*, Class__class>* ctable;

Class_ cur_class;

bool Expression_class::validate_arithmetic_expr(Expression e1, Expression e2 = NULL)
{
    e1->analyze();
    if (e2) {
        e2->analyze();
    }
    if (e1->get_type() == Int && (e2 == NULL || e2->get_type() == Int)) {
        type = Int;
        return true;
    }
    else {
        type = Object;
        return false;
    }
}

bool Expression_class::validate_comparison_expr(Expression e1, Expression e2 = NULL)
{
    e1->analyze();
    if (e2) {
        e2->analyze();

        if (e1->get_type() == Int && e2->get_type() == Int) {
            type = Bool;
            return true;
        }
        else {
            type = Object;
            return false;
        }
    }
    else {
        if (e1->get_type() == Bool) {
            type = Bool;
            return true;
        }
        else {
            type = Object;
            return false;
        }
    }
}

ClassTable::ClassTable(Classes classes)
    : semant_errors(0)
    , error_stream(cerr)
{

    /* Fill this in */
    this->classes = classes->copy_list();
}
void ClassTable::install_basic_classes()
{
    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class = class_(Object,
        No_class,
        append_Features(
                                     append_Features(
                                         single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                         single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                                     single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
        filename);

    ctable->addid(Object->get_string(), Object_class);

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = class_(IO,
        Object,
        append_Features(
                                 append_Features(
                                     append_Features(
                                         single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                             SELF_TYPE, no_expr())),
                                         single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                             SELF_TYPE, no_expr()))),
                                     single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                                 single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
        filename);

    ctable->addid(IO->get_string(), IO_class);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class = class_(Int,
        Object,
        single_Features(attr(val, prim_slot, no_expr())),
        filename);

    ctable->addid(Int->get_string(), Int_class);
    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class = class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //

    ctable->addid(Bool->get_string(), Bool_class);

    Class_ Str_class = class_(Str,
        Object,
        append_Features(
                                  append_Features(
                                      append_Features(
                                          append_Features(
                                              single_Features(attr(val, Int, no_expr())),
                                              single_Features(attr(str_field, prim_slot, no_expr()))),
                                          single_Features(method(length, nil_Formals(), Int, no_expr()))),
                                      single_Features(method(concat,
                                          single_Formals(formal(arg, Str)),
                                          Str,
                                          no_expr()))),
                                  single_Features(method(substr,
                                      append_Formals(single_Formals(formal(arg, Int)),
                                                             single_Formals(formal(arg2, Int))),
                                      Str,
                                      no_expr()))),
        filename);

    ctable->addid(Str->get_string(), Str_class);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(), c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node* t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}

/*   This is the entry point to the semantic checker.

	 Your checker should do the following two things:

	 1) Check that the program is semantically correct
	 2) Decorate the abstract syntax tree with type information
	 by setting the `type' field in each Expression node.
	 (see `tree.h')

	 You are free to first do 1), make sure you catch all semantic
	 errors. Part 2) can be done in a second stage, when you want
	 to build mycoolc.
	 */

class InheritanceGraph {
private:
    std::map<Symbol, Symbol> graph;

public:
    void addEdge(const Symbol&, const Symbol&); // a inherits b
    int validate();
    int conform(const Symbol&, const Symbol&); // a conforms to b
    Symbol lca(Symbol, Symbol); // lca of a and b
} * g;

void InheritanceGraph::addEdge(const Symbol& a, const Symbol& b)
{
    if (graph.count(a) != 0) {
        //throw 1;
        return;
    }
    graph[a] = b;
}
int InheritanceGraph::validate()
{
    for (std::map<Symbol, Symbol>::iterator it = graph.begin(); it != graph.end(); it++) {
        Symbol cur = it->first;
        Symbol next = it->second;
        std::set<Symbol> visited;
        visited.insert(cur);
        while (next != Object) {
            if (visited.count(next) != 0) {
                return 1;
            }
            visited.insert(next);
            cur = next;
            next = graph[next];
        }
    }
    return 0;
}

int InheritanceGraph::conform(const Symbol& a, const Symbol& b)
{
    if (a == b)
        return 1;
    Symbol cur = a;
    if (cur == SELF_TYPE)
        cur = cur_class->get_name();
    while (cur != Object) {
        if (cur == b) {
            return 1;
        }
        cur = graph[cur];
    }
    return cur == b;
}
Symbol InheritanceGraph::lca(Symbol a, Symbol b)
{
    if (a == b)
        return a;

    if (a == SELF_TYPE)
        a = cur_class->get_name();

    if (b == SELF_TYPE)
        b = cur_class->get_name();

    int ha = 0, hb = 0;
    Symbol cura = a, curb = b;

    while (cura != Object) {
        cura = graph[cura];
        ha++;
    }
    while (curb != Object) {
        curb = graph[curb];
        hb++;
    }

    cura = a;
    curb = b;
    if (ha >= hb) {
        for (int i = ha - hb; i > 0; i--) {
            cura = graph[cura];
        }
    }
    else {
        for (int i = hb - ha; i > 0; i--) {
            curb = graph[curb];
        }
    }

    while (cura != curb) {
        cura = graph[cura];
        curb = graph[curb];
    }
    return cura;
}
int program_class::check_inheritance_graph()
{
    g = new InheritanceGraph();
    try {
        g->addEdge(IO, Object);
        g->addEdge(Int, Object);
        g->addEdge(Bool, Object);
        g->addEdge(Str, Object);
        for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
            cur_class = classes->nth(i);
            Symbol csym = cur_class->get_name();
            Symbol psym = cur_class->get_parent();
            g->addEdge(csym, psym);
        }
    }
    catch (const int& e) {
        return e;
    }
    return g->validate();
}
void program_class::preprocessing()
{
    /* check inheritance graph */

    if (check_inheritance_graph()) {
        throw "Compilation halted due to violation of class inheritance graph.";
    }

    bool exist_main_class = false;

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        cur_class = classes->nth(i);
        char* class_name = cur_class->get_name()->get_string();

        if (strcmp(class_name, "Main") == 0) {
            exist_main_class = true;
        }

        if (strcmp(class_name, "SELF_TYPE") == 0) {
            throw "SELF_TYPE redeclared(used a class name).";
        }

        if (ctable->lookup(class_name) != NULL) {
            throw "Invalid redefinition.";
        }

        if (strcmp(class_name, "Object") == 0 || strcmp(class_name, "Bool") == 0 || strcmp(class_name, "Int") == 0 || strcmp(class_name, "String") == 0 || strcmp(class_name, "IO") == 0) {
            throw "Invalid redefinition.";
        }

        char* parent_class_name = cur_class->get_parent()->get_string();
        if (strcmp(parent_class_name, "Bool") == 0 || strcmp(parent_class_name, "Int") == 0 || strcmp(parent_class_name, "String") == 0 || strcmp(parent_class_name, "SELF_TYPE") == 0) {
            throw "Invalid inheritance.";
        }

        ctable->addid(cur_class->get_name()->get_string(), cur_class);
    }

    if (exist_main_class == false) {
        classtable->semant_error() << "Class Main is not defined." << endl;
    }
}
void program_class::analyze()
{

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        cur_class = classes->nth(i);
        if (ctable->lookup(cur_class->get_parent()->get_string()) == NULL) {
            throw "Inheritance from an undefined class.";
        }

        symboltable->enterscope();
        cur_class->analyze();
        symboltable->exitscope();
    }
}

void class__class::analyze()
{
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature f = features->nth(i);
        f->analyze();
    }
}

Feature class__class::get_attr_node(char* feature_name)
{
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature f = features->nth(i);
        if (f->get_formals() == NULL && strcmp(f->get_name()->get_string(), feature_name) == 0) {
            return f;
        }
    }
    return NULL;
}

Feature class__class::get_method_node(char* feature_name)
{
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature f = features->nth(i);
        if (f->get_formals() != NULL && strcmp(f->get_name()->get_string(), feature_name) == 0) {
            return f;
        }
    }
    return NULL;
}

void formal_class::analyze()
{
    if (symboltable->probe(name->get_string()) != NULL) {
        throw "Duplicate name in formal param list";
    }
    if (name == self) {
        throw "Self as name of a formal parameter (illegal)";
    }
    if (type_decl == SELF_TYPE) {
        throw "SELF_TYPE given as a parameter type (illegal)";
    }
    symboltable->addid(name->get_string(), type_decl);
}

Symbol formal_class::get_formal_type()
{
    return type_decl;
}

Formals method_class::get_formals()
{
    return formals;
}
Symbol method_class::get_type()
{
    return return_type;
}

void branch_class::analyze()
{
    symboltable->addid(name->get_string(), type_decl);
    expr->analyze();
    Symbol expr_type = expr->get_type();
}

Symbol branch_class::get_expr_type()
{
    return expr->get_type();
}

Symbol branch_class::get_decl_type()
{
    return type_decl;
}

void attr_class::analyze()
{
    if (type_decl == SELF_TYPE) {
        type_decl = cur_class->get_name();
    }

    if (strcmp(name->get_string(), "self") == 0) {
        throw "\'self\' cannot be the name of an attribute.";
    }

    Feature feature = NULL;
    Class_ target_class = ctable->lookup(cur_class->get_parent()->get_string());

    while (true) {
        feature = target_class->get_attr_node(name->get_string());
        if (feature != NULL) {
            throw "Cannot override parent's attribute";
            break;
        }
        Symbol parent = target_class->get_parent();
        if (parent == No_class)
            break;
        target_class = ctable->lookup(parent->get_string());
    }

    init->analyze();

    Symbol init_type = init->get_type();
    if (init_type != No_type && g->conform(init_type, type_decl) == false) {
        throw "type error in attr_class";
    }
    symboltable->addid(name->get_string(), type_decl);
}

void method_class::analyze()
{
    symboltable->enterscope();

    if (ctable->lookup(return_type->get_string()) == NULL && return_type != SELF_TYPE) {
        throw "Undefined return type";
    }

    Feature feature = NULL;
    Class_ target_class = ctable->lookup(cur_class->get_parent()->get_string());

    while (true) {
        feature = target_class->get_method_node(name->get_string());
        if (feature != NULL) {
            break;
        }
        Symbol parent = target_class->get_parent();
        if (parent == No_class)
            break;
        target_class = ctable->lookup(parent->get_string());
    }
    if (feature != NULL) {
        Formals parent_formals = feature->get_formals();
        if (parent_formals->len() != formals->len()) {
            throw "Invalid overriding!";
        }
        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
            Formal f = formals->nth(i);
            f->analyze();
            Formal parent_f = parent_formals->nth(i);
            if (f->get_formal_type() != parent_f->get_formal_type()) {
                throw "Invalid overriding!";
            }
        }
        if (feature->get_type() != return_type) {
            throw "Invalid overriding!";
        }
    }
    else {
        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
            Formal f = formals->nth(i);
            f->analyze();
        }
    }

    expr->analyze();

    if (g->conform(expr->get_type(), return_type) == false) {
        throw "expr type cannot conform to return type.";
    }

    symboltable->exitscope();
}

void assign_class::analyze()
{
    expr->analyze();
    Symbol exprtype = expr->get_type();
    Symbol mytype = symboltable->lookup(name->get_string());

    if (mytype == NULL) {
        Class_ now_c = cur_class;
        while (true) {

            Feature attr = now_c->get_attr_node(name->get_string());

            if (attr != NULL) {
                mytype = attr->get_type();
                break;
            }

            Symbol parent = now_c->get_parent();
            if (parent == No_class)
                break;
            now_c = ctable->lookup(parent->get_string());
        }
    }
    if (mytype == NULL) {
        throw "type error in object_class";
    }

    if (g->conform(exprtype, mytype) == false) {
        throw "type error in assign_class";
    }
    type = exprtype;
}

void block_class::analyze()
{
    Expression last = NULL;
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        Expression e = body->nth(i);
        e->analyze();
        last = e;
    }
    if (last != NULL) {
        type = last->get_type();
    }
}

void bool_const_class::analyze()
{
    type = Bool;
}

void comp_class::analyze()
{
    if (!validate_comparison_expr(e1)) {
        throw "type error in comp_class";
    }
}

void cond_class::analyze()
{
    pred->analyze();

    if (pred->get_type() != Bool) {
        throw "type error in cond_class";
    }
    then_exp->analyze();
    else_exp->analyze();

    Symbol join_type = g->lca(then_exp->get_type(), else_exp->get_type());
    type = join_type;
}

void dispatch_class::analyze()
{
    expr->analyze();

    Symbol expr_type = expr->get_type();

    if (expr_type == SELF_TYPE)
        expr_type = cur_class->get_name();

    Feature feature = NULL;
    Class_ target_class = ctable->lookup(expr_type->get_string());
    while (true) {
        feature = target_class->get_method_node(name->get_string());
        if (feature != NULL) {
            break;
        }
        Symbol parent = target_class->get_parent();
        if (parent == No_class)
            break;
        target_class = ctable->lookup(parent->get_string());
    }

    if (feature == NULL) {
        throw "type error in dispatch_class";
    }

    Formals formals_type = feature->get_formals();
    Symbol fun_type = feature->get_type();

    if (fun_type == SELF_TYPE)
        fun_type = expr->get_type();

    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Expression act = actual->nth(i);
        act->analyze();
        Symbol nth_formal_type = formals_type->nth(i)->get_formal_type();
        if (g->conform(act->get_type(), nth_formal_type) == false) {
            throw "type error in dispatch_class";
        }
    }

    type = fun_type;
}

void divide_class::analyze()
{
    if (!validate_arithmetic_expr(e1, e2)) {
        throw "type error in divide_class";
    }
}

void eq_class::analyze()
{
    e1->analyze();
    e2->analyze();

    if (e1->get_type() == Int) {
        if (e2->get_type() == Int)
            type = Bool;
        else {
            type = Object;
            throw "type error in eq_class";
        }
    }
    if (e1->get_type() == Str) {
        if (e2->get_type() == Str)
            type = Bool;
        else {
            type = Object;
            throw "type error in eq_class";
        }
    }
    if (e1->get_type() == Bool) {
        if (e2->get_type() == Bool)
            type = Bool;
        else {
            type = Object;
            throw "type error in eq_class";
        }
    }
    type = Bool;
}

void int_const_class::analyze()
{
    type = Int;
}

void isvoid_class::analyze()
{
    e1->analyze();
    type = Bool;
}

void leq_class::analyze()
{
    if (!validate_comparison_expr(e1, e2)) {
        throw "type error in leq_class";
    }
}

void let_class::analyze()
{
    if (identifier == self) {
        throw "type error in let_class";
    }
    init->analyze();
    symboltable->enterscope();

    Symbol init_type = init->get_type();

    if (init_type != No_type && g->conform(init_type, type_decl) == false) {
        throw "type error in let_class";
    }
    symboltable->addid(identifier->get_string(), type_decl);
    body->analyze();
    type = body->get_type();
    symboltable->exitscope();
}

void loop_class::analyze()
{
    pred->analyze();
    if (pred->get_type() != Bool) {
        throw "type error in lt_class";
    }
    body->analyze();
    type = Object;
}

void lt_class::analyze()
{
    if (!validate_comparison_expr(e1, e2)) {
        throw "type error in lt_class";
    }
}

void mul_class::analyze()
{
    if (!validate_arithmetic_expr(e1, e2)) {
        throw "type error in mul_class";
    }
}

void neg_class::analyze()
{
    if (!validate_arithmetic_expr(e1)) {
        throw "type error in neg_class";
    }
}

void new__class::analyze()
{
    Symbol new_type;

    if (type_name == SELF_TYPE) {
        new_type = cur_class->get_name();
    }
    else {
        new_type = type_name;
    }

    type = type_name;
}

void no_expr_class::analyze()
{
    type = No_type;
}

void object_class::analyze()
{
    if (name == self) {
        type = SELF_TYPE;
        return;
    }
    // 1. find object in symboltable
    // 2. find object in attr_node till Object

    Symbol mytype = symboltable->lookup(name->get_string());

    if (mytype == NULL) {
        Class_ now_c = cur_class;
        while (true) {

            Feature attr = now_c->get_attr_node(name->get_string());

            if (attr != NULL) {
                mytype = attr->get_type();
                break;
            }

            Symbol parent = now_c->get_parent();
            if (parent == No_class)
                break;
            now_c = ctable->lookup(parent->get_string());
        }
    }
    if (mytype == NULL) {
        throw "type error in object_class";
    }
    type = mytype;
}

void plus_class::analyze()
{
    if (!validate_arithmetic_expr(e1, e2)) {
        throw "type error in plus_class";
    }
}

void static_dispatch_class::analyze()
{
    expr->analyze();

    Symbol expr_type = expr->get_type();

    if (g->conform(expr_type, type_name) == false) {
        throw "type error in static_dispatch_class";
    }

    Feature feature = NULL;
    Class_ target_class = ctable->lookup(type_name->get_string());
    while (true) {
        feature = target_class->get_method_node(name->get_string());
        if (feature != NULL) {
            break;
        }
        Symbol parent = target_class->get_parent();
        if (parent == No_class)
            break;
        target_class = ctable->lookup(parent->get_string());
    }

    if (feature == NULL) {
        throw "type error in dispatch_class";
    }

    Formals formals_type = feature->get_formals();
    Symbol fun_type = feature->get_type();

    if (fun_type == SELF_TYPE)
        fun_type = expr->get_type();

    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Expression act = actual->nth(i);
        act->analyze();
        Symbol nth_formal_type = formals_type->nth(i)->get_formal_type();

        if (g->conform(act->get_type(), nth_formal_type) == false) {
            throw "type error in dispatch_class";
        }
    }

    type = fun_type;
}

void string_const_class::analyze()
{
    type = Str;
}

void sub_class::analyze()
{
    if (!validate_arithmetic_expr(e1, e2)) {
        throw "type error in sub_class";
    }
}

void typcase_class::analyze()
{
    expr->analyze();
    Symbol expr_type = expr->get_type();
    Symbol join_type = NULL;

    SymbolTable<char*, Entry>* casetable = new SymbolTable<char*, Entry>();
    casetable->enterscope();

    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        symboltable->enterscope();
        Case c = cases->nth(i);

        if (casetable->lookup(c->get_decl_type()->get_string()) != NULL) {
            throw "Duplicate branch Int in case statement.";
        }
        casetable->addid(c->get_decl_type()->get_string(), c->get_decl_type());

        c->analyze();

        if (g->conform(c->get_expr_type(), c->get_decl_type()) == false) {
            throw "type error in typcase_class";
        }
        if (join_type == NULL)
            join_type = c->get_expr_type();
        else
            join_type = g->lca(join_type, c->get_expr_type());

        symboltable->exitscope();
    }
    type = join_type;
}

void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */
    ctable = new SymbolTable<char*, Class__class>();
    ctable->enterscope();

    classtable->install_basic_classes();

    symboltable = new SymbolTable<char*, Entry>();

    try {
        preprocessing();
        analyze();
    }
    catch (const char* msg) {
        classtable->semant_error(cur_class) << msg << endl;
    }
    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}