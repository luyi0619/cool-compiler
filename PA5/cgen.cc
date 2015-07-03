
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

#include <vector>
#include <map>
#include <list>
#include <algorithm>

extern void emit_string_constant(ostream& str, char* s);
extern int cgen_debug;
Symbol cur_filename;

int label = 0;
int variable_offset;
int cur_offset()
{
	return variable_offset;
}
void set_offset(int of)
{
	variable_offset = of;
}
int next_offset()
{
	return variable_offset--;
}
void clear_variable_offset()
{
	variable_offset = 0;
}

int get_nextbranch()
{
	return label++;
}

Symbol selfclass = 0;

std::map<Symbol, int> classtag;
std::map<Symbol, int> objectsize;

std::map<Symbol, std::map<Symbol, std::pair<Symbol, int> > > class_attr_typeoffset;
std::map<Symbol, std::map<Symbol, int> > class_method_offset;

SymbolTable<char*, int>* identifiers;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
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

static char* gc_init_names[] = { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char* gc_collect_names[] = { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// add definition of methods defined in cool-tree.h

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream& os)
{
	// spim wants comments to start with '#'
	os << "# start of generated code\n";

	initialize_constants();
	CgenClassTable* codegen_classtable = new CgenClassTable(classes, os);

	os << "\n# end of generated code\n";
}

//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char* dest_reg, int offset, char* source_reg, ostream& s)
{
	s << LW << dest_reg << " " << offset* WORD_SIZE << "(" << source_reg << ")"
		<< endl;
}

static void emit_store(char* source_reg, int offset, char* dest_reg, ostream& s)
{
	s << SW << source_reg << " " << offset* WORD_SIZE << "(" << dest_reg << ")"
		<< endl;
}

static void emit_load_imm(char* dest_reg, int val, ostream& s)
{
	s << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(char* dest_reg, char* address, ostream& s)
{
	s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char* dest_reg, ostream& s)
{
	s << LA << dest_reg << " ";
}

static void emit_load_bool(char* dest, const BoolConst& b, ostream& s)
{
	emit_partial_load_address(dest, s);
	b.code_ref(s);
	s << endl;
}

static void emit_load_string(char* dest, StringEntry* str, ostream& s)
{
	emit_partial_load_address(dest, s);
	str->code_ref(s);
	s << endl;
}

static void emit_load_int(char* dest, IntEntry* i, ostream& s)
{
	emit_partial_load_address(dest, s);
	i->code_ref(s);
	s << endl;
}

static void emit_move(char* dest_reg, char* source_reg, ostream& s)
{
	s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char* dest, char* src1, ostream& s)
{
	s << NEG << dest << " " << src1 << endl;
}

static void emit_add(char* dest, char* src1, char* src2, ostream& s)
{
	s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char* dest, char* src1, char* src2, ostream& s)
{
	s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char* dest, char* src1, int imm, ostream& s)
{
	s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char* dest, char* src1, char* src2, ostream& s)
{
	s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char* dest, char* src1, char* src2, ostream& s)
{
	s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char* dest, char* src1, char* src2, ostream& s)
{
	s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char* dest, char* src1, int num, ostream& s)
{
	s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char* dest, ostream& s)
{
	s << JALR << "\t" << dest << endl;
}

static void emit_jal(char* address, ostream& s)
{
	s << JAL << address << endl;
}

static void emit_return(ostream& s)
{
	s << RET << endl;
}

static void emit_gc_assign(ostream& s)
{
	s << JAL << "_GenGC_Assign" << endl;
}

static void emit_disptable_ref(Symbol sym, ostream& s)
{
	s << sym << DISPTAB_SUFFIX;
}

static void emit_init_ref(Symbol sym, ostream& s)
{
	s << sym << CLASSINIT_SUFFIX;
}

static void emit_label_ref(int l, ostream& s)
{
	s << "label" << l;
}

static void emit_protobj_ref(Symbol sym, ostream& s)
{
	s << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{
	s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream& s)
{
	emit_label_ref(l, s);
	s << ":" << endl;
}

static void emit_beqz(char* source, int label, ostream& s)
{
	s << BEQZ << source << " ";
	emit_label_ref(label, s);
	s << endl;
}

static void emit_beq(char* src1, char* src2, int label, ostream& s)
{
	s << BEQ << src1 << " " << src2 << " ";
	emit_label_ref(label, s);
	s << endl;
}

static void emit_bne(char* src1, char* src2, int label, ostream& s)
{
	s << BNE << src1 << " " << src2 << " ";
	emit_label_ref(label, s);
	s << endl;
}

static void emit_bleq(char* src1, char* src2, int label, ostream& s)
{
	s << BLEQ << src1 << " " << src2 << " ";
	emit_label_ref(label, s);
	s << endl;
}

static void emit_blt(char* src1, char* src2, int label, ostream& s)
{
	s << BLT << src1 << " " << src2 << " ";
	emit_label_ref(label, s);
	s << endl;
}

static void emit_blti(char* src1, int imm, int label, ostream& s)
{
	s << BLT << src1 << " " << imm << " ";
	emit_label_ref(label, s);
	s << endl;
}

static void emit_bgti(char* src1, int imm, int label, ostream& s)
{
	s << BGT << src1 << " " << imm << " ";
	emit_label_ref(label, s);
	s << endl;
}

static void emit_branch(int l, ostream& s)
{
	s << BRANCH;
	emit_label_ref(l, s);
	s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char* reg, ostream& str)
{
	emit_store(reg, 0, SP, str);
	emit_addiu(SP, SP, -4, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char* dest, char* source, ostream& s)
{
	emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char* source, char* dest, ostream& s)
{
	emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}

static void emit_test_collector(ostream& s)
{
	emit_push(ACC, s);
	emit_move(ACC, SP, s); // stack end
	emit_move(A1, ZERO, s); // allocate nothing
	s << JAL << gc_collect_names[cgen_Memmgr] << endl;
	emit_addiu(SP, SP, 4, s);
	emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char* source, ostream& s)
{
	if (source != (char*)A1)
		emit_move(A1, source, s);
	s << JAL << "_gc_check" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
	s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
	IntEntryP lensym = inttable.add_int(len);

	// Add -1 eye catcher
	s << WORD << "-1" << endl;

	code_ref(s);
	s << LABEL // label
		<< WORD << stringclasstag << endl // tag
		<< WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl // size
		<< WORD;

	/***** Add dispatch information for class String ******/
	s << "String_dispTab";

	s << endl; // dispatch table
	s << WORD;
	lensym->code_ref(s);
	s << endl; // string length
	emit_string_constant(s, str); // ascii string
	s << ALIGN; // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{
	for (List<StringEntry>* l = tbl; l; l = l->tl())
		l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream& s)
{
	s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream& s, int intclasstag)
{
	// Add -1 eye catcher
	s << WORD << "-1" << endl;

	code_ref(s);
	s << LABEL // label
		<< WORD << intclasstag << endl // class tag
		<< WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
		<< WORD;

	/***** Add dispatch information for class Int ******/
	s << "Int_dispTab";

	s << endl; // dispatch table
	s << WORD << str << endl; // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream& s, int intclasstag)
{
	for (List<IntEntry>* l = tbl; l; l = l->tl())
		l->hd()->code_def(s, intclasstag);
}

//
// Bools
//
BoolConst::BoolConst(int i)
: val(i)
{
	assert(i == 0 || i == 1);
}

void BoolConst::code_ref(ostream& s) const
{
	s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
	// Add -1 eye catcher
	s << WORD << "-1" << endl;

	code_ref(s);
	s << LABEL // label
		<< WORD << boolclasstag << endl // class tag
		<< WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl // object size
		<< WORD;

	/***** Add dispatch information for class Bool ******/
	s << "Bool_dispTab";

	s << endl; // dispatch table
	s << WORD << val << endl; // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
	Symbol main = idtable.lookup_string(MAINNAME);
	Symbol string = idtable.lookup_string(STRINGNAME);
	Symbol integer = idtable.lookup_string(INTNAME);
	Symbol boolc = idtable.lookup_string(BOOLNAME);

	str << "\t.data\n" << ALIGN;
	//
	// The following global names must be defined first.
	//
	str << GLOBAL << CLASSNAMETAB << endl;
	str << GLOBAL;
	emit_protobj_ref(main, str);
	str << endl;
	str << GLOBAL;
	emit_protobj_ref(integer, str);
	str << endl;
	str << GLOBAL;
	emit_protobj_ref(string, str);
	str << endl;
	str << GLOBAL;
	falsebool.code_ref(str);
	str << endl;
	str << GLOBAL;
	truebool.code_ref(str);
	str << endl;
	str << GLOBAL << INTTAG << endl;
	str << GLOBAL << BOOLTAG << endl;
	str << GLOBAL << STRINGTAG << endl;

	//
	// We also need to know the tag of the Int, String, and Bool classes
	// during code generation.
	//
	str << INTTAG << LABEL
		<< WORD << intclasstag << endl;
	str << BOOLTAG << LABEL
		<< WORD << boolclasstag << endl;
	str << STRINGTAG << LABEL
		<< WORD << stringclasstag << endl;
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
	str << GLOBAL << HEAP_START << endl
		<< HEAP_START << LABEL
		<< WORD << 0 << endl
		<< "\t.text" << endl
		<< GLOBAL;
	emit_init_ref(idtable.add_string("Main"), str);
	str << endl
		<< GLOBAL;
	emit_init_ref(idtable.add_string("Int"), str);
	str << endl
		<< GLOBAL;
	emit_init_ref(idtable.add_string("String"), str);
	str << endl
		<< GLOBAL;
	emit_init_ref(idtable.add_string("Bool"), str);
	str << endl
		<< GLOBAL;
	emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
	str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
	falsebool.code_def(str, boolclasstag);
	truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc()
{
	//
	// Generate GC choice constants (pointers to GC functions)
	//
	str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
	str << "_MemMgr_INITIALIZER:" << endl;
	str << WORD << gc_init_names[cgen_Memmgr] << endl;
	str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
	str << "_MemMgr_COLLECTOR:" << endl;
	str << WORD << gc_collect_names[cgen_Memmgr] << endl;
	str << GLOBAL << "_MemMgr_TEST" << endl;
	str << "_MemMgr_TEST:" << endl;
	str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
	//
	// Add constants that are required by the code generator.
	//
	stringtable.add_string("");
	inttable.add_string("0");

	stringtable.code_string_table(str, stringclasstag);
	inttable.code_string_table(str, intclasstag);
	code_bools(boolclasstag);
}

void CgenClassTable::code_prototype_helper(CgenNode* curnode, int& attr_offset)
{
	if (curnode == 0)
		return;
	code_prototype_helper(curnode->get_parentnd(), attr_offset);

	Features features = curnode->features;
	// collect attrs
	for (int i = features->first(); features->more(i); i = features->next(i)) {
		Feature f = features->nth(i);
		if (f->type() == 1) // attr
		{
			attr_class* attr = (attr_class*)f;
			Symbol type = attr->type_decl;
			class_attr_typeoffset[selfclass][attr->get_name()] = std::make_pair(attr->type_decl, attr_offset++);

			if (type == Int) {
				IntEntryP lensym = inttable.add_int(0);
				str << WORD;
				lensym->code_ref(str);
				str << endl;
			}
			else if (type == Bool) {
				str << WORD;
				falsebool.code_ref(str);
				str << endl;
			}
			else if (type == Str) {
				str << WORD;
				stringtable.lookup_string("")->code_ref(str);
				str << endl;
			}
			else {
				str << WORD << 0 << endl;
			}

			//cout << s << " " << attr->get_name() << " " << attr->type_decl << " " << attr_offset << endl;
		}
	}
}
void CgenClassTable::code_prototype()
{
	for (List<CgenNode>* l = nds; l; l = l->tl()) {
		CgenNode* node = l->hd();
		Symbol sym = node->get_name();
		selfclass = sym;
		int tag = classtag[sym];

		// Add -1 eye catcher
		str << WORD << "-1" << endl;

		emit_protobj_ref(sym, str);
		str << LABEL;

		str << WORD << tag << endl;

		assert(objectsize.count(sym));
		int classsize = objectsize[sym];

		str << WORD << (3 + classsize) << endl;

		str << WORD << sym << "_dispTab" << endl;

		// we only care about type and offset
		int attr_offset = 3;
		code_prototype_helper(node, attr_offset);
	}
}

void CgenClassTable::code_nametab()
{
	str << CLASSNAMETAB << LABEL;
	std::vector<std::pair<int, Symbol> > tags;
	for (std::map<Symbol, int>::iterator it = classtag.begin(); it != classtag.end(); it++) {
		tags.push_back(std::make_pair(it->second, it->first));
	}
	std::sort(tags.begin(), tags.end());

	for (size_t i = 0; i < tags.size(); i++) {
		char* classname = tags[i].second->get_string();
		str << WORD;
		stringtable.lookup_string(classname)->code_ref(str);
		str << endl;
	}
}

void CgenClassTable::code_dispatch_helper(CgenNode* curnode, std::list<std::pair<Symbol, Symbol> >& functions)
{

	if (curnode == 0)
		return;
	code_dispatch_helper(curnode->get_parentnd(), functions);
	Features features = curnode->features;
	for (int i = features->first(); features->more(i); i = features->next(i)) {
		Feature f = features->nth(i);
		if (f->type() == 0) // method
		{
			bool found = false;
			for (std::list<std::pair<Symbol, Symbol> >::iterator it = functions.begin(); it != functions.end(); it++) {
				// func name equals
				if (it->second == f->get_name()) {
					found = true;
					it->first = curnode->name;
					break; // there should be only one matched!!
				}
			}
			if (!found) {
				functions.push_back(std::make_pair(curnode->name, f->get_name()));
			}
		}
	}
}

void CgenClassTable::code_dispatch()
{
	for (List<CgenNode>* l = nds; l; l = l->tl()) {
		CgenNode* node = l->hd();
		selfclass = node->get_name();
		// prepare override_set
		// class, func name
		std::list<std::pair<Symbol, Symbol> > functions;
		code_dispatch_helper(node, functions);
		int offset = 0;

		emit_disptable_ref(selfclass, str);
		str << LABEL;
		for (std::list<std::pair<Symbol, Symbol> >::iterator it = functions.begin(); it != functions.end(); it++) {
			str << WORD;
			str << it->first;
			str << ".";
			str << it->second;
			str << endl;
			class_method_offset[selfclass][it->second] = offset++;
			//str << "# " << it->first << " " << (int)it->first << " ~~~ " << it->second << " " << (int)it->second << "  offset: " << class_method_offset[it->first][it->second] << endl;
		}
	}
}
void CgenClassTable::code_objTab()
{
	str << CLASSOBJTAB << LABEL;
	std::vector<std::pair<int, Symbol> > tags;
	for (std::map<Symbol, int>::iterator it = classtag.begin(); it != classtag.end(); it++) {
		tags.push_back(std::make_pair(it->second, it->first));
	}
	std::sort(tags.begin(), tags.end());

	for (size_t i = 0; i < tags.size(); i++) {
		Symbol s = tags[i].second;
		str << WORD;
		emit_protobj_ref(s, str);
		str << endl;

		str << WORD;
		emit_init_ref(s, str);
		str << endl;
	}
}

void CgenClassTable::default_init(CgenNode* curnode)
{
	if (curnode == 0)
		return;
	default_init(curnode->get_parentnd());

	Features features = curnode->features;

	// implement init - defaul - init first
	for (int i = features->first(); features->more(i); i = features->next(i)) {
		Feature f = features->nth(i);
		if (f->type() == 1) // attr
		{
			attr_class* attr = (attr_class*)f;
			if (attr->init->no_expr())
				continue; // skip basic init ???

			////////    default - init

			if (attr->type_decl == Int) {
				IntEntryP lensym = inttable.add_int(0);
				emit_partial_load_address(ACC, str);
				lensym->code_ref(str);
				str << endl;
			}
			else if (attr->type_decl == Bool) {
				emit_partial_load_address(ACC, str);
				falsebool.code_ref(str);
				str << endl;
			}
			else if (attr->type_decl == Str) {
				emit_partial_load_address(ACC, str);
				stringtable.lookup_string("")->code_ref(str);
				str << endl;
			}
			else {
				emit_move(ACC, ZERO, str);
			}
			int offset = class_attr_typeoffset[selfclass][f->get_name()].second;
			emit_store(ACC, offset, SELF, str);
		}
	}
}

void CgenClassTable::code_methods()
{
	for (List<CgenNode>* l = nds; l; l = l->tl()) {
		CgenNode* node = l->hd();
		Symbol s = node->get_name();
		selfclass = s;
		cur_filename = node->filename;
		Features features = node->features;

		/* start class_init */
		emit_init_ref(s, str);
		str << LABEL;

		//store FP and SELF
		emit_addiu(SP, SP, -12, str);
		emit_store(FP, 3, SP, str);
		emit_store(SELF, 2, SP, str);
		emit_store(RA, 1, SP, str);
		emit_move(FP, SP, str);
		emit_move(SELF, ACC, str);

		str << "# ---  defaul - init starts. " << endl;

		default_init(node);

		emit_move(ACC, SELF, str);
		str << "# ---  defaul - init ends. " << endl;

		if (node->get_parent() != No_class) {
			str << JAL << node->get_parent() << "_init" << endl;
		}

		identifiers = new SymbolTable<char*, int>();
		identifiers->enterscope();

		for (int i = features->first(); features->more(i); i = features->next(i)) {
			Feature f = features->nth(i);
			if (f->type() == 1) // attr
			{
				attr_class* attr = (attr_class*)f;
				if (attr->init->no_expr())
					continue; // skip basic init ???

				attr->init->code(str);
				int offset = class_attr_typeoffset[s][f->get_name()].second;
				emit_store(ACC, offset, SELF, str);
			}
		}

		identifiers->exitscope();
		delete identifiers; /* still have memory leak */

		emit_move(ACC, SELF, str);
		emit_load(RA, 1, SP, str);
		emit_load(SELF, 2, SP, str);
		emit_load(FP, 3, SP, str);
		emit_addiu(SP, SP, 12, str);
		emit_return(str);

		/* end class_init */

		if (classtag[s] < 5)
			continue; // built-in class

		// implement methods
		for (int i = features->first(); features->more(i); i = features->next(i)) {
			Feature f = features->nth(i);
			if (f->type() == 0) // method
			{
				identifiers = new SymbolTable<char*, int>();

				identifiers->enterscope();

				emit_method_ref(s, f->get_name(), str);
				str << LABEL;

				method_class* med = (method_class*)f;

				for (int k = med->formals->first(), of = med->formals->len(); med->formals->more(k); k = med->formals->next(k), of--) {
					formal_class* f = (formal_class*)med->formals->nth(k);
					identifiers->addid(f->name->get_string(), new int(of));
				}

				/* cgen(def f(x_1, ..., x_n) = e)*/

				// set new FP
				emit_move(FP, SP, str);
				// set self pointer
				emit_move(SELF, ACC, str);

				//temp variables
				int variable_length = 100; /*add temp variables length*/
				emit_addiu(SP, SP, -variable_length, str);

				//store RA

				emit_store(RA, 0, SP, str);

				emit_addiu(SP, SP, -4, str);

				str << "# ----------------------- med->expr->code(str) follows" << endl;

				med->expr->code(str);
				emit_load(RA, 1, SP, str);
				int z = 4 + med->formals->len() * 4 + variable_length; // return address, parameters, variables
				emit_addiu(SP, SP, z, str);
				emit_load(SELF, 1, SP, str);
				emit_load(FP, 2, SP, str);
				emit_return(str);

				identifiers->exitscope();
				delete identifiers; /* still have memory leak */
			}
		}
	}
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s)
: nds(NULL)
, str(s)
{

	stringclasstag = 4 /* Change to your String class tag here */;
	intclasstag = 2 /* Change to your Int class tag here */;
	boolclasstag = 3 /* Change to your Bool class tag here */;

	enterscope();
	if (cgen_debug)
		cout << "Building CgenClassTable" << endl;
	install_basic_classes();
	install_classes(classes);
	build_inheritance_tree();
	code();
	exitscope();
}

void CgenClassTable::install_basic_classes()
{

	// The tree package uses these globals to annotate the classes built below.
	//curr_lineno  = 0;
	Symbol filename = stringtable.add_string("<basic class>");

	//
	// A few special class names are installed in the lookup table but not
	// the class list.  Thus, these classes exist, but are not part of the
	// inheritance hierarchy.
	// No_class serves as the parent of Object and the other special classes.
	// SELF_TYPE is the self class; it cannot be redefined or inherited.
	// prim_slot is a class known to the code generator.
	//
	addid(No_class,
		new CgenNode(class_(No_class, No_class, nil_Features(), filename),
		Basic, this));
	addid(SELF_TYPE,
		new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
		Basic, this));
	addid(prim_slot,
		new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
		Basic, this));

	//
	// The Object class has no parent class. Its methods are
	//        cool_abort() : Object    aborts the program
	//        type_name() : Str        returns a string representation of class name
	//        copy() : SELF_TYPE       returns a copy of the object
	//
	// There is no need for method bodies in the basic classes---these
	// are already built in to the runtime system.
	//
	install_class(
		new CgenNode(
		class_(Object,
		No_class,
		append_Features(
		append_Features(
		single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
		single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
		single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
		filename),
		Basic, this));

	//
	// The IO class inherits from Object. Its methods are
	//        out_string(Str) : SELF_TYPE          writes a string to the output
	//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
	//        in_string() : Str                    reads a string from the input
	//        in_int() : Int                         "   an int     "  "     "
	//
	install_class(
		new CgenNode(
		class_(IO,
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
		filename),
		Basic, this));

	//
	// The Int class has no methods and only a single attribute, the
	// "val" for the integer.
	//
	install_class(
		new CgenNode(
		class_(Int,
		Object,
		single_Features(attr(val, prim_slot, no_expr())),
		filename),
		Basic, this));

	//
	// Bool also has only the "val" slot.
	//
	install_class(
		new CgenNode(
		class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename),
		Basic, this));

	//
	// The class Str has a number of slots and operations:
	//       val                                  ???
	//       str_field                            the string itself
	//       length() : Int                       length of the string
	//       concat(arg: Str) : Str               string concatenation
	//       substr(arg: Int, arg2: Int): Str     substring
	//
	install_class(
		new CgenNode(
		class_(Str,
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
		filename),
		Basic, this));
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
	Symbol name = nd->get_name();

	if (probe(name)) {
		return;
	}

	// The class name is legal, so add it to the list of classes
	// and the symbol table.

	nds = new List<CgenNode>(nd, nds);
	addid(name, nd);
}

int CgenClassTable::get_object_size(CgenNode* curnode)
{
	if (curnode == 0)
		return 0;
	int size = get_object_size(curnode->get_parentnd());
	Features features = curnode->features;
	for (int i = features->first(); features->more(i); i = features->next(i)) {
		Feature f = features->nth(i);
		if (f->type() == 1) // attr
		{
			size++;
		}
	}
	return size;
}

void CgenClassTable::install_classes(Classes cs)
{
	for (int i = cs->first(); cs->more(i); i = cs->next(i))
		install_class(new CgenNode(cs->nth(i), NotBasic, this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
	for (List<CgenNode>* l = nds; l; l = l->tl())
		set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
	CgenNode* parent_node = probe(nd->get_parent());
	nd->set_parentnd(parent_node);
	parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
	children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
	assert(parentnd == NULL);
	assert(p != NULL);
	parentnd = p;
}

void CgenClassTable::code_preprocessing()
{
	/* initialize class tag */
	classtag[Object] = 0;
	classtag[IO] = 1;
	classtag[Int] = intclasstag;
	classtag[Bool] = boolclasstag;
	classtag[Str] = stringclasstag;

	int tag = 5;
	for (List<CgenNode>* l = nds; l; l = l->tl()) {
		CgenNode* node = l->hd();
		Symbol s = node->get_name();
		if (classtag.count(s))
			continue; // skip basic classes
		classtag[s] = tag++;
	}

	/* initialize object size */

	for (List<CgenNode>* l = nds; l; l = l->tl()) {
		CgenNode* node = l->hd();
		Symbol s = node->get_name();
		if (objectsize.count(s))
			continue; // skip basic classes

		objectsize[s] = get_object_size(node);
	}
}

void CgenClassTable::code()
{

	if (cgen_debug)
		cout << "coding global data" << endl;
	code_global_data();

	if (cgen_debug)
		cout << "choosing gc" << endl;
	code_select_gc();

	if (cgen_debug)
		cout << "coding constants" << endl;
	code_constants();

	code_preprocessing();
	code_nametab();
	code_objTab();
	code_dispatch();
	code_prototype();
	//                 Add your code to emit
	//                   - prototype objects
	//                   - class_nameTab
	//                   - dispatch tables
	//

	if (cgen_debug)
		cout << "coding global text" << endl;
	code_global_text();

	code_methods();
	//                 Add your code to emit
	//                   - object initializer
	//                   - the class methods
	//                   - etc...
}

CgenNodeP CgenClassTable::root()
{
	return probe(Object);
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct)
: class__class((const class__class&)*nd)
, parentnd(NULL)
, children(NULL)
, basic_status(bstatus)
{
	stringtable.add_string(name->get_string()); // Add class name to string table
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream& s)
{
	/* attrs or variables */
	s << "# ----> in assign_class " << endl;

	int* p = identifiers->lookup(name->get_string());

	if (p == NULL) // attrs
	{
		/*assume SELF has the object address, ACC contains the result */
		int of = class_attr_typeoffset[selfclass][name].second;
		emit_store(SELF, 0, SP, s);
		emit_addiu(SP, SP, -4, s);
		expr->code(s);
		/*
		emit_load(T1, 1, SP, s);
		emit_store(ACC, of, T1, s);
		emit_addiu(SP, SP, 4, s);
		*/

		emit_load(SELF, 1, SP, s);
		emit_store(ACC, of, SELF, s);
		emit_addiu(SP, SP, 4, s);
	}
	else {
		int of = *p;
		expr->code(s);
		s << "# -------- ofset " << of << endl;
		emit_store(ACC, of, FP, s);
	}
	s << "# ----> out assign_class " << endl;
}

void static_dispatch_class::code(ostream& s)
{

	s << "# ----> in static_dispatch_class " << endl;
	int dispatch_branch = get_nextbranch();

	//store FP and SELF
	emit_store(FP, 0, SP, s);
	emit_addiu(SP, SP, -4, s);
	emit_store(SELF, 0, SP, s);
	emit_addiu(SP, SP, -4, s);

	//parameters
	emit_addiu(SP, SP, -(4 * actual->len()), s);

	for (int i = actual->first(), of = actual->len(); actual->more(i); i = actual->next(i), of--) {
		Expression e = actual->nth(i);
		e->code(s);
		s << "# output parameter for " << name << endl;
		emit_store(ACC, of, SP, s);
	}

	int old_offset = cur_offset();

	clear_variable_offset(); /*old fp + self + # of actuals*/

	expr->code(s);

	// if acc is void, call _dispatch_abort
	emit_bne(ACC, ZERO, dispatch_branch, s);

	// pass line # to t1 and filename to a0
	emit_load_string(ACC, stringtable.add_string(cur_filename->get_string()), s);
	//emit_move(ACC, SELF, s);
	emit_load_imm(T1, get_line_number(), s);
	emit_jal("_dispatch_abort", s);

	int offset = class_method_offset[type_name][name];

	//output dispatch_branch
	emit_label_def(dispatch_branch, s);
	// load dispatch table, set function offset, jump to it.
	emit_partial_load_address(T1, s);
	emit_disptable_ref(type_name, s);
	s << endl;
	//emit_load(T1, 2, ACC, s);

	emit_load(T1, offset, T1, s);

	emit_jalr(T1, s);
	emit_addiu(SP, SP, 8, s);
	// restore offset
	set_offset(old_offset);
	s << "# ----> out static_dispatch_class " << endl;
}

void dispatch_class::code(ostream& s)
{
	s << "# ----> in dispatch_class " << endl;

	int dispatch_branch = get_nextbranch();

	//store FP and SELF
	emit_store(FP, 0, SP, s);
	emit_addiu(SP, SP, -4, s);
	emit_store(SELF, 0, SP, s);
	emit_addiu(SP, SP, -4, s);

	//parameters
	emit_addiu(SP, SP, -(4 * actual->len()), s);

	for (int i = actual->first(), of = actual->len(); actual->more(i); i = actual->next(i), of--) {
		Expression e = actual->nth(i);
		e->code(s);
		s << "# output parameter for " << name << endl;
		emit_store(ACC, of, SP, s);
	}
	int old_offset = cur_offset();

	clear_variable_offset(); /*old fp + self + # of actuals*/

	expr->code(s);

	// if acc is void, call _dispatch_abort
	emit_bne(ACC, ZERO, dispatch_branch, s);
	// pass line # to t1 and filename to a0

	emit_load_string(ACC, stringtable.add_string(cur_filename->get_string()), s);
	//emit_move(ACC, SELF, s);
	emit_load_imm(T1, get_line_number(), s);
	emit_jal("_dispatch_abort", s);

	//output dispatch_branch
	emit_label_def(dispatch_branch, s);

	// load dispatch table, set function offset, jump to it.
	//emit_load(T1, 2, SELF, s);

	int offset = 0;
	if (expr->get_type() == SELF_TYPE) {
		emit_move(ACC, SELF, s);
		offset = class_method_offset[selfclass][name];
	}
	else {
		offset = class_method_offset[expr->get_type()][name];
	}

	// load dispatch table, set function offset, jump to it.
	emit_load(T1, 2, ACC, s);

	s << "# expr->get_type() = " << expr->get_type() << " " << selfclass << " " << (int)selfclass << " name = " << name << " " << (int)name << " offset = " << offset << endl;

	emit_load(T1, offset, T1, s);

	emit_jalr(T1, s);
	emit_addiu(SP, SP, 8, s);
	// restore offset
	set_offset(old_offset);
	s << "# ----> out dispatch_class " << endl;
}

void cond_class::code(ostream& s)
{
	s << "# ----> in cond_class " << endl;
	int true_branch = get_nextbranch();
	int false_branch = get_nextbranch();
	int endif_branch = get_nextbranch();
	pred->code(s);
	emit_load(ACC, 3, ACC, s);
	emit_bne(ACC, ZERO, true_branch, s);
	//output false_branch label;
	emit_label_def(false_branch, s);
	else_exp->code(s);
	emit_branch(endif_branch, s);
	//output true_branch label;
	emit_label_def(true_branch, s);
	then_exp->code(s);
	//output endif label;
	emit_label_def(endif_branch, s);
	s << "# ----> out cond_class " << endl;
}

void loop_class::code(ostream& s)
{
	s << "# ----> in loop_class " << endl;
	int while_branch = get_nextbranch();
	int end_branch = get_nextbranch();
	//output while_branch label;
	emit_label_def(while_branch, s);
	pred->code(s);
	emit_load(ACC, 3, ACC, s);
	emit_beqz(ACC, end_branch, s); // false
	body->code(s);
	emit_branch(while_branch, s);
	//output end label;
	emit_label_def(end_branch, s);
	// set void
	emit_move(ACC, ZERO, s);
	s << "# ----> out loop_class " << endl;
}

void typcase_class::code(ostream& s)
{
	s << "# ----> in typcase_class " << endl;
	s << "# the following implementation picks up the first one which exactly matches with expr, which is not correct for now! " << endl;
	int unmatched_branch = get_nextbranch();
	int void_branch = get_nextbranch();
	int end_branch = get_nextbranch();
	std::vector<int> cases_branch;
	for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
		cases_branch.push_back(get_nextbranch());
	}
	expr->code(s);
	int of = next_offset();
	emit_store(ACC, of, FP, s);
	// jump to void
	emit_beqz(ACC, void_branch, s);

	int cases_len = cases->len();
	for (int i = cases->first(), j = 0; cases->more(i); i = cases->next(i), j++) {
		//output case label;
		emit_label_def(cases_branch[j], s);

		identifiers->enterscope();
		branch_class* branch = (branch_class*)cases->nth(i);
		s << "# put " << branch->name->get_string() << " at " << of << endl;
		identifiers->addid(branch->name->get_string(), new int(of));

		emit_load(T1, 0, ACC, s); // get expr class tag
		int case_class_tag = classtag[branch->type_decl];
		emit_load_imm(T2, case_class_tag, s); // set case class tag

		if (j == cases_len - 1) // last case
		{
			emit_bne(T1, T2, unmatched_branch, s);
		}
		else {
			emit_bne(T1, T2, cases_branch[j + 1], s);
		}
		// if match, then eval
		s << "# match case " << j << endl;
		branch->expr->code(s);
		// go to end
		emit_branch(end_branch, s);
		identifiers->exitscope();
	}

	//output void label;
	emit_label_def(void_branch, s);

	// pass line # to t1 and filename to a0

	emit_load_string(ACC, stringtable.lookup_string(cur_filename->get_string()), s);
	emit_load_imm(T1, get_line_number(), s);
	emit_jal("_case_abort2", s);

	//output unmatch label;
	emit_label_def(unmatched_branch, s);
	emit_move(ACC, SELF, s);
	emit_jal("_case_abort", s);

	//output end label;
	emit_label_def(end_branch, s);
	s << "# ----> out typcase_class " << endl;
}

void block_class::code(ostream& s)
{
	s << "# ----> in block_class " << endl;
	for (int i = body->first(); body->more(i); i = body->next(i)) {
		Expression e = body->nth(i);
		e->code(s);
	}
	s << "# ----> out block_class " << endl;
}

void let_class::code(ostream& s)
{
	s << "# ----> in let_class " << endl;
	identifiers->enterscope();
	init->code(s);
	int of = next_offset();
	identifiers->addid(identifier->get_string(), new int(of));
	s << "# put " << identifier << " at " << of << endl;
	// handle default init
	if (init->no_expr()) {
		if (type_decl == Int) {
			IntEntryP lensym = inttable.add_int(0);
			emit_partial_load_address(ACC, s);
			lensym->code_ref(s);
			s << endl;
		}
		else if (type_decl == Bool) {
			emit_partial_load_address(ACC, s);
			falsebool.code_ref(s);
			s << endl;
		}
		else if (type_decl == Str) {
			emit_partial_load_address(ACC, s);
			stringtable.lookup_string("")->code_ref(s);
			s << endl;
		}
		else {
			emit_move(ACC, ZERO, s);
		}
	}
	emit_store(ACC, of, FP, s);
	body->code(s);
	identifiers->exitscope();
	s << "# ----> out let_class " << endl;
}

void plus_class::code(ostream& s)
{
	s << "# ----> in plus_class " << endl;
	e1->code(s);
	emit_store(ACC, 0, SP, s);
	emit_addiu(SP, SP, -4, s);
	e2->code(s);
	emit_store(ACC, 0, SP, s);
	emit_addiu(SP, SP, -4, s);
	emit_load(ACC, 2, SP, s);
	emit_jal("Object.copy", s);
	emit_load(T1, 3, ACC, s);
	emit_load(T2, 1, SP, s);
	emit_load(T2, 3, T2, s);
	emit_add(T1, T1, T2, s);
	emit_store(T1, 3, ACC, s);
	emit_addiu(SP, SP, 8, s);
	s << "# ----> out plus_class " << endl;
}

void sub_class::code(ostream& s)
{
	s << "# ----> in sub_class " << endl;
	e1->code(s);
	emit_store(ACC, 0, SP, s);
	emit_addiu(SP, SP, -4, s);
	e2->code(s);
	emit_store(ACC, 0, SP, s);
	emit_addiu(SP, SP, -4, s);
	emit_load(ACC, 2, SP, s);
	emit_jal("Object.copy", s);
	emit_load(T1, 3, ACC, s);
	emit_load(T2, 1, SP, s);
	emit_load(T2, 3, T2, s);
	emit_sub(T1, T1, T2, s);
	emit_store(T1, 3, ACC, s);
	emit_addiu(SP, SP, 8, s);
	s << "# ----> out sub_class " << endl;
}

void mul_class::code(ostream& s)
{
	s << "# ----> in mul_class " << endl;
	e1->code(s);
	emit_store(ACC, 0, SP, s);
	emit_addiu(SP, SP, -4, s);
	e2->code(s);
	emit_store(ACC, 0, SP, s);
	emit_addiu(SP, SP, -4, s);
	emit_load(ACC, 2, SP, s);
	emit_jal("Object.copy", s);
	emit_load(T1, 3, ACC, s);
	emit_load(T2, 1, SP, s);
	emit_load(T2, 3, T2, s);
	emit_mul(T1, T1, T2, s);
	emit_store(T1, 3, ACC, s);
	emit_addiu(SP, SP, 8, s);
	s << "# ----> out mul_class " << endl;
}

void divide_class::code(ostream& s)
{
	s << "# ----> in divide_class " << endl;
	e1->code(s);
	emit_store(ACC, 0, SP, s);
	emit_addiu(SP, SP, -4, s);
	e2->code(s);
	emit_store(ACC, 0, SP, s);
	emit_addiu(SP, SP, -4, s);
	emit_load(ACC, 2, SP, s);
	emit_jal("Object.copy", s);
	emit_load(T1, 3, ACC, s);
	emit_load(T2, 1, SP, s);
	emit_load(T2, 3, T2, s);
	emit_div(T1, T1, T2, s);
	emit_store(T1, 3, ACC, s);
	emit_addiu(SP, SP, 8, s);
	s << "# ----> out divide_class " << endl;
}

void neg_class::code(ostream& s)
{
	s << "# ----> in neg_class " << endl;
	e1->code(s);
	emit_jal("Object.copy", s);
	emit_load(T1, 3, ACC, s);
	emit_neg(T1, T1, s);
	emit_store(T1, 3, ACC, s);
	s << "# ----> out neg_class " << endl;
}

void lt_class::code(ostream& s)
{
	s << "# ----> in lt_class " << endl;
	int less_branch = get_nextbranch();
	int end_branch = get_nextbranch();

	e1->code(s);
	emit_store(ACC, 0, SP, s);
	emit_addiu(SP, SP, -4, s);
	e2->code(s);
	emit_move(T1, ACC, s);
	emit_load(T1, 3, T1, s);
	emit_load(ACC, 1, SP, s);
	emit_load(ACC, 3, ACC, s);
	emit_blt(ACC, T1, less_branch, s);
	emit_load_bool(ACC, falsebool, s);
	emit_branch(end_branch, s);
	//output less_branch label;
	emit_label_def(less_branch, s);
	emit_load_bool(ACC, truebool, s);
	//output ebd_branch label;
	emit_label_def(end_branch, s);
	emit_addiu(SP, SP, 4, s);
	s << "# ----> out lt_class " << endl;
}

void eq_class::code(ostream& s)
{
	s << "# ----> in eq_class " << endl;

	int end_branch = get_nextbranch();

	e1->code(s);
	emit_store(ACC, 0, SP, s);
	emit_addiu(SP, SP, -4, s);
	e2->code(s);

	emit_load(T1, 1, SP, s);
	emit_addiu(SP, SP, 4, s);
	emit_move(T2, ACC, s);
	emit_load_bool(ACC, truebool, s);
	emit_beq(T1, T2, end_branch, s);
	emit_load_bool(A1, falsebool, s);
	emit_jal("equality_test", s);

	//output end_branch label;
	emit_label_def(end_branch, s);

	s << "# ----> out eq_class " << endl;
}

void leq_class::code(ostream& s)
{
	s << "# ----> in leq_class " << endl;
	int leq_branch = get_nextbranch();
	int end_branch = get_nextbranch();

	e1->code(s);
	emit_store(ACC, 0, SP, s);
	emit_addiu(SP, SP, -4, s);
	e2->code(s);
	emit_move(T1, ACC, s);
	emit_load(T1, 3, T1, s);
	emit_load(ACC, 1, SP, s);
	emit_load(ACC, 3, ACC, s);
	emit_bleq(ACC, T1, leq_branch, s);
	emit_load_bool(ACC, falsebool, s);
	emit_branch(end_branch, s);
	//output less_branch label;
	emit_label_def(leq_branch, s);
	emit_load_bool(ACC, truebool, s);
	//output ebd_branch label;
	emit_label_def(end_branch, s);
	emit_addiu(SP, SP, 4, s);
	s << "# ----> out leq_class " << endl;
}

void comp_class::code(ostream& s)
{ // not
	s << "# ----> in comp_class " << endl;

	int end_branch = get_nextbranch();
	e1->code(s);
	emit_load(T1, 3, ACC, s);
	emit_load_bool(ACC, truebool, s);
	emit_beqz(T1, end_branch, s);
	emit_load_bool(ACC, falsebool, s);
	//output end_branch label;
	emit_label_def(end_branch, s);
	s << "# ----> out comp_class " << endl;
}

void int_const_class::code(ostream& s)
{
	//
	// Need to be sure we have an IntEntry *, not an arbitrary Symbol
	//
	emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(ostream& s)
{
	emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream& s)
{
	emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream& s)
{

	s << "# ----> in new__class " << endl;

	/*
	emit_partial_load_address(ACC, s);

	if (type_name == SELF_TYPE)
	emit_protobj_ref(selfclass, s);
	else
	emit_protobj_ref(type_name, s);
	s << endl;
	*/

	if (type_name == SELF_TYPE) {
		emit_partial_load_address(ACC, s);
		s << CLASSOBJTAB;
		s << endl;

		// address of class_projObj = ACC + 4 * tag * 2 = ACC + (tag << 3)
		emit_load(T1, 0, SELF, s); // class tag
		emit_sll(T1, T1, 3, s);
		emit_addu(T2, ACC, T1, s); // go to this address

		emit_store(T2, 0, SP, s);
		emit_addiu(SP, SP, -4, s);

		emit_load(ACC, 0, T2, s); // load address

		emit_jal("Object.copy", s);

		emit_load(T2, 1, SP, s);
		emit_addiu(SP, SP, 4, s);

		emit_store(ACC, 0, SP, s);
		emit_addiu(SP, SP, -4, s);

		// use T1, now ACC contains the address to the object just copied
		emit_load(T1, 1, T2, s); // load init address

		emit_jalr(T1, s);
	}
	else {
		emit_partial_load_address(ACC, s);
		emit_protobj_ref(type_name, s);
		s << endl;

		emit_jal("Object.copy", s);
		emit_store(ACC, 0, SP, s);
		emit_addiu(SP, SP, -4, s);

		s << JAL;
		emit_init_ref(type_name, s);
		s << endl;
	}

	/*
	s << JAL;

	if (type_name == SELF_TYPE)
	emit_init_ref(selfclass, s);
	else
	emit_init_ref(type_name, s);

	s << endl;
	*/

	emit_load(ACC, 1, SP, s);
	emit_addiu(SP, SP, 4, s);

	s << "# ----> out new__class " << endl;
}

void isvoid_class::code(ostream& s)
{
	s << "# ----> in isvoid_class " << endl;
	int settrue_branch = get_nextbranch();
	int end_branch = get_nextbranch();
	e1->code(s);
	emit_beqz(ACC, settrue_branch, s); // ACC is zero, which means void, goto set ACC=true branch
	// now it is not void, set ACC = false;
	emit_load_bool(ACC, falsebool, s);
	emit_branch(end_branch, s);
	//output settrue_branch label;
	emit_label_def(settrue_branch, s);
	emit_load_bool(ACC, truebool, s);
	//output end_branch label;
	emit_label_def(end_branch, s);
	s << "# ----> out isvoid_class " << endl;
}

void no_expr_class::code(ostream& s)
{
	s << "# ----> in no_expr_class " << endl;
	s << "# ----> out no_expr_class " << endl;
}

void object_class::code(ostream& s)
{

	s << "# ----> in object_class " << endl;

	if (name == self) {
		/*do nothing for a method? try to set ACC to SELF*/
		emit_move(ACC, SELF, s);
		return;
	}

	/* using a symboltable, load it (the address) to acc*/

	int* offset = identifiers->lookup(name->get_string());

	int of = 0;
	if (offset == NULL) // attrs
	{
		of = class_attr_typeoffset[selfclass][name].second;
		s << "# --- attrs." << endl;
		emit_load(ACC, of, SELF, s);
	}
	else {
		of = *offset;
		s << "# --- variables." << endl;
		s << "# name " << name << " offset " << (*offset) << endl;
		emit_load(ACC, of, FP, s);
	}

	s << "# ----> out object_class " << endl;
}
