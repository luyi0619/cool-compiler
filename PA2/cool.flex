/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

int comment_level;

/*
 *  Add Your own definitions here
 */

#define append_str(character) \
	*(string_buf_ptr++) = character; \
	if(string_buf + MAX_STR_CONST == string_buf_ptr ) { BEGIN(COOL_STRING_LONG); cool_yylval.error_msg = "String constant too long";return ERROR;} 
%}

/*
 * Define names for regular expressions here.
 */

%x		                                COOL_SINGLECOMMENT
%x		                                COOL_COMMENT
%x		                                COOL_STRING
%x		                                COOL_STRING_ESCAPE
%x		                                COOL_STRING_NULL
%x		                                COOL_STRING_LONG
DARROW		                            =>
ASSIGN		                            <-
LE		                                <=

%%

 /*
  *  Nested comments
  */


 /*
  *  The multiple-character operators.
  */

\n			                        { curr_lineno++; }
[ \t\f\r\t\v]+


{DARROW}		                        { return (DARROW); }
{ASSIGN}		                        { return (ASSIGN); }
{LE}			                        { return (LE); }

"--"            		                { BEGIN(COOL_SINGLECOMMENT); }
<COOL_SINGLECOMMENT>\n  	                { BEGIN(INITIAL); curr_lineno++; }
<COOL_SINGLECOMMENT>.	

"(*"            	                        { BEGIN(COOL_COMMENT); comment_level = 1; }
<COOL_COMMENT>"(*"    	                        { comment_level++; }
<COOL_COMMENT>\n    	                        { curr_lineno++; }
<COOL_COMMENT><<EOF>>	                        { BEGIN(INITIAL); cool_yylval.error_msg = "EOF in comment"; return ERROR; }
<COOL_COMMENT>.	
<COOL_COMMENT>"*)"	                        { comment_level --; if (comment_level == 0) BEGIN(INITIAL); }

"*)"			                        { cool_yylval.error_msg = "Unmatched *)"; return ERROR; }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */



(?i:class)		                        { return CLASS; }
(?i:else)		                        { return ELSE; }
(?i:fi)			                        { return FI; }
(?i:if)			                        { return IF; }
(?i:in)			                        { return IN; }
(?i:inherits)		                        { return INHERITS; }
(?i:isvoid)		                        { return ISVOID; }
(?i:let)		                        { return LET; }
(?i:loop)		                        { return LOOP; }
(?i:pool)		                        { return POOL; }
(?i:then)		                        { return THEN; }
(?i:while)		                        { return WHILE; }
(?i:case)		                        { return CASE; }
(?i:esac)		                        { return ESAC; }
(?i:new)		                        { return NEW; }
(?i:of)			                        { return OF; }
(?i:not)		                        { return NOT; }
t(?i:rue)		                        { cool_yylval.boolean = true;  return BOOL_CONST; }
f(?i:alse)		                        { cool_yylval.boolean = false; return BOOL_CONST; }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

\"            			                { BEGIN(COOL_STRING); string_buf_ptr = string_buf;}
<COOL_STRING>\"    		                { BEGIN(INITIAL); cool_yylval.symbol = stringtable.add_string(string_buf, string_buf_ptr - string_buf); return STR_CONST; }
<COOL_STRING><<EOF>>		                { BEGIN(INITIAL); cool_yylval.error_msg = "EOF in string constant"; return ERROR; }
<COOL_STRING>\n    		                { BEGIN(INITIAL); cool_yylval.error_msg = "Unterminated string constant"; curr_lineno++; return ERROR; }
<COOL_STRING>\0		    	                { BEGIN(COOL_STRING_NULL); cool_yylval.error_msg = "String contains null character"; return ERROR; }
<COOL_STRING>\\0	    	                { append_str('0');}
<COOL_STRING>\\n	    	                { append_str('\n');}
<COOL_STRING>\\t	    	                { append_str('\t');}
<COOL_STRING>\\b	    	                { append_str('\b');}
<COOL_STRING>\\f	    	                { append_str('\f');}
<COOL_STRING>\\    		                { BEGIN(COOL_STRING_ESCAPE); }
<COOL_STRING>.		    	                { append_str(yytext[0]);}

<COOL_STRING_ESCAPE><<EOF>>	                { BEGIN(INITIAL); cool_yylval.error_msg = "EOF in string constant"; return ERROR; }
<COOL_STRING_ESCAPE>\0	    	                { BEGIN(COOL_STRING_NULL); cool_yylval.error_msg = "String contains null character"; return ERROR; }
<COOL_STRING_ESCAPE>\n	    	                { BEGIN(COOL_STRING); append_str('\n'); curr_lineno++; }
<COOL_STRING_ESCAPE>.    	                { BEGIN(COOL_STRING); append_str(yytext[0]);}


<COOL_STRING_NULL>\"		                { BEGIN(INITIAL); }
<COOL_STRING_NULL>\n		                { BEGIN(INITIAL); curr_lineno++; }
<COOL_STRING_NULL>.		

<COOL_STRING_LONG>\"		                { BEGIN(INITIAL); }
<COOL_STRING_LONG>\n		                { BEGIN(INITIAL); curr_lineno++; }
<COOL_STRING_LONG>.		


[0-9]+						{ cool_yylval.symbol = inttable.add_string(yytext); return INT_CONST; }
[A-Z][A-Za-z0-9_]*				{ cool_yylval.symbol = idtable.add_string(yytext); return TYPEID; }
[a-z][A-Za-z0-9_]*				{ cool_yylval.symbol = idtable.add_string(yytext); return OBJECTID; }
[^A-Za-z0-9+\-*/~=\(\)<;:\.,@\{\}]		{ yytext[1] = 0; cool_yylval.error_msg = yytext; return ERROR; }
.						{ return yytext[0]; }

%%


