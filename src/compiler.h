#ifndef COMPILER_H
#define COMPILER_H

#include <stdint.h>
#include <stddef.h>

typedef uint32_t u32;
typedef uint64_t u64;
typedef int32_t i32;
typedef int64_t i64;

typedef struct StackHeader StackHeader;
struct StackHeader
{
  size_t count, capacity;
};

#define Stack(T) T *
#define stack_header(stack) ((StackHeader *)(stack) - 1)
#define stack_init(stack, capacity) stack = _stack_init(capacity, sizeof(*stack))
#define stack_push(stack, elem) stack = _stack_push(stack, &elem, sizeof(*stack))
#define stack_pop(stack, count) _stack_pop(stack, count, sizeof(*stack))

typedef struct String String;
struct String
{
  char *data;
  size_t size;
};

typedef struct StringView StringView;
struct StringView
{
  const char *data;
  size_t size;
};

enum TokenType
  {
    Token_Binop_Start,
    Token_Or,
    Token_And,
    Token_Double_Equals,
    Token_Plus,
    Token_Minus,
    Token_Times,
    Token_Divide,
    Token_Binop_End,

    Token_Open_Paren,
    Token_Open_Bracket,
    Token_Open_Curly,

    Token_Close_Paren,
    Token_Close_Bracket,
    Token_Close_Curly,

    Token_Colon_Equal,
    Token_Colon,
    Token_Comma,
    Token_Equal,
    Token_Question_Mark,
    Token_Semicolon,

    Token_Type_Start,
    Token_Type_Void,
    Token_Type_Char,
    Token_Type_Bool,
    Token_Type_Int,
    Token_Type_End,

    Token_Print,
    Token_If,
    Token_Then,
    Token_Else,
    Token_While,
    Token_Do,
    Token_Break,
    Token_Continue,
    Token_Return,

    Token_Integer_Literal,
    Token_False_Literal,
    Token_True_Literal,
    Token_Char_Literal,
    Token_String_Literal,
    Token_Identifier,

    Token_End_Of_File
  };
typedef enum TokenType TokenType;

typedef struct Token Token;
struct Token
{
  TokenType type;
  StringView view;
};

typedef struct Tokenizer Tokenizer;
struct Tokenizer
{
  const char *at;
  Token token;
  StringView last_id;
};

enum AstType
  {
    Ast_Type_Void,
    Ast_Type_Char,
    Ast_Type_Bool,
    Ast_Type_Int
  };
typedef enum AstType AstType;

typedef struct AstSymbolVar AstSymbolVar;
struct AstSymbolVar
{
  AstType type;
};

typedef struct AstSymbolArray AstSymbolArray;
struct AstSymbolArray
{
  AstType type;
};

typedef struct AstSymbolFunc AstSymbolFunc;
struct AstSymbolFunc
{
  AstType return_type;
};

enum AstSymbolType
  {
    Ast_Symbol_Var,
    Ast_Symbol_Array,
    Ast_Symbol_Func
  };
typedef enum AstSymbolType AstSymbolType;

typedef struct AstSymbol AstSymbol;
struct AstSymbol
{
  AstSymbolType type;
  String id;
  u32 frame;

  union
  {
    AstSymbolVar var;
    AstSymbolArray array;
    AstSymbolFunc func;
  } as;
};

typedef struct AstSymbolNode AstSymbolNode;
struct AstSymbolNode
{
  AstSymbol data;
  AstSymbolNode *next;
};

typedef struct AstSymbolTable AstSymbolTable;
struct AstSymbolTable
{
  AstSymbolNode **data;
  size_t count;
  size_t capacity;
};

typedef struct AstExpr AstExpr;

enum AstBinopType
  {
    Ast_Binop_Or,
    Ast_Binop_And,
    Ast_Binop_Equals,
    Ast_Binop_Plus,
    Ast_Binop_Minus,
    Ast_Binop_Times,
    Ast_Binop_Divide
  };
typedef enum AstBinopType AstBinopType;

typedef struct AstBinop AstBinop;
struct AstBinop
{
  AstBinopType type;
  AstExpr *left, *right;
};

typedef struct AstInlineIf AstInlineIf;
struct AstInlineIf
{
  AstExpr *cond, *if_true, *if_false;
};

typedef struct AstArrayAccess AstArrayAccess;
struct AstArrayAccess
{
  AstSymbolArray *symbol;
  AstExpr *index;
};

typedef struct AstFuncCall AstFuncCall;
struct AstFuncCall
{
  AstSymbolFunc *symbol;
  AstExpr **expr_list;
  size_t expr_count;
};

enum AstExprType
  {
    Ast_Expr_Binop,
    Ast_Expr_Unop_Minus,
    Ast_Expr_Inline_If,
    Ast_Expr_Var,
    Ast_Expr_Array_Access,
    Ast_Expr_Func_Call,
    Ast_Expr_Char,
    Ast_Expr_Bool,
    Ast_Expr_Int64
  };
typedef enum AstExprType AstExprType;

struct AstExpr
{
  AstExprType type;

  union
  {
    AstBinop binop;
    AstExpr *unop;
    AstInlineIf inline_if;
    AstSymbolVar *var;
    AstArrayAccess array_access;
    AstFuncCall func_call;
    i64 character;
    i64 boolean;
    i64 int64;
  } as;
};

typedef struct AstStmt AstStmt;

typedef struct AstStmtBlock AstStmtBlock;
struct AstStmtBlock
{
  AstStmt *data;
  size_t count;
};

typedef struct AstVarDecl AstVarDecl;
struct AstVarDecl
{
  AstSymbolVar *symbol;
  AstType type;
  AstExpr *expr;
};

enum AstListInitType
  {
    Ast_List_Init_None,
    Ast_List_Init_Expr_List,
    Ast_List_Init_String_Literal
  };
typedef enum AstListInitType AstListInitType;

typedef struct AstListInit AstListInit;
struct AstListInit
{
  AstListInitType type;
  size_t count;

  union
  {
    AstExpr **exprs;
    char *string;
  } as;
};

typedef struct AstArrayDecl AstArrayDecl;
struct AstArrayDecl
{
  AstSymbolArray *symbol;
  AstType type;
  AstExpr *size;
  AstListInit list_init;
};

typedef struct AstFuncDecl AstFuncDecl;
struct AstFuncDecl
{
  AstSymbolFunc *symbol;
  AstType return_type;
  size_t param_list_idx;
  size_t param_count;
  AstStmtBlock stmt_list;
};

typedef struct AstFuncParam AstFuncParam;
struct AstFuncParam
{
  AstSymbolVar *symbol;
  AstType type;
};

enum AstDeclType
  {
    Ast_Decl_Var,
    Ast_Decl_Array,
    Ast_Decl_Func
  };
typedef enum AstDeclType AstDeclType;

typedef struct AstDecl AstDecl;
struct AstDecl
{
  AstDeclType type;

  union
  {
    AstVarDecl var;
    AstArrayDecl array;
    AstFuncDecl func;
  } as;
};

typedef struct AstIf AstIf;
struct AstIf
{
  AstExpr *cond;
  AstStmtBlock if_true, if_false;
};

typedef struct AstWhile AstWhile;
struct AstWhile
{
  AstExpr *cond;
  AstStmtBlock body;
};

typedef struct AstVarAssign AstVarAssign;
struct AstVarAssign
{
  AstSymbolVar *symbol;
  AstExpr *expr;
};

typedef struct AstArrayAssign AstArrayAssign;
struct AstArrayAssign
{
  AstSymbolArray *symbol;
  AstExpr *index, *expr;
};

enum AstStmtType
  {
    Ast_Stmt_Print,
    Ast_Stmt_If,
    Ast_Stmt_While,
    Ast_Stmt_Break,
    Ast_Stmt_Continue,
    Ast_Stmt_Return_Expr,
    Ast_Stmt_Return_Nothing,
    Ast_Stmt_Var_Assign,
    Ast_Stmt_Array_Assign,
    Ast_Stmt_Func_Call,
    Ast_Stmt_Decl
  };
typedef enum AstStmtType AstStmtType;

struct AstStmt
{
  AstStmtType type;

  union
  {
    AstExpr *print_expr;
    AstIf iff;
    AstWhile while_loop;
    AstExpr *return_expr;
    AstVarAssign var_assign;
    AstArrayAssign array_assign;
    AstFuncCall func_call;
    AstDecl decl;
  } as;
};

typedef struct Ast Ast;
struct Ast
{
  Stack(AstDecl) decl_list;
  Stack(AstFuncParam) func_param_list;
  Stack(AstExpr *) expr_list;
  Stack(AstStmt) stmt_list;
};

typedef struct Compiler Compiler;
struct Compiler
{
  Tokenizer tokz;
  Ast ast;

  u32 *frames;
  u32 frame_count;
  u32 frame_capacity;
  u32 last_unused_frame;

  AstSymbolTable symbols;
};

#endif // COMPILER_H
