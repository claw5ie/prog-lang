#ifndef COMPILER_H
#define COMPILER_H

#include <stdint.h>
#include <stddef.h>

typedef uint32_t u32;
typedef uint64_t u64;
typedef int32_t i32;
typedef int64_t i64;

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

typedef struct Compiler Compiler;
struct Compiler
{
  Tokenizer tokz;

  u32 *frames;
  u32 frame_count;
  u32 frame_capacity;
  u32 last_unused_frame;

  AstSymbolTable symbols;
};

#endif // COMPILER_H
