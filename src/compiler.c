#include "utils.c"
#include "types.h"

#include <ctype.h>
#include <limits.h>

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

#define LOWEST_PREC (-127)
#define HIGHEST_PREC (127)

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

void advance(Compiler *c);
void assert_token_is(Compiler *c, TokenType type);
void parse_top_level(Compiler *c);
void parse_prec(Compiler *c, int limit);
void parse_expr(Compiler *c);
void parse_variable_declaration(Compiler *c);
void parse_array_declaration(Compiler *c);
void parse_function_declaration(Compiler *c);
void parse_arg_list(Compiler *c);
void parse_stmt_block(Compiler *c, bool should_create_frame);

static Compiler compiler;

void
compile(const char *filepath)
{
  char *file_data = read_entire_file(filepath);

  compiler.tokz.at = file_data;

#define MAX_FRAME_COUNT 64
  static u32 frames[MAX_FRAME_COUNT];

  compiler.frames = frames;
  compiler.frame_count = 0;
  compiler.frame_capacity = MAX_FRAME_COUNT;
  compiler.last_unused_frame = 0;

  compiler.symbols.count = 0;
  compiler.symbols.capacity = 32;
  compiler.symbols.data
    = malloc_or_exit(compiler.symbols.capacity
                     * sizeof(*compiler.symbols.data));

  parse_top_level(&compiler);

  free(file_data);
}

void
push_frame(Compiler *c)
{
  assert(c->frame_count < c->frame_capacity);

  c->frames[c->frame_count++] = c->last_unused_frame++;
}

void
pop_frame(Compiler *c)
{
  assert(c->frame_count > 0);

  --c->frame_count;
}

u32
get_current_frame(Compiler *c)
{
  assert(c->frame_count > 0);

  return c->frames[c->frame_count - 1];
}

u32
compute_hash(StringView view, u32 frame)
{
  u32 hash = 5381;

  while (view.size-- > 0)
    hash = 33 * hash + view.data[view.size];

  hash = 33 * hash + frame;

  return hash;
}

AstSymbol *
insert_ast_symbol(Compiler *c, StringView id)
{
  if (c->symbols.capacity == 0
      || (double)c->symbols.count / c->symbols.capacity > 0.75)
    {
      size_t new_cap = 2 * c->symbols.capacity + 1;
      AstSymbolNode **new_data
        = malloc_or_exit(new_cap * sizeof(*new_data));

      for (size_t i = new_cap; i-- > 0; )
        new_data[i] = NULL;

      for (size_t i = c->symbols.capacity; i-- > 0; )
        {
          AstSymbolNode *node = c->symbols.data[i];

          while (node != NULL)
            {
              size_t ind = compute_hash(string2view(node->data.id),
                                        node->data.frame) % new_cap;

              AstSymbolNode *const next = node->next;

              node->next = new_data[ind];
              new_data[ind] = node;

              node = next;
            }
        }

      free(c->symbols.data);
      c->symbols.data = new_data;
      c->symbols.capacity = new_cap;
    }

  u32 frame = get_current_frame(c);
  size_t ind = compute_hash(id, frame) % c->symbols.capacity;

  AstSymbolNode *node = c->symbols.data[ind];

  while (node != NULL)
    {
      if (frame == node->data.frame
          && are_views_equal(id, string2view(node->data.id)))
        assert(false && "redefinition");

      node = node->next;
    }

  node = malloc_or_exit(sizeof(*node));
  node->data.id = copy_view_to_string(id);
  node->data.frame = frame;

  node->next = c->symbols.data[ind];
  c->symbols.data[ind] = node;

  return &node->data;
}

AstSymbol *
find_symbol_in_frames(Compiler *c, StringView view)
{
  for (u32 i = c->frame_count; i-- > 0; )
    {
      u32 frame = c->frames[i];

      AstSymbolNode *node = c->symbols.data[compute_hash(view, frame)
                                            % c->symbols.capacity];

      while (node != NULL)
        {
          if (frame == node->data.frame
              && are_views_equal(view, string2view(node->data.id)))
            return &node->data;

          node = node->next;
        }
    }

  assert(false && "undeclared identifier");
}

void
parse_top_level(Compiler *c)
{
  push_frame(c);

  advance(c);

  do
    {
      assert_token_is(c, Token_Identifier);
      advance(c);
      assert_token_is(c, Token_Colon);

      AstSymbol *symbol = insert_ast_symbol(c, c->tokz.last_id);

      advance(c);

      switch (c->tokz.token.type)
        {
        case Token_Open_Paren:
          advance(c);
          symbol->type = Ast_Symbol_Func;
          parse_function_declaration(c);
          break;
        case Token_Open_Bracket:
          advance(c);
          symbol->type = Ast_Symbol_Array;
          parse_array_declaration(c);
          assert_token_is(c, Token_Semicolon);
          advance(c);
          break;
        default:
          symbol->type = Ast_Symbol_Var;
          parse_variable_declaration(c);
          assert_token_is(c, Token_Semicolon);
          advance(c);
        }
    }
  while (c->tokz.token.type != Token_End_Of_File);

  pop_frame(c);
}

bool
can_char_be_in_identifier(char ch)
{
  return isalnum(ch) || ch == '_';
}

void
advance(Compiler *c)
{
  {
    const char *at = c->tokz.at;

    while (*at != '\0')
      {
        while (isspace(*at))
          ++at;

        if (at[0] == '/' && at[1] == '/')
          while (*at != '\n' && *at != '\0')
            ++at;
        else
          break;
      }

    c->tokz.at = at;
  }

  c->tokz.token.view.data = c->tokz.at;

  switch (*c->tokz.at)
    {
    case '\0':
      c->tokz.token.type = Token_End_Of_File;
      c->tokz.token.view.size = 0;
      return;
    case '\'':
      assert(c->tokz.at[1] != '\0');
      c->tokz.at += 2;
      c->tokz.token.type = Token_Char_Literal;
      c->tokz.token.view.size = 2;
      return;
    case '\"':
      ++c->tokz.at;

      while (*c->tokz.at != '\"' && *c->tokz.at != '\0')
        ++c->tokz.at;

      assert(*c->tokz.at == '\"');
      ++c->tokz.at;

      c->tokz.token.type = Token_String_Literal;
      c->tokz.token.view.size = c->tokz.at - c->tokz.token.view.data;

      return;
    default:
      if (isdigit(*c->tokz.at))
        {
          while (isdigit(*++c->tokz.at))
            ;

          c->tokz.token.type = Token_Integer_Literal;
          c->tokz.token.view.size = c->tokz.at - c->tokz.token.view.data;

          return;
        }
    }

  struct Keyword
  {
    const char *text;
    TokenType type;
  } keywords[]
    = { { "||", Token_Or },
        { "&&", Token_And },
        { "==", Token_Double_Equals },
        { "+", Token_Plus },
        { "-", Token_Minus },
        { "*", Token_Times },
        { "/", Token_Divide },

        { "(", Token_Open_Paren },
        { "[", Token_Open_Bracket },
        { "{", Token_Open_Curly },

        { ")", Token_Close_Paren },
        { "]", Token_Close_Bracket },
        { "}", Token_Close_Curly },

        { ":=", Token_Colon_Equal },
        { ":", Token_Colon },
        { ",", Token_Comma },
        { "=", Token_Equal },
        { "?", Token_Question_Mark },
        { ";", Token_Semicolon },

        { "void", Token_Type_Void },
        { "char", Token_Type_Char },
        { "bool", Token_Type_Bool },
        { "int", Token_Type_Int },

        { "print", Token_Print },
        { "if", Token_If },
        { "then", Token_Then },
        { "else", Token_Else },
        { "while", Token_While },
        { "do", Token_Do },
        { "break", Token_Break },
        { "continue", Token_Continue },
        { "return", Token_Return },

        { "false", Token_False_Literal },
        { "true", Token_True_Literal } };

  // Yes, this constant is hardcoded. Should be the index of the last
  // token (in the keywords array) that can't be interpreted as
  // identifier.
  size_t const last_non_identifier = 18;

  for (size_t i = 0; i < sizeof(keywords) / sizeof(*keywords); i++)
    {
      const char *after_prefix
        = is_prefix(keywords[i].text, c->tokz.at);

      // If keyword can be interpred as identifier, only consume it if
      // the character that goes after prefix can't be in identifier.
      if (after_prefix != NULL
          && (i <= last_non_identifier
              || !can_char_be_in_identifier(*after_prefix)))
        {
          c->tokz.at = after_prefix;
          c->tokz.token.type = keywords[i].type;
          c->tokz.token.view.size
            = c->tokz.at - c->tokz.token.view.data;

          return;
        }
    }

  if (isalpha(*c->tokz.at))
    {
      while (can_char_be_in_identifier(*++c->tokz.at))
        ;

      c->tokz.token.type = Token_Identifier;
      c->tokz.token.view.size = c->tokz.at - c->tokz.token.view.data;
      c->tokz.last_id = c->tokz.token.view;

      return;
    }
  else
    assert(false);
}

void
assert_token_is(Compiler *c, TokenType expected)
{
  assert(c->tokz.token.type == expected);
}

void
assert_token_is_type(Compiler *c)
{
  assert(Token_Type_Start < c->tokz.token.type
         && c->tokz.token.type < Token_Type_End);
}

void
assert_token_is_non_void_type(Compiler *c)
{
  assert_token_is_type(c);
  assert(c->tokz.token.type != Token_Type_Void);
}

int
prec_of(TokenType type)
{
  switch (type)
    {
    case Token_Or:
      return 0;
    case Token_And:
      return 1;
    case Token_Double_Equals:
      return 2;
    case Token_Plus:
    case Token_Minus:
      return 3;
    case Token_Times:
    case Token_Divide:
      return 4;
    default:
      return LOWEST_PREC - 1;
    }
}

void
parse_base(Compiler *c)
{
  switch (c->tokz.token.type)
    {
    case Token_Minus:
      advance(c);
      parse_prec(c, HIGHEST_PREC);
      break;
    case Token_Open_Paren:
      advance(c);
      parse_expr(c);
      break;
    case Token_Integer_Literal:
    case Token_False_Literal:
    case Token_True_Literal:
    case Token_Char_Literal:
    case Token_String_Literal:
      advance(c);
      break;
    case Token_Identifier:
      find_symbol_in_frames(c, c->tokz.last_id);

      advance(c);

      switch (c->tokz.token.type)
        {
        case Token_Open_Paren:
          advance(c);
          parse_arg_list(c);
          break;
        case Token_Open_Bracket:
          advance(c);
          parse_expr(c);
          assert_token_is(c, Token_Close_Bracket);
          advance(c);
          break;
        default:
          ;
        }

      break;
    default:
      assert(false);
    }
}

void
parse_prec(Compiler *c, int limit)
{
  parse_base(c);

  TokenType op = c->tokz.token.type;
  int prev_prec = INT_MAX, curr_prec = prec_of(op);

  while (curr_prec >= limit && curr_prec < prev_prec)
    {
      do
        {
          advance(c);
          parse_prec(c, limit + 1);
          op = c->tokz.token.type;
        }
      while (curr_prec == prec_of(op));

      prev_prec = curr_prec;
      curr_prec = prec_of(op);
    }
}

void
parse_expr(Compiler *c)
{
  parse_prec(c, LOWEST_PREC);

  if (c->tokz.token.type == Token_Question_Mark)
    {
      advance(c);
      parse_expr(c);
      assert_token_is(c, Token_Colon);
      advance(c);
      parse_expr(c);
    }
}

void
parse_arg_list(Compiler *c)
{
  if (c->tokz.token.type != Token_Close_Paren)
    {
      do
        {
          parse_expr(c);

          if (c->tokz.token.type == Token_Close_Paren)
            break;
          else
            assert_token_is(c, Token_Comma);

          advance(c);
        }
      while (true);
    }

  advance(c);
}

void
parse_single_statement(Compiler *c)
{
  switch (c->tokz.token.type)
    {
    case Token_Print:
      advance(c);
      assert_token_is(c, Token_Open_Paren);
      advance(c);
      parse_expr(c);
      assert_token_is(c, Token_Close_Paren);
      advance(c);
      assert_token_is(c, Token_Semicolon);
      advance(c);
      break;
    case Token_If:
      advance(c);
      parse_expr(c);
      assert_token_is(c, Token_Then);
      advance(c);

      assert(c->tokz.token.type != Token_Else);

      switch (c->tokz.token.type)
        {
        case Token_Semicolon:
          advance(c);
          break;
        case Token_Open_Curly:
          advance(c);
          parse_stmt_block(c, true);
          break;
        default:
          push_frame(c);
          parse_single_statement(c);
          pop_frame(c);
        }

      if (c->tokz.token.type == Token_Else)
        {
          advance(c);

          switch (c->tokz.token.type)
            {
            case Token_Semicolon:
              advance(c);
              break;
            case Token_Open_Curly:
              advance(c);
              parse_stmt_block(c, true);
              break;
            default:
              push_frame(c);
              parse_single_statement(c);
              pop_frame(c);
            }
        }

      break;
    case Token_While:
      advance(c);
      parse_expr(c);
      assert_token_is(c, Token_Do);
      advance(c);

      switch (c->tokz.token.type)
        {
        case Token_Semicolon:
          advance(c);
          break;
        case Token_Open_Curly:
          advance(c);
          parse_stmt_block(c, true);
          break;
        default:
          push_frame(c);
          parse_single_statement(c);
          pop_frame(c);
        }

      break;
    case Token_Break:
      advance(c);
      assert_token_is(c, Token_Semicolon);
      advance(c);
      break;
    case Token_Continue:
      advance(c);
      assert_token_is(c, Token_Semicolon);
      advance(c);
      break;
    case Token_Return:
      advance(c);

      if (c->tokz.token.type != Token_Semicolon)
        {
          parse_expr(c);
          assert_token_is(c, Token_Semicolon);
        }

      advance(c);

      break;
    case Token_Identifier:
      advance(c);

      switch (c->tokz.token.type)
        {
        case Token_Open_Paren:
          find_symbol_in_frames(c, c->tokz.last_id);

          advance(c);
          parse_arg_list(c);
          break;
        case Token_Open_Bracket:
          find_symbol_in_frames(c, c->tokz.last_id);

          advance(c);
          parse_expr(c);
          assert_token_is(c, Token_Close_Bracket);
          advance(c);
          assert_token_is(c, Token_Colon_Equal);
          advance(c);
          parse_expr(c);
          break;
        case Token_Colon_Equal:
          find_symbol_in_frames(c, c->tokz.last_id);

          advance(c);
          parse_expr(c);
          break;
        case Token_Colon:
          {
            AstSymbol *symbol
              = insert_ast_symbol(c, c->tokz.last_id);

            advance(c);

            switch (c->tokz.token.type)
              {
              case Token_Open_Bracket:
                advance(c);
                symbol->type = Ast_Symbol_Array;
                parse_array_declaration(c);
                break;
              default:
                symbol->type = Ast_Symbol_Var;
                parse_variable_declaration(c);
              }
          }

          break;
        default:
          assert(false);
        }

      assert_token_is(c, Token_Semicolon);
      advance(c);

      break;
    default:
      assert(false);
    }
}

void
parse_stmt_block(Compiler *c, bool should_create_frame)
{
  if (should_create_frame)
    push_frame(c);

  while (c->tokz.token.type != Token_Close_Curly
         && c->tokz.token.type != Token_End_Of_File)
    {
      parse_single_statement(c);
    }

  assert_token_is(c, Token_Close_Curly);
  advance(c);

  if (should_create_frame)
    pop_frame(c);
}

void
parse_variable_declaration(Compiler *c)
{
  assert_token_is_non_void_type(c);
  advance(c);

  switch (c->tokz.token.type)
    {
    case Token_Semicolon:
      break;
    case Token_Equal:
      advance(c);
      parse_expr(c);
      break;
    default:
      assert(false);
    }
}

void
parse_list_initilizer(Compiler *c)
{
  if (c->tokz.token.type != Token_Close_Bracket)
    {
      do
        {
          parse_expr(c);

          if (c->tokz.token.type == Token_Close_Bracket)
            break;
          else
            assert_token_is(c, Token_Comma);

          advance(c);
        }
      while (true);
    }

  advance(c);
}

void
parse_array_declaration(Compiler *c)
{
  if (c->tokz.token.type == Token_Close_Bracket)
    ;
  else
    {
      parse_expr(c);
      assert_token_is(c, Token_Close_Bracket);
    }

  advance(c);
  assert_token_is_non_void_type(c);
  advance(c);

  switch (c->tokz.token.type)
    {
    case Token_Semicolon:
      break;
    case Token_Equal:
      advance(c);

      switch (c->tokz.token.type)
        {
        case Token_Open_Bracket:
          advance(c);
          parse_list_initilizer(c);
          break;
        case Token_String_Literal:
          advance(c);
          break;
        default:
          assert(false);
        }

      break;
    default:
      assert(false);
    }
}

void
parse_function_declaration(Compiler *c)
{
  push_frame(c);

  if (c->tokz.token.type == Token_Close_Paren)
    ;
  else
    {
      do
        {
          assert_token_is(c, Token_Identifier);
          advance(c);
          assert_token_is(c, Token_Colon);
          advance(c);
          assert_token_is_non_void_type(c);

          {
            AstSymbol *symbol = insert_ast_symbol(c, c->tokz.last_id);
            symbol->type = Ast_Symbol_Var;
          }

          advance(c);

          if (c->tokz.token.type == Token_Close_Paren)
            break;
          else
            assert_token_is(c, Token_Comma);

          advance(c);
        }
      while (true);
    }

  advance(c);

  if (c->tokz.token.type != Token_Open_Curly)
    {
      assert_token_is_type(c);
      advance(c);
      assert_token_is(c, Token_Open_Curly);
    }

  advance(c);

  parse_stmt_block(c, false);

  pop_frame(c);
}
