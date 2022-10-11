#include "compiler.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stddef.h>
#include <ctype.h>
#include <limits.h>
#include <assert.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

void *
malloc_or_exit(size_t size)
{
  void *data = malloc(size);

  if (data == NULL)
    {
      fprintf(stderr,
              "ERROR: failed to allocate %zu bytes.\n",
              size);
      abort();
    }

  return data;
}

void *
realloc_or_exit(void *data, size_t size)
{
  void *new_data = realloc(data, size);

  if (new_data == NULL)
    {
      fprintf(stderr,
              "ERROR: failed to reallocate to %zu bytes.\n",
              size);
      abort();
    }

  return new_data;
}

const char *
is_prefix(const char *prefix, const char *string)
{
  char p, s;

  do
    {
      p = *prefix++;
      s = *string++;
    }
  while (p != '\0' && p == s);

  return p != '\0' ? NULL : string - 1;
}

char *
read_entire_file(const char *filepath)
{
  enum
    {
      Ok,
      Failed_To_Open_File,
      Failed_To_Stat_File,
      Failed_To_Read_File,
      Failed_To_Close_File
    } status = Ok;

  int fd = open(filepath, O_RDONLY);

  if (fd == -1)
    {
      status = Failed_To_Open_File;
      goto skip_body;
    }

  off_t file_size = 0;

  {
    struct stat stats;

    if (fstat(fd, &stats) == -1)
      {
        status = Failed_To_Stat_File;
        goto skip_body;
      }

    file_size = stats.st_size;
  }

  char *file_data = malloc_or_exit(file_size + 1);

  if (read(fd, file_data, file_size) != file_size)
    {
      status = Failed_To_Read_File;
      goto skip_body;
    }

  file_data[file_size] = '\0';

  if (close(fd) == -1)
    {
      status = Failed_To_Close_File;
      goto skip_body;
    }

 skip_body: { }
  const char *action = NULL;

  switch (status)
    {
    case Ok:
      return file_data;
    case Failed_To_Open_File:
      action = "open";
      break;
    case Failed_To_Stat_File:
      action = "stat";
      break;
    case Failed_To_Read_File:
      action = "read";
      break;
    case Failed_To_Close_File:
      action = "close";
      break;
    }

  fprintf(stderr,
          "ERROR: failed to %s file \'%s\'.\n",
          action,
          filepath);
  abort();
}

void *
_stack_init(size_t capacity, size_t elem_size)
{
  StackHeader *data = malloc_or_exit(sizeof(StackHeader)
                                     + capacity * elem_size);
  data->count = 0;
  data->capacity = capacity;

  return data + 1;
}

void
stack_destroy(void *stack)
{
  free(stack_header(stack));
}

size_t
stack_count(void *stack)
{
  return stack_header(stack)->count;
}

void *
_stack_push(void *stack, void *elem, size_t elem_size)
{
  StackHeader *header = stack_header(stack);

  if (header->count >= header->capacity)
    {
      header->capacity = 2 * header->capacity + 1;
      header = realloc_or_exit(header,
                               sizeof(StackHeader)
                               + header->capacity * elem_size);
      stack = header + 1;
    }

  memcpy((char *)stack + header->count * elem_size,
         elem,
         elem_size);
  ++header->count;

  return stack;
}

void *
_stack_pop(void *stack, size_t count, size_t elem_size)
{
  if (count == 0)
    return NULL;

  StackHeader *header = stack_header(stack);

  assert(count <= header->count);

  void *data = malloc_or_exit(count * elem_size);

  header->count -= count;
  memcpy(data,
         (char *)stack + header->count * elem_size,
         count * elem_size);

  return data;
}

String
copy_view_to_string(StringView view)
{
  String str;
  str.data = malloc_or_exit(view.size + 1);
  str.size = view.size;

  memcpy(str.data, view.data, view.size);
  str.data[view.size] = '\0';

  return str;
}

char *
copy_view_to_cstring(StringView view)
{
  char *data = malloc_or_exit(view.size + 1);

  memcpy(data, view.data, view.size);
  data[view.size] = '\0';

  return data;
}

StringView
string2view(String str)
{
  return (StringView){ str.data, str.size };
}

bool
are_views_equal(StringView v0, StringView v1)
{
  if (v0.size != v1.size)
    return false;

  while (v0.size-- > 0)
    if (v0.data[v0.size] != v1.data[v0.size])
      return false;

  return true;
}

#define LOWEST_PREC (-127)
#define HIGHEST_PREC 127
#define MAX_FRAME_COUNT 64

static Compiler compiler;

void parse_top_level(Compiler *c);
AstExpr *parse_prec(Compiler *c, int limit);
AstExpr *parse_expr(Compiler *c);
AstStmt parse_single_statement(Compiler *c);
void parse_variable_declaration(Compiler *c, AstVarDecl *decl);
void parse_array_declaration(Compiler *c, AstArrayDecl *decl);
void parse_function_declaration(Compiler *c, AstFuncDecl *decl);
AstExpr **parse_arg_list(Compiler *c, size_t *arg_count);
AstStmtBlock parse_stmt_block(Compiler *c, bool should_create_frame);

void
compile(const char *filepath)
{
  char *file_data = read_entire_file(filepath);

  compiler.tokz.at = file_data;

  stack_init(compiler.ast.global_decl_list, 32);
  stack_init(compiler.ast.func_param_list, 32);
  stack_init(compiler.ast.expr_list, 32);
  stack_init(compiler.ast.stmt_list, 32);

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

  assert(stack_count(compiler.ast.expr_list) == 0);
  assert(stack_count(compiler.ast.stmt_list) == 0);

  stack_destroy(compiler.ast.expr_list);
  stack_destroy(compiler.ast.stmt_list);

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
        {
          fprintf(stderr,
                  "ERROR: redefinition of identifier \'%.*s\'.\n",
                  (int)id.size,
                  id.data);
          exit(EXIT_FAILURE);
        }

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
find_symbol_in_frames(Compiler *c, StringView id)
{
  for (u32 i = c->frame_count; i-- > 0; )
    {
      u32 frame = c->frames[i];

      AstSymbolNode *node = c->symbols.data[compute_hash(id, frame)
                                            % c->symbols.capacity];

      while (node != NULL)
        {
          if (frame == node->data.frame
              && are_views_equal(id, string2view(node->data.id)))
            return &node->data;

          node = node->next;
        }
    }

  fprintf(stderr,
          "ERROR: use of undeclared identifier \'%.*s\'.\n",
          (int)id.size,
          id.data);
  exit(EXIT_FAILURE);
}

void
assert_token_is(Compiler *c, TokenType expected)
{
  if (c->tokz.token.type != expected)
    {
      fputs("ERROR: unexpected token.\n", stderr);
      exit(EXIT_FAILURE);
    }
}

void
assert_token_is_type(Compiler *c)
{
  if (!(Token_Type_Start < c->tokz.token.type
        && c->tokz.token.type < Token_Type_End))
    {
      fputs("ERROR: token is not a type.\n", stderr);
      exit(EXIT_FAILURE);
    }
}

AstType
token_type2ast_type(Compiler *c)
{
  TokenType type = c->tokz.token.type;

  if (Token_Type_Start < type && type < Token_Type_End)
    return (AstType)(type - Token_Type_Start - 1);
  else
    {
      fputs("ERROR: token is not a type.\n", stderr);
      exit(EXIT_FAILURE);
    }
}

AstBinopType
token_type2binop_type(TokenType type)
{
  if (Token_Binop_Start < type && type < Token_Binop_End)
    return (AstBinopType)(type - Token_Binop_Start - 1);
  else
    {
      fputs("ERROR: token is not a binary operator.\n", stderr);
      exit(EXIT_FAILURE);
    }
}

void
expect_symbol(AstSymbol *symbol, AstSymbolType type)
{
  if (symbol->type != type)
    {
      fputs("ERROR: unexpected type of symbol.\n", stderr);
      exit(EXIT_FAILURE);
    }
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
      if (c->tokz.at[1] == '\0')
        {
          fputs("ERROR: missing character literal.\n", stderr);
          exit(EXIT_FAILURE);
        }

      c->tokz.at += 2;
      c->tokz.token.type = Token_Char_Literal;
      c->tokz.token.view.size = 2;
      return;
    case '\"':
      ++c->tokz.at;

      while (*c->tokz.at != '\"' && *c->tokz.at != '\0')
        ++c->tokz.at;

      if (*c->tokz.at != '\"')
        {
          fputs("ERROR: not terminated string literal.\n", stderr);
          exit(EXIT_FAILURE);
        }

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
    {
      fputs("ERROR: unexpected token.\n", stderr);
      exit(EXIT_FAILURE);
    }
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

      AstDecl decl;
      AstSymbol *symbol = insert_ast_symbol(c, c->tokz.last_id);

      advance(c);

      switch (c->tokz.token.type)
        {
        case Token_Open_Paren:
          advance(c);

          decl.type = Ast_Decl_Func;
          decl.as.func.symbol = &symbol->as.func;
          symbol->type = Ast_Symbol_Func;
          symbol->as.func.return_type = decl.as.func.return_type;

          parse_function_declaration(c, &decl.as.func);
          break;
        case Token_Open_Bracket:
          advance(c);

          decl.type = Ast_Decl_Array;
          decl.as.array.symbol = &symbol->as.array;
          symbol->type = Ast_Symbol_Array;
          symbol->as.array.type = decl.as.array.type;

          parse_array_declaration(c, &decl.as.array);
          assert_token_is(c, Token_Semicolon);
          advance(c);
          break;
        default:
          decl.type = Ast_Decl_Var;
          decl.as.var.symbol = &symbol->as.var;
          symbol->type = Ast_Symbol_Var;
          symbol->as.var.type = decl.as.var.type;

          parse_variable_declaration(c, &decl.as.var);
          assert_token_is(c, Token_Semicolon);
          advance(c);
        }

      stack_push(c->ast.global_decl_list, decl);
    }
  while (c->tokz.token.type != Token_End_Of_File);

  pop_frame(c);
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

AstExpr *
parse_base(Compiler *c)
{
  AstExpr *expr;

  switch (c->tokz.token.type)
    {
    case Token_Minus:
      advance(c);
      expr = malloc_or_exit(sizeof(AstExpr));
      expr->type = Ast_Expr_Unop_Minus;
      expr->as.unop = parse_prec(c, HIGHEST_PREC);
      break;
    case Token_Open_Paren:
      advance(c);
      expr = parse_expr(c);
      assert_token_is(c, Token_Close_Paren);
      advance(c);
      break;
    case Token_Integer_Literal:
      expr = malloc_or_exit(sizeof(AstExpr));
      expr->type = Ast_Expr_Int64;

      {
        i64 value = 0;

        for (size_t i = 0; i < c->tokz.token.view.size; i++)
          value = 10 * value + (c->tokz.token.view.data[i] - '0');

        expr->as.int64 = value;
      }

      advance(c);

      break;
    case Token_False_Literal:
      expr = malloc_or_exit(sizeof(AstExpr));
      expr->type = Ast_Expr_Bool;
      expr->as.boolean = 0;
      advance(c);
      break;
    case Token_True_Literal:
      expr = malloc_or_exit(sizeof(AstExpr));
      expr->type = Ast_Expr_Bool;
      expr->as.boolean = 1;
      advance(c);
      break;
    case Token_Char_Literal:
      expr = malloc_or_exit(sizeof(AstExpr));
      expr->type = Ast_Expr_Char;
      expr->as.character = c->tokz.token.view.data[1];
      advance(c);
      break;
    case Token_Identifier:
      {
        AstSymbol *symbol
          = find_symbol_in_frames(c, c->tokz.last_id);

        advance(c);

        switch (c->tokz.token.type)
          {
          case Token_Open_Paren:
            advance(c);
            expr = malloc_or_exit(sizeof(AstExpr));
            expr->type = Ast_Expr_Func_Call;
            expect_symbol(symbol, Ast_Symbol_Func);
            expr->as.func_call.symbol = &symbol->as.func;
            expr->as.func_call.expr_list
              = parse_arg_list(c, &expr->as.func_call.expr_count);
            break;
          case Token_Open_Bracket:
            advance(c);
            expr = malloc_or_exit(sizeof(AstExpr));
            expr->type = Ast_Expr_Array_Access;
            expect_symbol(symbol, Ast_Symbol_Array);
            expr->as.array_access.symbol = &symbol->as.array;
            expr->as.array_access.index = parse_expr(c);
            assert_token_is(c, Token_Close_Bracket);
            advance(c);
            break;
          default:
            ;
          }
      }

      break;
    default:
      fputs("ERROR: unexpected token.\n", stderr);
      exit(EXIT_FAILURE);
    }

  return expr;
}

AstExpr *
combine(TokenType op, AstExpr *left, AstExpr *right)
{
  AstExpr *expr = malloc_or_exit(sizeof(AstExpr));
  expr->type = Ast_Expr_Binop;
  expr->as.binop.type = token_type2binop_type(op);
  expr->as.binop.left = left;
  expr->as.binop.right = right;

  return expr;
}

AstExpr *
parse_prec(Compiler *c, int limit)
{
  AstExpr *left = parse_base(c);

  TokenType op = c->tokz.token.type;
  int prev_prec = INT_MAX, curr_prec = prec_of(op);

  while (curr_prec >= limit && curr_prec < prev_prec)
    {
      do
        {
          advance(c);
          left = combine(op, left, parse_prec(c, limit + 1));
          op = c->tokz.token.type;
        }
      while (curr_prec == prec_of(op));

      prev_prec = curr_prec;
      curr_prec = prec_of(op);
    }

  return left;
}

AstExpr *
parse_expr(Compiler *c)
{
  AstExpr *expr = parse_prec(c, LOWEST_PREC);

  if (c->tokz.token.type == Token_Question_Mark)
    {
      AstExpr *inline_if = malloc_or_exit(sizeof(AstExpr));
      inline_if->type = Ast_Expr_Inline_If;
      inline_if->as.inline_if.cond = expr;
      advance(c);
      inline_if->as.inline_if.if_true = parse_expr(c);
      assert_token_is(c, Token_Colon);
      advance(c);
      inline_if->as.inline_if.if_false = parse_expr(c);
      expr = inline_if;
    }

  return expr;
}

AstExpr **
parse_arg_list(Compiler *c, size_t *arg_count)
{
  size_t count = 0;

  if (c->tokz.token.type != Token_Close_Paren)
    {
      do
        {
          AstExpr *expr = parse_expr(c);
          stack_push(c->ast.expr_list, expr);
          ++count;

          if (c->tokz.token.type == Token_Close_Paren)
            break;
          else
            assert_token_is(c, Token_Comma);

          advance(c);
        }
      while (true);
    }

  advance(c);

  AstExpr **expr_list = stack_pop(c->ast.expr_list, count);
  *arg_count = count;

  return expr_list;
}

AstStmtBlock
parse_body(Compiler *c)
{
  AstStmtBlock block = { NULL, 0 };

  switch (c->tokz.token.type)
    {
    case Token_Semicolon:
      advance(c);
      break;
    case Token_Open_Curly:
      advance(c);
      block = parse_stmt_block(c, true);
      break;
    default:
      push_frame(c);

      block.count = 1;
      block.data =
        malloc_or_exit(block.count * sizeof(*block.data));
      block.data[0] = parse_single_statement(c);

      pop_frame(c);
    }

  return block;
}

AstStmt
parse_single_statement(Compiler *c)
{
  AstStmt stmt;

  switch (c->tokz.token.type)
    {
    case Token_Print:
      stmt.type = Ast_Stmt_Print;
      advance(c);
      assert_token_is(c, Token_Open_Paren);
      advance(c);
      stmt.as.print_expr = parse_expr(c);
      assert_token_is(c, Token_Close_Paren);
      advance(c);
      assert_token_is(c, Token_Semicolon);
      advance(c);
      break;
    case Token_If:
      stmt.type = Ast_Stmt_If;
      advance(c);
      stmt.as.iff.cond = parse_expr(c);
      assert_token_is(c, Token_Then);
      advance(c);

      if (c->tokz.token.type == Token_Else)
        {
          fputs("ERROR: body of an if statement cannot be empty"
                " (at least semicolon is required).\n",
                stderr);
          exit(EXIT_FAILURE);
        }

      stmt.as.iff.if_true = parse_body(c);
      stmt.as.iff.if_false = (AstStmtBlock){ NULL, 0 };

      if (c->tokz.token.type == Token_Else)
        {
          advance(c);
          stmt.as.iff.if_false = parse_body(c);
        }

      break;
    case Token_While:
      stmt.type = Ast_Stmt_While;
      advance(c);
      stmt.as.while_loop.cond = parse_expr(c);
      assert_token_is(c, Token_Do);
      advance(c);
      stmt.as.while_loop.body = parse_body(c);

      break;
    case Token_Break:
      stmt.type = Ast_Stmt_Break;
      advance(c);
      assert_token_is(c, Token_Semicolon);
      advance(c);
      break;
    case Token_Continue:
      stmt.type = Ast_Stmt_Continue;
      advance(c);
      assert_token_is(c, Token_Semicolon);
      advance(c);
      break;
    case Token_Return:
      stmt.type = Ast_Stmt_Return_Nothing;
      advance(c);

      if (c->tokz.token.type != Token_Semicolon)
        {
          stmt.type = Ast_Stmt_Return_Expr;
          stmt.as.return_expr = parse_expr(c);
          assert_token_is(c, Token_Semicolon);
        }

      advance(c);

      break;
    case Token_Identifier:
      advance(c);

      switch (c->tokz.token.type)
        {
        case Token_Open_Paren:
          {
            stmt.type = Ast_Stmt_Func_Call;
            AstSymbol *symbol
              = find_symbol_in_frames(c, c->tokz.last_id);

            advance(c);

            expect_symbol(symbol, Ast_Symbol_Func);
            stmt.as.func_call.symbol = &symbol->as.func;
            stmt.as.func_call.expr_list
              = parse_arg_list(c, &stmt.as.func_call.expr_count);
          }

          break;
        case Token_Open_Bracket:
          {
            stmt.type = Ast_Stmt_Array_Assign;
            AstSymbol *symbol
              = find_symbol_in_frames(c, c->tokz.last_id);
            expect_symbol(symbol, Ast_Symbol_Array);
            stmt.as.array_assign.symbol = &symbol->as.array;

            advance(c);
            stmt.as.array_assign.index = parse_expr(c);
            assert_token_is(c, Token_Close_Bracket);
            advance(c);
            assert_token_is(c, Token_Colon_Equal);
            advance(c);
            stmt.as.array_assign.expr = parse_expr(c);
          }

          break;
        case Token_Colon_Equal:
          {
            stmt.type = Ast_Stmt_Var_Assign;
            AstSymbol *symbol
              = find_symbol_in_frames(c, c->tokz.last_id);
            expect_symbol(symbol, Ast_Symbol_Var);
            stmt.as.var_assign.symbol = &symbol->as.var;

            advance(c);
            stmt.as.var_assign.expr = parse_expr(c);
          }

          break;
        case Token_Colon:
          {
            stmt.type = Ast_Stmt_Decl;
            AstSymbol *symbol
              = insert_ast_symbol(c, c->tokz.last_id);

            advance(c);

            switch (c->tokz.token.type)
              {
              case Token_Open_Bracket:
                advance(c);

                stmt.as.decl.type = Ast_Decl_Array;
                stmt.as.decl.as.array.symbol = &symbol->as.array;
                symbol->type = Ast_Symbol_Array;
                symbol->as.array.type = stmt.as.decl.as.array.type;

                parse_array_declaration(c, &stmt.as.decl.as.array);
                break;
              default:
                stmt.as.decl.type = Ast_Decl_Var;
                stmt.as.decl.as.var.symbol = &symbol->as.var;
                symbol->type = Ast_Symbol_Var;
                symbol->as.var.type = stmt.as.decl.as.var.type;

                parse_variable_declaration(c, &stmt.as.decl.as.var);
              }
          }

          break;
        default:
          fputs("ERROR: unexpected token.\n", stderr);
          exit(EXIT_FAILURE);
        }

      assert_token_is(c, Token_Semicolon);
      advance(c);

      break;
    default:
      fputs("ERROR: unexpected token.\n", stderr);
      exit(EXIT_FAILURE);
    }

  return stmt;
}

AstStmtBlock
parse_stmt_block(Compiler *c, bool should_create_frame)
{
  if (should_create_frame)
    push_frame(c);

  AstStmtBlock block;
  block.count = 0;

  while (c->tokz.token.type != Token_Close_Curly
         && c->tokz.token.type != Token_End_Of_File)
    {
      AstStmt stmt = parse_single_statement(c);
      stack_push(c->ast.stmt_list, stmt);
      ++block.count;
    }

  assert_token_is(c, Token_Close_Curly);
  advance(c);

  block.data = stack_pop(c->ast.stmt_list, block.count);

  if (should_create_frame)
    pop_frame(c);

  return block;
}

void
parse_variable_declaration(Compiler *c, AstVarDecl *decl)
{
  decl->type = token_type2ast_type(c);

  if (decl->type == Ast_Type_Void)
    {
      fputs("ERROR: type cannot be void.\n", stderr);
      exit(EXIT_FAILURE);
    }

  advance(c);

  switch (c->tokz.token.type)
    {
    case Token_Semicolon:
      decl->expr = NULL;
      break;
    case Token_Equal:
      advance(c);
      decl->expr = parse_expr(c);
      break;
    default:
      fputs("ERROR: unexpected token.\n", stderr);
      exit(EXIT_FAILURE);
    }
}

AstListInit
parse_list_initializer(Compiler *c)
{
  AstListInit list_init;
  list_init.type = Ast_List_Init_Expr_List;
  list_init.count = 0;

  if (c->tokz.token.type != Token_Close_Bracket)
    {
      do
        {
          AstExpr *expr = parse_expr(c);
          stack_push(c->ast.expr_list, expr);
          ++list_init.count;

          if (c->tokz.token.type == Token_Close_Bracket)
            break;
          else
            assert_token_is(c, Token_Comma);

          advance(c);
        }
      while (true);
    }

  list_init.as.exprs = stack_pop(c->ast.expr_list, list_init.count);
  advance(c);

  return list_init;
}

void
parse_array_declaration(Compiler *c, AstArrayDecl *decl)
{
  if (c->tokz.token.type != Token_Close_Bracket)
    {
      decl->size = parse_expr(c);
      assert_token_is(c, Token_Close_Bracket);
    }

  advance(c);

  decl->type = token_type2ast_type(c);

  if (decl->type == Ast_Type_Void)
    {
      fputs("ERROR: type cannot be void.\n", stderr);
      exit(EXIT_FAILURE);
    }

  advance(c);

  switch (c->tokz.token.type)
    {
    case Token_Semicolon:
      decl->list_init.type = Ast_List_Init_None;
      break;
    case Token_Equal:
      advance(c);

      switch (c->tokz.token.type)
        {
        case Token_Open_Bracket:
          advance(c);
          decl->list_init = parse_list_initializer(c);
          break;
        case Token_String_Literal:
          decl->list_init.type = Ast_List_Init_String_Literal;
          decl->list_init.count = c->tokz.token.view.size;
          decl->list_init.as.string
            = copy_view_to_cstring(c->tokz.token.view);

          advance(c);
          break;
        default:
          fputs("ERROR: unexpected token.\n", stderr);
          exit(EXIT_FAILURE);
        }

      break;
    default:
      fputs("ERROR: unexpected token.\n", stderr);
      exit(EXIT_FAILURE);
    }
}

void
parse_function_declaration(Compiler *c, AstFuncDecl *decl)
{
  push_frame(c);

  decl->param_list_idx = stack_count(c->ast.func_param_list);
  decl->param_count = 0;

  if (c->tokz.token.type != Token_Close_Paren)
    {
      do
        {
          assert_token_is(c, Token_Identifier);
          advance(c);
          assert_token_is(c, Token_Colon);
          advance(c);

          AstSymbol *symbol = insert_ast_symbol(c, c->tokz.last_id);

          AstFuncParam param;
          param.symbol = &symbol->as.var;
          param.type = token_type2ast_type(c);

          if (param.type == Ast_Type_Void)
            {
              fputs("ERROR: type cannot be void.\n", stderr);
              exit(EXIT_FAILURE);
            }

          symbol->type = Ast_Symbol_Var;
          symbol->as.var.type = param.type;

          stack_push(c->ast.func_param_list, param);
          ++decl->param_count;

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
  decl->return_type = Ast_Type_Void;

  if (c->tokz.token.type != Token_Open_Curly)
    {
      decl->return_type = token_type2ast_type(c);
      advance(c);
      assert_token_is(c, Token_Open_Curly);
    }

  advance(c);

  decl->stmt_list = parse_stmt_block(c, false);

  pop_frame(c);
}
