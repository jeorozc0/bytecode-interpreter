#include <complex.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "object.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
  Token current;
  Token previous;
  bool hadError;  // have compile time error
  bool panicMode; // error will be suppressed if in panic mode, ends when the
                  // parser reach sync point
} Parser;

// all of Lox's precedence levels, from lowest to highest
typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT, // =
  PREC_OR,         // or
  PREC_AND,        // and
  PREC_EQUALITY,   // == !=
  PREC_COMPARISON, // < > <= >=
  PREC_TERM,       // + -
  PREC_FACTOR,     // * /
  PREC_UNARY,      // ! -
  PREC_CALL,       // . ()
  PREC_PRIMARY
} Precedence;

// even `canAssign` is only useful for setter and assignment
// C compiler requires all parse functions have the same type
typedef void (*ParseFn)(bool canAssign);

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

typedef struct {
  Token name;
  int depth;
} Local;

typedef enum { TYPE_FUNCTION, TYPE_SCRIPT } FunctionType;

typedef struct Compiler {
  struct Compiler *enclosing;
  ObjFunction *function;
  FunctionType type;

  Local locals[UINT8_COUNT];
  int localCount;
  int scopeDepth;
} Compiler;

Parser parser;
Compiler *current = NULL;

static Chunk *currentChunk() { return &current->function->chunk; }

static void errorAt(Token *token, const char *message) {
  // if in panic mode, suppress error
  if (parser.panicMode)
    return;
  // get into panic mode when an error occurs
  parser.panicMode = true;
  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
  parser.hadError = true;
}

static void error(const char *message) {
  // more often an error will be reported at the token we just consumed
  errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char *message) {
  errorAt(&parser.current, message);
}

static void advance() {
  // take old current and store it in previous
  // so we can get the lexeme after matching a token
  parser.previous = parser.current;

  for (;;) {
    parser.current = scanToken();
    // keep looping until find a non-error token
    if (parser.current.type != TOKEN_ERROR)
      break;

    errorAtCurrent(parser.current.start);
  }
}

// read and validate next token
static void consume(TokenType type, const char *message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  errorAtCurrent(message);
}

static bool check(TokenType type) { return parser.current.type == type; }

// only consume the token if type is as expected
// but leave the token alone when unmatched, so it can be matched by other type
// later
static bool match(TokenType type) {
  if (!check(type))
    return false;
  advance();
  return true;
}

static void emitByte(uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.previous.line);
}

// Emits a backward jump instruction for loops
static void emitLoop(int loopStart) {
  // Emit the loop instruction opcode
  emitByte(OP_LOOP);

  // Calculate how far backward to jump:
  // Current position - loop start + 2 bytes for offset operand
  int offset = currentChunk()->count - loopStart + 2;

  // Ensure loop body fits in 16-bit offset
  if (offset > UINT16_MAX)
    error("Loop body too large.");

  // Emit the 16-bit offset in big-endian format:
  emitByte((offset >> 8) & 0xff); // High byte
  emitByte(offset & 0xff);        // Low byte
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
}

// Emits a jump instruction and returns the position of the jump offset
static int emitJump(uint8_t instruction) {
  emitByte(instruction); // Write the jump opcode
  emitByte(0xff);        // Placeholder bytes for jump offset
  emitByte(0xff);        // Will be patched later with actual offset
  return currentChunk()->count -
         2; // Return position where jump offset will be written
}

static void emitReturn() {
  emitByte(OP_NIL);
  emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
  int constant = addConstant(currentChunk(), value);
  if (constant > UINT8_MAX) {
    error("Too many constants in one chunk.");
    return 0;
  }

  return (uint8_t)constant;
}

static void emitConstant(Value value) {
  emitBytes(OP_CONSTANT, makeConstant(value));
}

// Updates a previously emitted jump instruction with the correct offset
static void patchJump(int offset) {
  // Calculate relative jump distance (accounting for size of jump operand)
  int jump = currentChunk()->count - offset - 2;

  // Ensure jump distance fits in 16 bits
  if (jump > UINT16_MAX) {
    error("Too much code to jump over.");
  }

  // Write the 16-bit jump offset into the bytecode, big-endian
  currentChunk()->code[offset] = (jump >> 8) & 0xff; // High byte
  currentChunk()->code[offset + 1] = jump & 0xff;    // Low byte
}

// Initializes a new compiler instance for function compilation
static void initCompiler(Compiler *compiler, FunctionType type) {
  // Capture about-to-no-longer-be-current compiler
  compiler->enclosing = current;
  // Initially set function to NULL before proper initialization
  compiler->function = NULL;

  // Set the type of function being compiled (script, function )
  compiler->type = type;

  // Reset local variable tracking
  compiler->localCount = 0;
  compiler->scopeDepth = 0;

  // Create a new function object to hold the compiled code
  compiler->function = newFunction();

  // Make this the currently active compiler
  current = compiler;
  // Copy name of function if we are not at top level
  if (type != TYPE_SCRIPT) {
    current->function->name =
        copyString(parser.previous.start, parser.previous.length);
  }

  // Initialize slot zero for implicit 'this' in methods
  // or the function itself in function declarations
  Local *local = &current->locals[current->localCount++];
  local->depth = 0;
  local->name.start = ""; // Empty name for implicit variables
  local->name.length = 0;
}

// Finalizes compilation of a function and returns the compiled function object
static ObjFunction *endCompiler() {
  // Emit return instruction to ensure all functions return properly
  emitReturn();

  // Get the completed function object from current compiler
  ObjFunction *function = current->function;

#ifdef DEBUG_PRINT_CODE
  // If compilation was successful and debug mode is on,
  // disassemble the chunk for debugging
  if (!parser.hadError) {
    disassembleChunk(currentChunk(),
                     // Use function name if available, otherwise "script"
                     function->name != NULL ? function->name->chars
                                            : "scrtipt");
  }
#endif

  current = current->enclosing;
  return function;
}

static void beginScope() { current->scopeDepth++; }

static void endScope() {
  current->scopeDepth--;
  while (current->localCount > 0 &&
         current->locals[current->localCount - 1].depth > current->scopeDepth) {
    emitByte(OP_POP);
    current->localCount--;
  }
}

static void expression();
static void statement();
static void declaration();
static ParseRule *getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

static bool identifiersEqual(Token *a, Token *b) {
  if (a->length != b->length)
    return false;
  return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler *compiler, Token *name) {
  for (int i = compiler->localCount - 1; i >= 0; i--) {
    Local *local = &compiler->locals[i];
    if (identifiersEqual(name, &local->name)) {
      if (local->depth == -1) {
        error("Cannot read local variable in its own initializer.");
      }
      return i;
    }
  }

  return -1;
}

// takes the given token and adds its lexeme to the chunk's constant table as a
// string returns the index of that constant in the constant table
static uint8_t identifierConstant(Token *name) {
  return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static void addLocal(Token name) {
  if (current->localCount == UINT8_COUNT) {
    error("Too many local variables in function.");
    return;
  }

  Local *local = &current->locals[current->localCount++];
  local->name = name;
  local->depth = -1;
}

static void declareVariable() {
  if (current->scopeDepth == 0)
    return;

  Token *name = &parser.previous;
  for (int i = current->localCount - 1; i >= 0; i--) {
    Local *local = &current->locals[i];
    if (local->depth != -1 && local->depth < current->scopeDepth)
      break;
    if (identifiersEqual(name, &local->name)) {
      error("Variable with this name already declared in this scope.");
    }
  }
  addLocal(*name);
}

// Parse a variable declaration and return its identifier's constant table index
static uint8_t parseVariable(const char *errorMessage) {
  // Expect and consume a variable name
  consume(TOKEN_IDENTIFIER, errorMessage);

  // Add variable to current scope's declarations
  declareVariable();

  // For local variables (non-zero scope depth), return 0
  // since they don't need a constant table entry
  if (current->scopeDepth > 0)
    return 0;

  // For global variables, add name to constant table and return index
  return identifierConstant(&parser.previous);
}

static void markInitialized() {
  if (current->scopeDepth == 0)
    return;
  current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(uint8_t global) {
  if (current->scopeDepth > 0) {
    markInitialized();
    return;
  }
  emitBytes(OP_DEFINE_GLOBAL, global);
}

static uint8_t argumentList() {
  uint8_t argCount = 0;
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      expression();
      if (argCount == 255) {
        error("Can't have more than 255 arguments.");
      }
      argCount++;
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  return argCount;
}

static void and_(bool canAssign) {
  int endJump = emitJump(OP_JUMP_IF_FALSE);

  emitByte(OP_POP);
  parsePrecedence(PREC_AND);

  patchJump(endJump);
}

static void binary(bool canAssign) {
  TokenType operatorType = parser.previous.type;
  ParseRule *rule = getRule(operatorType);
  // Each binary operator’s right-hand operand precedence is one level higher
  // than its own, because the binary operators are left-associative.
  parsePrecedence((Precedence)(rule->precedence + 1));

  switch (operatorType) {
  case TOKEN_BANG_EQUAL:
    emitBytes(OP_EQUAL, OP_NOT);
    break;
  case TOKEN_EQUAL_EQUAL:
    emitByte(OP_EQUAL);
    break;
  case TOKEN_GREATER:
    emitByte(OP_GREATER);
    break;
  case TOKEN_GREATER_EQUAL:
    emitBytes(OP_LESS, OP_NOT);
    break;
  case TOKEN_LESS:
    emitByte(OP_LESS);
    break;
  case TOKEN_LESS_EQUAL:
    emitBytes(OP_GREATER, OP_NOT);
    break;
  case TOKEN_PLUS:
    emitByte(OP_ADD);
    break;
  case TOKEN_MINUS:
    emitByte(OP_SUBTRACT);
    break;
  case TOKEN_STAR:
    emitByte(OP_MULTIPLY);
    break;
  case TOKEN_SLASH:
    emitByte(OP_DIVIDE);
    break;
  default:
    return; // Unreachable.
  }
}

// Compiles a function call expression
static void call(bool canAssign) {
  // Parse argument list and get argument count
  uint8_t argCount = argumentList();

  // Emit call instruction with argument count
  // (callee is already on stack from parsing prefix)
  emitBytes(OP_CALL, argCount);
}

// directly push true/false/nil on the stack
// instead of storing on constant table, as they have only 3 values
static void literal(bool canAssign) {
  switch (parser.previous.type) {
  case TOKEN_FALSE:
    emitByte(OP_FALSE);
    break;
  case TOKEN_NIL:
    emitByte(OP_NIL);
    break;
  case TOKEN_TRUE:
    emitByte(OP_TRUE);
    break;
  default:
    return; // Unreachable.
  }
}

static void expression() { parsePrecedence(PREC_ASSIGNMENT); }

static void block() {
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    declaration();
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

// Compiles a function definition into bytecode
static void function(FunctionType type) {
  // Create and initialize a new compiler for this function
  Compiler compiler;
  initCompiler(&compiler, type);

  // Create a new scope for function parameters and body
  beginScope();

  // Parse function syntax
  consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  // Parse function parameters until we hit the closing parenthesis
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      // Track number of parameters
      current->function->arity++;

      // Check parameter limit (bytecode restriction)
      if (current->function->arity > 255) {
        errorAtCurrent("Can't have more than 255 parameters.");
      }

      // Parse parameter name as a local variable
      uint8_t constant = parseVariable("Expect parameter name.");

      // Define it immediately (parameters are pre-initialized)
      defineVariable(constant);
    } while (match(TOKEN_COMMA)); // Continue if there are more parameters
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
  consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");

  // Compile the function body
  block();

  // Finish compilation and get the resulting function object
  ObjFunction *function = endCompiler();

  // Emit the function object as a constant in the enclosing function's chunk
  emitBytes(OP_CONSTANT, makeConstant(OBJ_VAL(function)));
}

// Compiles a function declaration statement
static void funcDeclaration() {
  // Parse and declare the function's name as a global variable
  uint8_t global = parseVariable("Expect function name.");

  // Mark variable as initialized right away so function can be recursive
  markInitialized();

  // Compile the function body
  function(TYPE_FUNCTION);

  // Define the global variable that holds the function
  defineVariable(global);
}

static void varDeclaration() {
  // add the variable name to constant table
  uint8_t global = parseVariable("Expect variable name.");

  // evaluate initializer expression
  // result is saved on stack
  if (match(TOKEN_EQUAL)) {
    expression();
  } else {
    // implicitly initializes to `nil`
    emitByte(OP_NIL);
  }

  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

  // defines the new variable and stores its initial value
  // initial value is left on the stack
  defineVariable(global);
}

/**
 * @brief An expression, followed by a semicolon.
 */
static void expressionStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
  emitByte(OP_POP);
}

// Compiles a for loop with optional initialization, condition, and increment
static void forStatement() {
  // Create scope for loop variable if any
  beginScope();

  // Parse initializer
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
  if (match(TOKEN_SEMICOLON)) {
    // No initializer
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    expressionStatement();
  }

  // Store start of loop body
  int loopStart = currentChunk()->count;
  int exitJump = -1;

  // Parse condition
  if (!match(TOKEN_SEMICOLON)) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

    // Jump out of loop if condition is false
    exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // Pop condition value
  }

  // Parse increment
  if (!match(TOKEN_RIGHT_PAREN)) {
    // Jump over increment to start of body
    int bodyJump = emitJump(OP_JUMP);

    // Compile increment expression
    int incrementStart = currentChunk()->count;
    expression();
    emitByte(OP_POP); // Pop increment value
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

    // After body, jump to increment
    emitLoop(loopStart);
    loopStart = incrementStart;
    patchJump(bodyJump);
  }

  // Compile body
  statement();
  emitLoop(loopStart);

  // Patch condition exit jump if present
  if (exitJump != -1) {
    patchJump(exitJump);
    emitByte(OP_POP);
  }

  endScope();
}

static void ifStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after 'if'.");

  int thenJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement();

  int elseJump = emitJump(OP_JUMP);

  patchJump(thenJump);
  emitByte(OP_POP);

  if (match(TOKEN_ELSE)) {
    statement();
  }
  patchJump(elseJump);
}

static void printStatement() {
  // `print` has been consumed, just parse and compile the expression after it
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_PRINT);
}

// Compiles a while statement, handling the condition and loop body
static void whileStatement() {
  // Store the position where we'll loop back to
  int loopStart = currentChunk()->count;

  // Parse and compile the condition
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after 'while'.");

  // Emit jump to exit the loop if condition is false
  int exitJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP); // Pop condition value before executing body

  // Compile the loop body
  statement();

  // Emit backward jump to return to start of loop
  emitLoop(loopStart);

  // Patch the exit jump to point here
  patchJump(exitJump);
  emitByte(OP_POP); // Pop condition value in exit case
}

// skip tokens until we reach something that looks like a statement boundary
static void synchronize() {
  parser.panicMode = false;

  while (parser.current.type != TOKEN_EOF) {
    // look for a preceding token that can end a statement, like a semicolon
    if (parser.previous.type == TOKEN_SEMICOLON)
      return;

    // look for a subsequent token that begins a statement
    // usually one of the control flow or declaration keywords
    switch (parser.current.type) {
    case TOKEN_CLASS:
    case TOKEN_FUNC:
    case TOKEN_VAR:
    case TOKEN_FOR:
    case TOKEN_IF:
    case TOKEN_WHILE:
    case TOKEN_PRINT:
    case TOKEN_RETURN:
      return;

    default:; // Do nothing.
    }

    advance();
  }
}

static void declaration() {
  if (match(TOKEN_FUNC)) {
    printf("Matched FUNC\n");
    funcDeclaration();
  } else if (match(TOKEN_VAR)) {
    printf("Matched VAR\n");
    varDeclaration();
  } else {
    statement();
  }

  if (parser.panicMode)
    synchronize();
}

static void statement() {
  if (match(TOKEN_PRINT)) {
    printStatement();
  } else if (match(TOKEN_FOR)) {
    forStatement();
  } else if (match(TOKEN_IF)) {
    ifStatement();
  } else if (match(TOKEN_WHILE)) {
    whileStatement();
  } else if (match(TOKEN_LEFT_BRACE)) {
    beginScope();
    block();
    endScope();
  } else {
    expressionStatement();
  }
}

static void grouping(bool canAssign) {
  // for backend, there is nothing to a grouping expression
  // Its sole function is syntactic, letting you insert a lower-precedence
  // expression where a higher precedence is expected. no runtime semantic on
  // its own and doesn't emit any bytecode
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool canAssign) {
  // `strtod`'s 2nd param EndPtr points to the location after the number
  // not needed here, as we will scan and parse manually
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

// Compiles the '||' logical OR operator using short-circuiting
static void or_(bool canAssign) {
  // First jump: if condition is false, jump to evaluate second operand
  int elseJump = emitJump(OP_JUMP_IF_FALSE);

  // Second jump: if condition is true, skip second operand
  int endJump = emitJump(OP_JUMP);

  // Patch the first jump to point here (where second operand evaluation
  // begins)
  patchJump(elseJump);

  // Pop the false value before evaluating second operand
  emitByte(OP_POP);

  // Compile the right operand
  parsePrecedence(PREC_OR);

  // Patch the second jump to point here (after second operand)
  patchJump(endJump);
}

static void string(bool canAssign) {
  // +1 and -2: trim the leading and trailing quotation marks
  // note: Lox don't support string escape sequence ('\n'), which could be
  // processed here

  // note: as this is an interpreter, parsing, code-gen and vm share the same
  // heap therefore a pointer into heap is enough
  emitConstant(OBJ_VAL(
      copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

static void namedVariable(Token name, bool canAssign) {
  uint8_t getOp, setOp;
  int arg = resolveLocal(current, &name);
  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else {
    arg = identifierConstant(&name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  // look for an equal sign after identifier, to determine whether this is a
  // set or a get
  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    emitBytes(setOp, (uint8_t)arg);
  } else {
    emitBytes(getOp, (uint8_t)arg);
  }
}

static void variable(bool canAssign) {
  namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign) {
  TokenType operatorType = parser.previous.type;

  // Compile the operand. (with correct precedence level limit)
  parsePrecedence(PREC_UNARY);

  // Emit the operator instruction.
  switch (operatorType) {
  case TOKEN_BANG:
    emitByte(OP_NOT);
    break;
  case TOKEN_MINUS:
    emitByte(OP_NEGATE);
    break;
  default:
    return; // Unreachable;
  }
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, call, PREC_NONE},
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, NULL, PREC_NONE},
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_BANG] = {unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, and_, PREC_AND},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUNC] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, or_, PREC_OR},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {NULL, NULL, PREC_NONE},
    [TOKEN_THIS] = {NULL, NULL, PREC_NONE},
    [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};

// start at the current token, and parses any expression at the given
// precedence level or higher refer to Section 17.6.1 for details
static void parsePrecedence(Precedence precedence) {
  advance();
  // first token is always going to belong to some kind of prefix expression,
  // by definition
  ParseFn prefixRule = getRule(parser.previous.type)->prefix;
  if (prefixRule == NULL) {
    error("Expect expression.");
    return;
  }

  // only allow assignment when parsing an assignment expression
  // or top-level expression like in an expression statement
  // fixes `a * b = c + d`, see Section 21.4
  // if the variable is nested in higher precedence expression, `canAssign`
  // will be false, and `=` will be ignored
  bool canAssign = precedence <= PREC_ASSIGNMENT;
  prefixRule(canAssign);

  // prefix expression parse done, now look for an infix parser
  // if found, the prefix expression is might be its left operand
  // but only when `precedence` is low enough to permit that infix operator
  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule(canAssign);
  }

  // `=` doesn't get consumed, indicating in further parsing the precedence
  // isn't low enough (See section 21.4) if `canAssign` is true, and
  // assignment is allowed, `=` should get consumed
  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignment target.");
  }
}

// wrap lookup in a function
// exists solely to handle a declaration cycle in the C code
static ParseRule *getRule(TokenType type) { return &rules[type]; }

// Main compilation function that turns source code into a function object
ObjFunction *compile(const char *source) {
  // Initialize the scanner with source code
  initScanner(source);

  // Set up a compiler for top-level script code
  Compiler compiler;
  initCompiler(&compiler, TYPE_SCRIPT);

  // Reset parser error state
  parser.hadError = false;
  parser.panicMode = false;

  // Prime the parser with first token
  advance();

  // Main compilation loop:
  // Keep processing declarations until we hit end of file
  while (!match(TOKEN_EOF)) {
    declaration();
  }

  // Finish compilation and get resulting function
  ObjFunction *function = endCompiler();

  // Return NULL if there were any errors, otherwise return the compiled
  // function
  return parser.hadError ? NULL : function;
}
