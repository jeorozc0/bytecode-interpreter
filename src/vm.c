#include "vm.h"
#include "chunk.h"
#include "common.h"

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/_types/_u_int16_t.h>
#include <sys/_types/_va_list.h>
#include <time.h>

#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

VM vm;

static Value clockNative(int argCount, Value *args) {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static void resetStack() {
  vm.stackTop = vm.stack;
  vm.frameCount = 0;
}

static void runtimeError(const char *format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  for (int i = vm.frameCount - 1; i >= 0; i--) {
    CallFrame *frame = &vm.frames[i];
    ObjFunction *function = frame->function;
    size_t instruction = frame->ip - function->chunk.code - 1;
    fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
    if (function->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%s()\n", function->name->chars);
    }
  }

  resetStack();
}

static void defineNative(const char *name, NativeFn function) {
  // Create a string object for the native function's name.
  push(OBJ_VAL(copyString(name, (int)strlen(name))));
  // Create a native function object wrapping the C function pointer.
  push(OBJ_VAL(newNative(function)));
  // Store the native function in the global variable table, associating
  // the function name with the native function object.
  tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
  // Pop the name and the function object from the stack.  These were
  // only pushed to protect them from garbage collection during the
  // tableSet operation.
  pop();
  pop();
}

void initVM() {
  resetStack();
  vm.objects = NULL;
  initTable(&vm.globals);
  initTable(&vm.strings);
}

void freeVM() {
  freeTable(&vm.globals);
  freeTable(&vm.strings);
  freeObjects();
  freeObjects();
}

void push(Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}

Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}

static Value peek(int distance) { return vm.stackTop[-1 - distance]; }

// Sets up a new call frame for a function invocation
static bool call(ObjFunction *function, int argCount) {
  // Check if correct amount of arguments are passed to function.
  if (argCount != function->arity) {
    runtimeError("Expected %d arguments but got %d.", function->arity,
                 argCount);
    return false;
  }

  if (vm.frameCount == FRAMES_MAX) {
    // Check if the call stack has reached its maximum allowed size.
    runtimeError("Stack overflow.");
    return false;
  }
  // Get the next available call frame
  CallFrame *frame = &vm.frames[vm.frameCount++];

  // Initialize the frame with function data
  frame->function = function;       // Function to execute
  frame->ip = function->chunk.code; // Start at first instruction

  // Stack slot setup:
  // vm.stackTop - argCount - 1 points to the function object itself
  // -argCount skips past the arguments
  // -1 moves past the function object to get to first slot
  frame->slots = vm.stackTop - argCount - 1;

  return true;
}

// Handles runtime function invocation with type checking
static bool callValue(Value callee, int argCount) {
  // Only objects can be called
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
    case OBJ_FUNCTION:
      // If it's a function, set up call frame and execute it
      return call(AS_FUNCTION(callee), argCount);
    case OBJ_NATIVE: {
      // Handle calls to native (C) functions.
      NativeFn native = AS_NATIVE(callee); // Get the native function pointer.
      // Call the native function with the given arguments.
      Value result = native(argCount, vm.stackTop - argCount);
      // Pop the arguments and the function itself from the stack.
      vm.stackTop -= argCount + 1;
      // Push the result of the native function call onto the stack.
      push(result);
      return true; // Indicate successful execution.
    }
    default:
      break; // Other object types aren't callable yet
    }
  }

  // Error if trying to call a non-function value
  runtimeError("Can only call functions and classes.");
  return false;
}

static bool isFalsey(Value value) {
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
  ObjString *b = AS_STRING(pop());
  ObjString *a = AS_STRING(pop());

  int length = a->length + b->length;
  char *chars = ALLOCATE(char, length + 1);
  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  ObjString *result = takeString(chars, length);
  push(OBJ_VAL(result));
}

static InterpretResult run() {
  CallFrame *frame = &vm.frames[vm.frameCount - 1];

#define READ_BYTE() (*frame->ip++)
#define READ_SHORT()                                                           \
  (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_CONSTANT() (frame->function->chunk.constants.values[READ_BYTE()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op)                                               \
  do {                                                                         \
    if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) {                          \
      runtimeError("Operands must be numbers.");                               \
      return INTERPRET_RUNTIME_ERROR;                                          \
    }                                                                          \
    double b = AS_NUMBER(pop());                                               \
    double a = AS_NUMBER(pop());                                               \
    push(valueType(a op b));                                                   \
  } while (false)

  for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
    printf("          ");
    for (Value *slot = vm.stack; slot < vm.stackTop; slot++) {
      printf("[ ");
      printValue(*slot);
      printf(" ]");
    }
    printf("\n");
    disassembleInstruction(&frame->function->chunk,
                           (int)(frame->ip - frame->function->chunk.code));
#endif

    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
    case OP_CONSTANT: {
      Value constant = READ_CONSTANT();
      push(constant);
      break;
    }
    case OP_NIL:
      push(NIL_VAL);
      break;
    case OP_TRUE:
      push(BOOL_VAL(true));
      break;
    case OP_FALSE:
      push(BOOL_VAL(false));
      break;
    case OP_POP:
      pop();
      break;
    case OP_GET_LOCAL: {
      uint8_t slot = READ_BYTE();
      push(frame->slots[slot]);
      break;
    }
    case OP_SET_LOCAL: {
      uint8_t slot = READ_BYTE();
      frame->slots[slot] = peek(0);
      break;
    }
    case OP_DEFINE_GLOBAL: {
      ObjString *name = READ_STRING();
      tableSet(&vm.globals, name, peek(0));
      pop();
      break;
    }
    case OP_GET_GLOBAL: {
      ObjString *name = READ_STRING();
      Value value;
      if (!tableGet(&vm.globals, name, &value)) {
        runtimeError("Undefined variable '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }
      push(value);
      break;
    }
    case OP_SET_GLOBAL: {
      ObjString *name = READ_STRING();
      if (tableSet(&vm.globals, name, peek(0))) {
        tableDelete(&vm.globals, name);
        runtimeError("Undefined variable: '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    }
    case OP_EQUAL: {
      Value b = pop();
      Value a = pop();
      push(BOOL_VAL(valuesEqual(a, b)));
      break;
    }
    case OP_GREATER:
      BINARY_OP(BOOL_VAL, >);
      break;
    case OP_LESS:
      BINARY_OP(BOOL_VAL, <);
      break;
    case OP_ADD:
      if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
        concatenate();
      } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
        double b = AS_NUMBER(pop());
        double a = AS_NUMBER(pop());
        push(NUMBER_VAL(a + b));
      } else {
        runtimeError("Operands must be two numbers or two "
                     "strings.");
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    case OP_SUBTRACT:
      BINARY_OP(NUMBER_VAL, -);
      break;
    case OP_MULTIPLY:
      BINARY_OP(NUMBER_VAL, *);
      break;
    case OP_DIVIDE:
      BINARY_OP(NUMBER_VAL, /);
      break;
    case OP_NOT:
      push(BOOL_VAL(isFalsey(pop())));
      break;
    case OP_NEGATE:
      if (!IS_NUMBER(peek(0))) {
        runtimeError("Operand must be a number.");
        return INTERPRET_RUNTIME_ERROR;
      }
      push(NUMBER_VAL(-AS_NUMBER(pop())));
      break;
    case OP_PRINT: {
      printValue(pop());
      printf("\n");
      break;
    }
    case OP_JUMP: {
      u_int16_t offset = READ_SHORT();
      frame->ip += offset;
      break;
    }
    case OP_JUMP_IF_FALSE: {
      u_int16_t offset = READ_SHORT();
      if (isFalsey(peek(0)))
        frame->ip += offset;
      break;
    }
    case OP_LOOP: {
      u_int16_t offset = READ_SHORT();
      frame->ip -= offset;
      break;
    }
    case OP_CALL: {
      int argCount = READ_BYTE();
      if (!callValue(peek(argCount), argCount)) {
        return INTERPRET_RUNTIME_ERROR;
      }
      frame = &vm.frames[vm.frameCount - 1];
      break;
    }
    case OP_RETURN: {
      Value result = pop();
      vm.frameCount--;
      if (vm.frameCount == 0) {
        pop();
        return INTERPRET_OK;
      }
      vm.stackTop = frame->slots;
      push(result);
      frame = &vm.frames[vm.frameCount - 1];
      break;
    }
    }
  }
#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

// Main interpreter function - compiles and executes source code
InterpretResult interpret(const char *source) {
  // Compile source code into a function object
  ObjFunction *function = compile(source);
  // If compilation fails, return a compile error
  if (function == NULL)
    return INTERPRET_COMPILE_ERROR;

  // Push function onto the stack to prevent garbage collection
  // The VM manages a stack of values, and the garbage collector
  // needs to know what values are still in use. Pushing the
  // function onto the stack ensures that it won't be prematurely
  // collected.
  push(OBJ_VAL(function));

  // Call the compiled function with 0 arguments
  // This initiates the execution of the compiled code.
  call(function, 0);

  // Run the VM
  // This starts the virtual machine, which executes the bytecode
  // generated by the compiler.
  return run();
}
