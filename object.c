#include <stdio.h>
#include <string.h>
#include <sys/_types/_u_int32_t.h>
#include <sys/_types/_u_int8_t.h>

#include "chunk.h"
#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType)                                         \
  (type *)allocateObject(sizeof(type), objectType);

static Obj *allocateObject(size_t size, ObjType type) {
  Obj *object = (Obj *)reallocate(NULL, 0, size);
  object->type = type;

  object->next = vm.objects;
  vm.objects = object;
  return object;
}

// Creates and initializes a new function object
ObjFunction *newFunction() {
  // Allocate memory for function object and tag it as OBJ_FUNCTION type
  ObjFunction *function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);

  // Initialize function with no parameters (arity of 0)
  function->arity = 0;

  // Function starts with no name (anonymous function)
  function->name = NULL;

  // Initialize the function's bytecode chunk
  initChunk(&function->chunk);

  return function;
}

static ObjString *allocateString(char *chars, int length, u_int32_t hash) {
  ObjString *string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
  string->length = length;
  string->chars = chars;
  string->hash = hash;
  printf("DEBUG allocateString: created string=%p hash=%u chars='%s'\n",
         (void *)string, hash, chars);
  return string;
}

static u_int32_t hashString(const char *key, int length) {
  u_int32_t hash = 2166136261u;
  for (int i = 0; i < length; i++) {
    hash ^= (u_int8_t)key[i];
    hash *= 16777619;
  }
  return hash;
}

// claims ownership of `chars` passed in
ObjString *takeString(char *chars, int length) {
  uint32_t hash = hashString(chars, length);

  // if already exist a same string, return the reference to it
  ObjString *interned = tableFindString(&vm.strings, chars, length, hash);
  if (interned != NULL) {
    // already obtained ownership, no longer need the duplicate string
    FREE_ARRAY(char, chars, length + 1);
    return interned;
  }

  return allocateString(chars, length, hash);
}

// copy the characters into heap, so every ObjString owns its char array and can
// free it assumes it can not take ownership of `chars` passed in
ObjString *copyString(const char *chars, int length) {
  uint32_t hash = hashString(chars, length);

  // if already exist a same string, just return the reference to it, and skip
  // the copying
  ObjString *interned = tableFindString(&vm.strings, chars, length, hash);
  if (interned != NULL)
    return interned;

  char *heapChars = ALLOCATE(char, length + 1);
  memcpy(heapChars, chars, length);
  // make a null-terminated c-string for ease of use
  heapChars[length] = '\0';
  printf("DEBUG allocateString: created string=%p hash=%u chars='%s'\n",
         (void *)interned, hash, chars);
  return allocateString(heapChars, length, hash);
}

void printObject(Value value) {
  switch (OBJ_TYPE(value)) {
  case OBJ_STRING:
    printf("%s", AS_CSTRING(value));
    break;
  }
}
