#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

void *reallocate(void *pointer, size_t oldSize, size_t newSize) {
  if (newSize == 0) {
    free(pointer);
    return NULL;
  }
  void *result = realloc(pointer, newSize);
  if (result == NULL)
    exit(1);
  return result;
}

// Frees memory for different types of objects based on their type
static void freeObject(Obj *object) {
  switch (object->type) {
  case OBJ_STRING: {
    ObjString *string = (ObjString *)object;
    // Free the character array (adding 1 for null terminator)
    FREE_ARRAY(char, string->chars, string->length + 1);
    // Free the string object itself
    FREE(ObjString, object);
    break;
  }
  case OBJ_FUNCTION: {
    ObjFunction *function = (ObjFunction *)object;
    // Free the function's bytecode chunk
    freeChunk(&function->chunk);
    // Free the function object itself
    FREE(ObjFunction, object);
    break;
  }
  }
}

void freeObjects() {
  Obj *object = vm.objects;
  while (object != NULL) {
    Obj *next = object->next;
    freeObject(object);
    object = next;
  }
}
