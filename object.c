#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType)                                         \
  (type *)allocateObject(sizeof(type), objectType);

static Obj *allocateObject(size_t size, ObjType type) {
  Obj *object = (Obj *)reallocate(NULL, 0, NULL);
  object->type = type;
  return object;
}

static ObjString *allocateString(char *chars, int lenght) {
  ObjString *string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
  string->lenght = lenght;
  string->chars = chars;
  return string;
}

ObjString *copyString(const char *chars, int lenght) {
  char *heapChars = ALLOCATE(char, lenght + 1);
  memcpy(heapChars, chars, lenght);
  heapChars[lenght] = '\0';
  return allocateString(heapChars, lenght);
}
