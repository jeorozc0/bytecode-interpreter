#include <stdio.h>
#include <string.h>
#include <sys/_types/_u_int32_t.h>
#include <sys/_types/_u_int8_t.h>

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

static ObjString *allocateString(char *chars, int length, u_int32_t hash) {
  ObjString *string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
  string->length = length;
  string->chars = chars;
  string->hash = hash;
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

ObjString *takeString(char *chars, int length) {
  u_int32_t hash = hashString(chars, length);
  return allocateString(chars, length,
                        hash); // Claims ownership of the string given,
                               // as it was already copied.
}

ObjString *copyString(const char *chars, int length) {
  u_int32_t hash = hashString(chars, length);
  char *heapChars = ALLOCATE(char, length + 1);
  memcpy(heapChars, chars, length);
  heapChars[length] = '\0';
  return allocateString(heapChars, length, hash);
}

void printObject(Value value) {
  switch (OBJ_TYPE(value)) {
  case OBJ_STRING:
    printf("%s", AS_CSTRING(value));
    break;
  }
}
