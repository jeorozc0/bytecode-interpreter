#ifndef clox_table_h
#define clox_table_h

#include "common.h"
#include "value.h"
#include <sys/_types/_u_int32_t.h>

typedef struct {
  ObjString *key;
  Value value;
} Entry;

typedef struct {
  int count;
  int capacity;
  Entry *entries;
} Table;

void initTable(Table *table);
void freeTable(Table *table);
bool tableSet(Table *table, ObjString *key, Value value);
bool tableGet(Table *table, ObjString *key, Value *value);
bool tableDelete(Table *table, ObjString *key);
void tableAddAll(Table *from, Table *to);
ObjString *tableFindString(Table *table, const char *chars, int length,
                           u_int32_t hash);

#endif // clox_table_h
