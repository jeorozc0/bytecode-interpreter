#include "table.h"
#include "memory.h"
#include "object.h"
#include "value.h"
#include <stdlib.h>
#include <string.h>

void initTable(Table *table) {
  table->count = 0;
  table->capacity = 0;
  table->entries = NULL;
}
