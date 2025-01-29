#include "table.h"
#include "memory.h"
#include "object.h"
#include "value.h"
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <sys/_types/_u_int32_t.h>

#define TABLE_MAX_LOAD 0.75

void initTable(Table *table) {
  table->count = 0;
  table->capacity = 0;
  table->entries = NULL;
}

void freeTable(Table *table) {
  FREE_ARRAY(Entry, table->entries, table->capacity);
  initTable(table);
}

/**
 * Finds a hash table entry for a given key using linear probing.
 *
 * This is used for both inserting new entries and looking up existing ones.
 *
 * @param entries   Pointer to the start of the hash table entries array
 * @param capacity  Total size of the entries array
 * @param key       String key to search for
 * @return Entry*   Pointer to either:
 *                  - Existing entry with matching key (found case)
 *                  - First empty entry (NULL key) where new key can be inserted
 */
static Entry *findEntry(Entry *entries, int capacity, ObjString *key) {
  // Calculate initial bucket index using hash modulo capacity
  uint32_t index = key->hash % capacity;
  Entry *tombstone = NULL;

  for (;;) {
    Entry *entry = &entries[index];
    if (entry->key == NULL) {
      if (IS_NIL(entry->value)) {
        // Empty entry.
        return tombstone != NULL ? tombstone : entry;
      } else {
        // We found a tombstone.
        if (tombstone == NULL)
          tombstone = entry;
      }
    } else if (entry->key == key) {
      // We found the key.
      return entry;
    }

    // Collision resolution: linear probing with wrap-around
    // Move to next bucket (index + 1) and wrap around using modulo capacity
    index = (index + 1) % capacity;
  }
}

/**
 * Resizes the hash table to a new capacity and rehashes all existing entries.
 *
 * This function performs the following steps:
 * 1. Allocates a new array with the desired capacity
 * 2. Initializes all new entries to empty state
 * 3. Rehashes all existing entries into the new array
 * 4. Frees the old array
 *
 * @param table     Pointer to the hash table being resized
 * @param capacity  New desired capacity for the table
 */
static void adjustCapacity(Table *table, int capacity) {
  // Step 1: Allocate new array with desired capacity
  Entry *entries = ALLOCATE(Entry, capacity);

  // Step 2: Initialize all entries to empty state
  for (int i = 0; i < capacity; i++) {
    entries[i].key = NULL;      // Mark slot as empty
    entries[i].value = NIL_VAL; // Set default nil value
  }

  // Step 3: Rehash all existing entries into new array
  table->count = 0;
  for (int i = 0; i < table->capacity; i++) {
    Entry *entry = &table->entries[i];

    // Skip empty entries in old array
    if (entry->key == NULL) {
      continue;
    }

    // Find new location for entry in new array and copy data
    Entry *dest = findEntry(entries, capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
    table->count++;
  }

  // Step 4: Clean up and update table structure
  FREE_ARRAY(Entry, table->entries, table->capacity); // Free old array
  table->entries = entries;                           // Point to new array
  table->capacity = capacity;                         // Update capacity
}

/**
 * Inserts or updates a key-value pair in the hash table.
 *
 * This function handles:
 * 1. Dynamic resizing of the table when it gets too full
 * 2. Insertion of new entries
 * 3. Updating of existing entries
 *
 * @param table  Pointer to the hash table
 * @param key    String key to insert/update
 * @param value  Value to associate with the key
 * @return bool  true if a new key was inserted, false if an existing key was
 * updated
 */
bool tableSet(Table *table, ObjString *key, Value value) {
  // Check if table needs to grow (load factor threshold exceeded)
  if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
    // Calculate new capacity and resize the table
    int capacity = GROW_CAPACITY(table->capacity);
    adjustCapacity(table, capacity);
  }

  // Find the entry where this key belongs (either existing or empty slot)
  Entry *entry = findEntry(table->entries, table->capacity, key);

  // Track whether we're inserting a new key or updating an existing one
  bool isNewKey = entry->key == NULL;

  // Increment count only when inserting a new key
  if (isNewKey && IS_NIL(entry->value)) {
    table->count++;
  }

  // Update the entry with the new key-value pair
  entry->key = key;
  entry->value = value;

  // Return whether this was a new insertion
  return isNewKey;
}

/**
 * Retrieves a value from the hash table given a key.
 *
 * This function performs a lookup operation and returns the associated value
 * through the value pointer if the key is found.
 *
 * @param table  Pointer to the hash table to search in
 * @param key    String key to look up
 * @param value  Pointer to store the found value (if key exists)
 * @return bool  true if key was found, false if key doesn't exist
 */
bool tableGet(Table *table, ObjString *key, Value *value) {
  // Early exit if table is empty
  if (table->count == 0) {
    return false;
  }

  // Find the entry for this key using linear probing
  Entry *entry = findEntry(table->entries, table->capacity, key);

  // If key doesn't exist (found an empty slot), return false
  if (entry->key == NULL) {
    return false;
  }

  // Key found - store the value through the pointer
  *value = entry->value;
  return true;
}

/**
 * Removes an entry from the hash table given a key.
 *
 * This function implements deletion using tombstone markers to maintain
 * proper collision chain behavior in open addressing. A tombstone is an
 * entry marked as deleted (NULL key with true value) rather than never used
 * (NULL key with NIL_VAL value).
 *
 * @param table  Pointer to the hash table
 * @param key    String key to remove
 * @return bool  true if key was found and deleted, false if key didn't exist
 */
bool tableDelete(Table *table, ObjString *key) {
  // Early exit if table is empty - no deletion possible
  if (table->count == 0) {
    return false;
  }

  // Find the entry using linear probing
  Entry *entry = findEntry(table->entries, table->capacity, key);

  // Return false if key doesn't exist
  if (entry->key == NULL) {
    return false;
  }

  // Create a tombstone:
  // - Set key to NULL to mark as deleted
  // - Set value to true to indicate this is a tombstone
  // This distinguishes it from a never-used NULL slot (which has NIL_VAL)
  entry->key = NULL;
  entry->value = BOOL_VAL(true);

  // Note: table->count should probably be decremented here
  // table->count--;

  return true;
}

/**
 * Copies all entries from one hash table to another.
 *
 * This function iterates through the source table and copies all non-empty
 * entries to the destination table. If the destination table already contains
 * any of the keys, they will be updated with the new values.
 *
 * @param from  Source hash table to copy entries from
 * @param to    Destination hash table to copy entries to
 */
void tableAddAll(Table *from, Table *to) {
  // Iterate through all slots in the source table
  for (int i = 0; i < from->capacity; i++) {
    Entry *entry = &from->entries[i];

    // Only copy non-empty entries
    if (entry->key != NULL) {
      // Use tableSet to handle insertion and any needed resizing
      // in the destination table
      tableSet(to, entry->key, entry->value);
    }
  }
}
