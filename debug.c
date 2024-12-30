#include <stdio.h>

#include "debug.h"

void disassembleChunk(Chunk* chunk, const char* name) {
    //Print header of chunk
    printf("== %s ==\n", name);

    //We loop through the bytecode, disassembling each instruction
    for (int offset = 0; offset < chunk->count;) {
        /* We let the function increment the offset of the next
         * instruction.  This is because instructions can have
         * different offsets*/
        offset = disassembleInstruction(chunk, offset);
    }
}