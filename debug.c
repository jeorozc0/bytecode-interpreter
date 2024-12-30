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
 int disassembleInstruction(Chunk* chunk, int offset) {
    //Prints byte offset of the instruction
    printf("%04d ", offset);

    //Reads single byte from the bytecode at given offset
    uint8_t instruction = chunk->code[offset];

    /* We switch on the given instruction, printing any bug
     * in the compiler*/
    switch (instruction) {
        case OP_RETURN:
            return simpleInstruction("OP_RETURN", offset);
        default:
            printf("Unknown opcode %d\n", instruction);
        return offset + 1;
    }
}