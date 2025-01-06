#include <stdio.h>

#include "debug.h"
#include "value.h"

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

//Prints name if the opcode and increments the next byte offset
static int simpleInstruction(const char* name, int offset) {
    printf("%s\n", name);
    return offset + 1;
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset + 1];
    /* Print name of opcode, and pull the constant index from the
     * subsequent byte in the chunk*/
    printf("%-16s %4d '", name, constant);
    /* Print the value of the constant, which are known at compile time*/
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    /*move offset by two, as OP_CONSTANT is two bytes,
     *one for the opcode, and one for the operand*/
    return offset + 2;
}


 int disassembleInstruction(Chunk* chunk, int offset) {
    //Prints byte offset of the instruction
    printf("%04d ", offset);
    if (offset > 0 && chunk->lines[offset] == chunk->lines[offset-1]) {
        /* Print | for every instruction that came from the same
         * source line as the preceding one. */
        printf(" | ");
    } else {
        printf("%4d ", chunk->lines[offset]);
    }

    //Reads single byte from the bytecode at given offset
    uint8_t instruction = chunk->code[offset];

    /* We switch on the given instruction, printing any bug
     * in the compiler*/
    switch (instruction) {
        case OP_RETURN:
            return simpleInstruction("OP_RETURN", offset);
        case OP_CONSTANT:
            return constantInstruction("OP_CONSTANT", chunk, offset);
        case OP_ADD:
            return simpleInstruction("OP_ADD", offset);
        case OP_SUBTRACT:
            return simpleInstruction("OP_SUBTRACT", offset);
        case OP_MULTIPLY:
            return simpleInstruction("OP_MULTIPLY", offset);
        case OP_DIVIDE:
            return simpleInstruction("OP_DIVIDE", offset);
        case OP_NEGATE:
            return simpleInstruction("OP_NEGATE", offset);
        default:
            printf("Unknown opcode %d\n", instruction);
        return offset + 1;
    }
}