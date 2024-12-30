#include "chunk.h"
#include "debug.h"

int main(int argc, const char * argv[]) {
    Chunk chunk;
    initChunk(&chunk);
    writeChunk(&chunk, OP_RETURN);

    //Given a chunk, this will print out the instructions
    disassembleChunk(&chunk, "test chunk");
    freeChunk(&chunk);
    return 0;
}