#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void error()
{
    puts("runtime error");
    exit(1);
}

int64_t * __alloc_array(int64_t size)
{
    if (size < 0) {
        error();
    }

    int64_t * arr = (int64_t*)malloc((size + 1) * sizeof(int64_t));
    arr[0] = size;
    memset((void*)(arr + 1), 0, size * sizeof(int64_t));

    return arr;
}

void printInt(int64_t i)
{
    printf("%ld\n", i);
}

