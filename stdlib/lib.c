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

int64_t readInt(void)
{
    int64_t ret;
    scanf("%ld", &ret);
    return ret;
}

const char * __add_strings(const char * sz1, const char * sz2)
{
    const size_t l1 = strlen(sz1);
    const size_t l2 = strlen(sz2);
    char * ret = (char*)malloc(l1 + l2 + 1);

    memcpy(ret, sz1, l1);
    memcpy(ret + l1, sz2, l2);
    ret[l1 + l2] = '\0';

    return ret;
}

void printString(const char * sz)
{
    puts(sz);
}

const char * readString(void)
{
    size_t bufSize = 16;
    size_t occupiedSize = 0;
    char * ret = (char*)malloc(bufSize);

    while (1) {
        int c = getchar();
        if (c == EOF || c == '\n') {
            ret[occupiedSize] = '\0';
            break;
        }
        else {
            ret[occupiedSize] = (char)c;
        }

        occupiedSize++;
        if (occupiedSize >= bufSize) {
            bufSize *= 2;
            char * nu = (char*)realloc(ret, bufSize);
            if (nu == NULL) {
                // Tough luck, return what we have
                ungetc(c, stdin);
                ret[occupiedSize - 1] = '\0';
                break;
            }

            ret = nu;
        }
    }

    return ret;
}

