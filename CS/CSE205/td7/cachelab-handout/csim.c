#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>

#include "cachelab.h"


typedef unsigned long int uli;

typedef struct
{
    int valid;
    uli tag;
    int lru;
}Line;

typedef Line* Set;
typedef Set* Cache;

Cache cache;

int hits = 0;
int misses = 0;
int evictions = 0;

/* Globals set by command line args */
int verbosity = 0; /* print trace if set */
int s = 0;         /* set index bits */
int b = 0;         /* block offset bits */
int E = 0;         /* associativity */
char* trace_file = NULL;

/*
 * printUsage - Print usage info
 */
void printUsage(char* argv[])
{
    printf("Usage: %s [-hv] -s <num> -E <num> -b <num> -t <file>\n", argv[0]);
    printf("Options:\n");
    printf("  -h         Print this help message.\n");
    printf("  -v         Optional verbose flag.\n");
    printf("  -s <num>   Number of set index bits.\n");
    printf("  -E <num>   Number of lines per set.\n");
    printf("  -b <num>   Number of block offset bits.\n");
    printf("  -t <file>  Trace file.\n");
    printf("\nExamples:\n");
    printf("  linux>  %s -s 4 -E 1 -b 4 -t traces/yi.trace\n", argv[0]);
    printf("  linux>  %s -v -s 8 -E 2 -b 4 -t traces/yi.trace\n", argv[0]);
    exit(0);
}

int visCache(uli address)
{
    uli tag = (address) >> (s + b);
    unsigned int index = address >> b & ((1 << s) - 1);

    int f = -1;
    int evi = 0;
    Set set = cache[index];

    for (int i = 0; i < E; i++)
    {
        if (!set[i].valid)
        {
            f = i;
            continue;
        }
        if (set[i].tag == tag)
        {
            set[i].lru = 1;
            hits++;
            return 0;
        }

        set[i].lru ++;
        if (set[evi].lru <= set[i].lru)
            evi = i;
    }

    misses ++;
    if (f != -1)
    {
        set[f].valid = 1;
        set[f].tag = tag;
        set[f].lru = 1;
        return 1;
    }
    set[evi].tag = tag;
    set[evi].lru = 1;
    evictions ++;
    return 2;
}

/*
 * main - Main routine 
 */
int main(int argc, char* argv[])
{
    char c;
    
    while( (c=getopt(argc,argv,"s:E:b:t:vh")) != -1)
    {
        switch(c)
        {
        case 's':
            s = atoi(optarg);
            break;
        case 'E':
            E = atoi(optarg);
            break;
        case 'b':
            b = atoi(optarg);
            break;
        case 't':
            trace_file = optarg;
            break;
        case 'v':
            verbosity = 1;
            break;
        case 'h':
            printUsage(argv);
            exit(0);
        default:
            printUsage(argv);
            exit(1);
        }
    }

    /* Make sure that all required command line args were specified */
    if (s == 0 || E == 0 || b == 0 || trace_file == NULL) 
    {
        printf("%s: Missing required command line argument\n", argv[0]);
        printUsage(argv);
        exit(1);
    }

    int S = 1 << s;
    cache = (Cache)malloc(sizeof(Set) * S);

    if (cache == NULL)
        return -1;
    
    for (int i = 0; i < S; i++)
    {
        cache[i] = (Set)calloc(E, sizeof(Line));
        if (cache[i] == NULL)
            return  -1;
    }

    char opt[20];
    char f;
    uli address;
    int size;
    FILE* file;
    file = fopen(trace_file, "r");

    while (fgets(opt, sizeof(opt), file) != NULL)
    {
        int flag;

        if (opt[0] == 'I')
            continue;

        
        sscanf(opt, " %c %lx,%d", &f, &address, &size);
        if (f == 'M')
            hits ++;
        flag = visCache(address);

        if (verbosity)
        {
            switch (flag)
            {
            case 0:
                printf("%c %lx,%d hit\n", f, address, size);
                break;
            case 1:
                printf("%c %lx,%d miss\n", f, address, size);
                break;
            case 2:
                printf("%c %lx,%d miss eviction\n", f, address, size);
                break;
            }
        }
    }

    for (int i = 0; i < S; i++)
        free(cache[i]);
    free(cache);
    fclose(file);

    printSummary(hits, misses, evictions);
    return 0;
}
