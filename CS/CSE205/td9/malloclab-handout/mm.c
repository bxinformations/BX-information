/*
 * mm-naive.c - The fastest, least memory-efficient malloc package.
 * 
 * I use segregated free list and best fit to do this.
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>

#include "mm.h"
#include "memlib.h"

/*********************************************************
 * NOTE TO STUDENTS: Before you do anything else, please
 * provide your team information in the following struct.
 ********************************************************/
team_t team = {
    /* Team name */
    "ateam",
    /* First member's full name */
    "Peixin YOU",
    /* First member's email address */
    "peixin.you@polytechnique.edu",
    /* Second member's full name (leave blank if none) */
    "",
    /* Second member's email address (leave blank if none) */
    ""
};

static char *heap_listp = 0;
static unsigned int *segregate_starter = 0; // Pointer to first one of segregated list

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define PACK(size, alloc)  ((size) | (alloc))

/* single word (4) or double word (8) alignment */
#define ALIGNMENT 8

#define WSIZE 4
#define DSIZE 8
#define CHUNKSIZE (1<<12)

/* rounds up to the nearest multiple of ALIGNMENT */
#define ALIGN(size) (((size) + (ALIGNMENT - 1)) & ~0x7)

#define GET(p) (*(unsigned int *)(p))
#define PUT(p, val) (*(unsigned int *)(p) = (val))

/*return the size from p*/
#define GET_SIZE(p) (GET(p) & ~0x7)
#define GET_ALLOC(p) (GET(p) & 0x1)

#define PACK(size,alloc)    ((size) | (alloc))

/*for a given block retuen the head or tail*/
#define HDRP(bp) ((char *)(bp) - WSIZE)
#define FTRP(bp) ((char *)(bp) + GET_SIZE(HDRP(bp)) - DSIZE)

#define NEXT_BLKP(bp) ((char *)(bp) + GET_SIZE(((char *)(bp) - WSIZE)))
#define PREV_BLKP(bp) ((char *)(bp) - GET_SIZE(((char *)(bp) - DSIZE)))

#define PREV_LINKNODE_RP(bp) ((char*)(bp))
#define NEXT_LINKNODE_RP(bp) ((char*)(bp) + WSIZE)

#define GET_PREV(p) (GET(p))	  /* Just a alias of former GET */
#define PUT_PREV(p, val) (PUT(p, val))  /* Alias of former PUT */
#define GET_SUCC(p) (*((unsigned int *)p + 1))
#define PUT_SUCC(p, val) (*((unsigned int *)p + 1) = (unsigned int)(val))

/*
 * find_segregate - find the suitable segregated group based on size
 */
static void *find_segregate(size_t size)
{
    
    if (size <= 16) 
        return segregate_starter;
    if (size <= 32) 
        return segregate_starter + 1;
    if (size <= 64)
        return segregate_starter + 2;
    if (size <= 128)
        return segregate_starter + 3;
    if (size <= 256)
        return segregate_starter + 4;
    if (size <= 512)
        return segregate_starter + 5;
    if (size <= 1024)
        return segregate_starter + 6;
    if (size <= 2048)
        return segregate_starter + 7;
    if (size <= 4096)
        return segregate_starter + 8;
    return segregate_starter + 9;
}

/*
 * here we use the technique introduce in CSAPP 9.9.13
 */

/*
 * chain_segregate - chain a free blk to a specified starter
 * based on its size
 */
void chain_segregate(void* bp, size_t size)
{
    unsigned int *starter;
    starter = find_segregate(size);
    unsigned int segregate_succ_free = *starter;
    PUT_PREV(bp, starter);
    *starter = (unsigned int)bp;
    PUT_SUCC(bp, segregate_succ_free);
    if(segregate_succ_free) PUT_PREV(segregate_succ_free, bp);
}

/*
 * chain_prevnext - chain the free prev and next of a free blk
 */
void chain_prevnext(void *bp)
{
    unsigned int succ_free, *prev_free;
    succ_free = GET_SUCC(bp);
    prev_free = (unsigned int *)GET_PREV(bp);
    if((segregate_starter + 9) >= prev_free) *prev_free = succ_free;
    else PUT_SUCC(prev_free, succ_free);
    if(succ_free) PUT_PREV(succ_free, prev_free);
}

/*
 * coalesce - Boundary tag coalescing. Return ptr to coalesced block
 */
static void *coalesce(void *bp)
{
    void *prev_bp = PREV_BLKP(bp), *next_bp = NEXT_BLKP(bp);
    size_t prev_alloc = GET_ALLOC(FTRP(prev_bp));
    size_t next_alloc = GET_ALLOC(HDRP(next_bp));
    size_t size = GET_SIZE(HDRP(bp));

    if (prev_alloc && next_alloc) 
    {
        chain_segregate(bp, size);
    }

    if (prev_alloc && !next_alloc) 
    {
        size += GET_SIZE(HDRP(next_bp));
        PUT(HDRP(bp), PACK(size, 0));
        PUT(FTRP(bp), PACK(size,0));
        chain_prevnext(next_bp);
        chain_segregate(bp, size);
    }

    if (!prev_alloc && next_alloc) 
    {
        size += GET_SIZE(HDRP(prev_bp));
        bp = prev_bp;
        PUT(HDRP(bp), PACK(size, 0));
        PUT(FTRP(bp), PACK(size, 0));
        chain_prevnext(bp);
        chain_segregate(bp, size);
    }

    if (!prev_alloc && !next_alloc) 
    {
        size += GET_SIZE(HDRP(prev_bp)) +
        GET_SIZE(FTRP(next_bp));
        bp = prev_bp;
        PUT(HDRP(bp), PACK(size, 0));
        PUT(FTRP(bp), PACK(size, 0));
        chain_prevnext(next_bp);
        chain_prevnext(prev_bp);
        chain_segregate(bp, size);
    }
    return bp;
}

/*
 * extend_heap - Extend heap with free block and return its block pointer
 */
static void *extend_heap(size_t words)
{
    char *bp;
    size_t size;

    size = (words % 2) ? (words+1) * WSIZE : words * WSIZE;
    if ((long)(bp = (char *)mem_sbrk(size)) == -1)
        return NULL;

    PUT(HDRP(bp), PACK(size, 0));
    PUT(FTRP(bp), PACK(size, 0));
    PUT(HDRP(NEXT_BLKP(bp)), PACK(0, 1));
    
    return coalesce(bp);
}

/* 
 * mm_init - initialize the malloc package.
 * we create a pointer who points to the second heap byte.
 * where will stores 0 if there is no prev free block
 * and the next word stores the adress of the suc free block
 * And in order to avoid waste space, we only store the pointer whoes suc block is free
 */

int mm_init(void)
{
    void *bp;
    mem_init();

    /* Create the initial empty heap */
    if ((heap_listp = (char *)mem_sbrk(14 * WSIZE)) == (char *)-1)
        return -1;

    segregate_starter = (unsigned int *)heap_listp;

    PUT(heap_listp, 0);              // free list, block size <=16
    PUT(heap_listp + (1 * WSIZE), 0);    // etc
    PUT(heap_listp + (2 * WSIZE), 0);    
    PUT(heap_listp + (3 * WSIZE), 0);
    PUT(heap_listp + (4 * WSIZE), 0);
    PUT(heap_listp + (5 * WSIZE), 0);
    PUT(heap_listp + (6 * WSIZE), 0);
    PUT(heap_listp + (7 * WSIZE), 0);
    PUT(heap_listp + (8 * WSIZE), 0);
    PUT(heap_listp + (9 * WSIZE), 0);
    PUT(heap_listp + (10 * WSIZE), 0);
    PUT(heap_listp + (11 * WSIZE), PACK(DSIZE, 1));
    PUT(heap_listp + (12 * WSIZE), PACK(DSIZE, 1));
    PUT(heap_listp + (13 * WSIZE), PACK(0, 1));
    heap_listp += (12 * WSIZE);
    
    bp = extend_heap(2 * DSIZE / WSIZE);

#ifdef DEBUG
    mm_check(1);
#endif // DEBUG

    if (bp == NULL)
        return -1;
    return 0;

}

/*
 * find_fit - Find a fit for a block with asize bytes
 * First-fit search
 */
static void *find_fit(size_t asize)
{
    unsigned int *bp, *starter, *maxstarter = segregate_starter + 9;
    
    for (starter = find_segregate(asize); starter <= maxstarter; starter += 1)
        for (bp = (unsigned int *)(*starter); bp != 0; bp = (unsigned int *)GET_SUCC(bp))
            if (asize <= GET_SIZE(HDRP(bp)))
                return (void *)bp;

    return NULL;
}

/*
 * place - Place block of asize bytes at start of free block bp
 *         and split if remainder would be at least minimum block size
 */
static void place(void *bp, size_t asize)
{
    size_t csize = GET_SIZE(HDRP(bp));

    chain_prevnext(bp);

    if ((csize - asize) >= (2*DSIZE)) 
    {
        PUT(HDRP(bp), PACK(asize, 1));
        PUT(FTRP(bp), PACK(asize, 1));
        bp = NEXT_BLKP(bp);
        PUT(HDRP(bp), PACK(csize-asize, 0));
        PUT(FTRP(bp), PACK(csize-asize, 0));
        coalesce(bp);
    }
    else {
        PUT(HDRP(bp), PACK(csize, 1));
        PUT(FTRP(bp), PACK(csize, 1));
    }
}

/* 
 * mm_malloc - Allocate a block by incrementing the brk pointer.
 *     Always allocate a block whose size is a multiple of the alignment.
 */
void *mm_malloc(size_t size)
{
    size_t aSize;
    size_t extendSize; //if not fit the size need to be extended
    char *bp;

    if (heap_listp == 0)
        mm_init();

    if (size == 0)
        return NULL;
    
    if (size <= DSIZE)
        aSize = 2 * DSIZE;
    else
        aSize = DSIZE * ((size + (DSIZE) + (DSIZE-1)) / DSIZE);

    if ((bp = (char *)find_fit(aSize)) != NULL) 
    {
        place(bp, aSize);

#ifdef DEBUG
    mm_check(1);
#endif // DEBUG

        return bp;
    }

    extendSize = MAX(aSize,CHUNKSIZE);
    if ((bp = (char *)extend_heap(extendSize/WSIZE)) == NULL) 
        return NULL;
    place(bp, aSize);
    return bp;
}

/*
 * mm_free - Freeing a block does nothing.
 */

void mm_free(void *ptr)
{
    if(ptr == 0)
        return;
    size_t size = GET_SIZE(HDRP(ptr));
    if (heap_listp == 0) 
        mm_init();

    PUT(HDRP(ptr), PACK(size, 0));
    PUT(FTRP(ptr), PACK(size, 0));
    coalesce(ptr);
}

/*
 * mm_realloc - Implemented simply in terms of mm_malloc and mm_free
 * we should consider the size of initial block and the realloc block.
 * so we add two new functions
 * mm_realloc_place : ptr is not free. Than we don't need to consider the chain relation
 * mm_realloc_coalesce : When we combine two free block we should first check if the size is
 *                      equal to the size we need or not. If it is yes, then we need the block ofter
 *                      the combination. Than we need to use it imidiately.
 */

static void mm_realloc_place(void *bp, size_t aSize)
{
    size_t cSize = GET_SIZE(HDRP(bp));

    if ((cSize - aSize) >= (2*DSIZE))
    {
        PUT(HDRP(bp), PACK(aSize, 1));
        PUT(FTRP(bp), PACK(aSize, 1));
        bp = NEXT_BLKP(bp);
        PUT(HDRP(bp), PACK(cSize - aSize, 0));
        PUT(FTRP(bp), PACK(cSize - aSize, 0));
        coalesce(bp);
    }
    else 
    {
        PUT(HDRP(bp), PACK(cSize, 1));
        PUT(FTRP(bp), PACK(cSize, 1));
    }
}

static void *mm_realloc_coalesce(void *bp, size_t aSize, int *flag)
{
    void *prev_bp = PREV_BLKP(bp);
    size_t preAlloc = GET_ALLOC(FTRP(prev_bp));
    void *next_bp = NEXT_BLKP(bp);
    size_t nexAlloc = GET_ALLOC(HDRP(next_bp));

    *flag = 0;

    size_t size = GET_SIZE(HDRP(bp));

    if (preAlloc && !nexAlloc)
    {
        size += GET_SIZE(HDRP(next_bp));
        if (size >= aSize)
        {
            PUT(HDRP(bp), PACK(size, 0));
	        PUT(FTRP(next_bp), PACK(size,0));
	        chain_prevnext(next_bp);
            *flag = 1;
        }
    }

    if (!preAlloc && nexAlloc)
    {
        size += GET_SIZE(HDRP(prev_bp));
        if (size >= aSize)
        {
	        PUT(FTRP(bp), PACK(size, 0));
	        PUT(HDRP(prev_bp), PACK(size, 0));
	        chain_prevnext(prev_bp);
	        bp = prev_bp;
        }
    }

    if (!preAlloc && !nexAlloc)
    {
        size += GET_SIZE(HDRP(prev_bp)) +
        GET_SIZE(FTRP(next_bp));
        if (size >= aSize)
        {
	        PUT(HDRP(prev_bp), PACK(size, 0));
	        PUT(FTRP(next_bp), PACK(size, 0));
	        chain_prevnext(next_bp);
	        chain_prevnext(prev_bp);
	        bp = prev_bp;
        }
    }

    return bp;
}

void *mm_realloc(void *ptr, size_t size)
{

#ifdef DEBUG
    mm_check(1);
#endif // DEBUG

    void *newptr;

    if (size == 0)
    {
        mm_free(ptr);
        return 0;
    }

    if (ptr == NULL)
    {
        return mm_malloc(size);
    }

    size_t oldSize = GET_SIZE(HDRP(ptr));
    size_t copySize = oldSize - DSIZE;
    size_t aSize;

    if (size <= DSIZE)
        aSize = 2 * (DSIZE);
    else aSize = (DSIZE)*((size + (DSIZE) + (DSIZE - 1)) / (DSIZE));
    
    int flag;

    if (oldSize == aSize)
        return ptr;
    else
        if (oldSize < aSize)
        {
            newptr = mm_realloc_coalesce(ptr, aSize, &flag);
            if (flag)
            {
                mm_realloc_place(newptr, aSize);
                return newptr;
            }
            else
                if (newptr != ptr)
                {
                    memmove(newptr, ptr, copySize);
                    mm_realloc_place(newptr, aSize);
                    return newptr;
                }
                else
                {
                    newptr = mm_malloc(aSize);
                    memmove(newptr, ptr, copySize);
                    mm_free(ptr);
                    return newptr;
                }
        }
        else
        {
            mm_realloc_place(ptr, aSize);
            return ptr;
        }
}

/*
 * checkblock - Check if a block is not doubleword algined
 * or header-footer not matched
 */
static void checkblock(void *bp)
{
    if ((size_t)bp % 8)
        printf("Error: %p is not doubleword aligned\n", bp);
    if (GET(HDRP(bp)) != GET(FTRP(bp)))
    {
        printf("Error: %p header does not match footer\n", bp);
    }
}

/*
 * checkchain - Check if the double linked list is linked in order
 */
static void checkchain(int verbose)
{
    unsigned int *bp, *starter, *prev_chain, *succ_chain, *maxstarter = segregate_starter + 9;
    
    for (starter = segregate_starter; starter <= maxstarter; starter += 1)
    {
        for (bp = (unsigned int *)(*starter); bp != 0; bp = succ_chain)
        {
            succ_chain = (unsigned int *)GET_SUCC(bp);
            if (succ_chain) 
                prev_chain = (unsigned int *)GET_PREV(succ_chain);
            else 
                prev_chain = bp;
            if (verbose)
            {
                printf("{%p} -> {%p}\n", bp, succ_chain);
                printf("{%p} <- {%p}\n", prev_chain, succ_chain);
            }
            if (prev_chain != bp)
                printf("%p, %p unmatch\n", prev_chain, bp);
        }
    }
}

void mm_check(int verbose)
{
    char *bp = heap_listp;

    checkchain(verbose);
    if (verbose)
        printf("Heap (%p):\n", heap_listp);

    if ((GET_SIZE(HDRP(heap_listp)) != DSIZE) || !GET_ALLOC(HDRP(heap_listp)))
        printf("Bad prologue header\n");
    checkblock(heap_listp);

    for (bp = heap_listp; GET_SIZE(HDRP(bp)) > 0; bp = NEXT_BLKP(bp)) 
    {
        checkblock(bp);
    }
    if ((GET_SIZE(HDRP(bp)) != 0) || !(GET_ALLOC(HDRP(bp))))
        printf("Bad epilogue header\n");
}