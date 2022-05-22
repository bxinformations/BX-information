/* 
 * trans.c - Matrix transpose B = A^T
 *
 * Each transpose function must have a prototype of the form:
 * void trans(int M, int N, int A[N][M], int B[M][N]);
 *
 * A transpose function is evaluated by counting the number of misses
 * on a 1KB direct mapped cache with a block size of 32 bytes.
 */ 
#include <stdio.h>
#include "cachelab.h"

int is_transpose(int M, int N, int A[N][M], int B[M][N]);

/* 
 * transpose_submit - This is the solution transpose function that you
 *     will be graded on for Part B of the assignment. Do not change
 *     the description string "Transpose submission", as the driver
 *     searches for that string to identify the transpose function to
 *     be graded. 
 */
char transpose_submit_desc[] = "Transpose submission";

void transpose_submit(int M, int N, int A[N][M], int B[M][N])
{
    #define min(a, b) ((a) < (b) ? (a) : (b))
    int x, y, i, j;
    
    int a[8];

    for (x = 0; x < N; x += 8)
        for (y = 0; y < M; y += 8)
            if (x != y || x + 8 > min(N, M) || y + 8 > min(N, M))
            {
                for (j = y; j < min(M, y + 8); ++j)
                    a[j - y] = A[x][j];

                for (j = y; j < min(M, y + 4); ++j)
                    B[j][x] = a[j - y];

                for (i = x + 1; i < min(N, x + 8); ++i)
                {
                    for (j = y; j < min(M, y + 4); ++j)
                        a[j - y] = A[i][j];

                    for (j = y; j < min(M, y + 4); ++j)
                        B[j][i] = a[j - y];
                }

                y += 4;

                for (i = min(N, x + 8) - 1; i > x; --i)
                {
                    for (j = y; j < min(M, y + 4); ++j)
                        a[j - y] = A[i][j];

                    for (j = y; j < min(M, y + 4); ++j)
                        B[j][i] = a[j - y];
                }

                for (j = y; j < min(M, y + 4); ++j)
                    B[j][x] = a[4 + j - y];
                
                y -= 4;
            }
            else
            {
                a[0] = x; x = y; y = a[0];
                for (i = 0; i < 4; ++i)
                {
                    for (j = 0; j < 8; ++j)
                        a[j] = A[y + i][x + j];
                    for (j = 0; j < 8; ++j)
                        B[x + i][y + j] = a[j];
                }

                for (i = 0; i < 4; ++i)
                    for (j = i + 1; j < 4; ++j)
                    {
                        a[0] = B[x + i][y + j];
                        B[x + i][y + j] = B[x + j][y + i];
                        B[x + j][y + i] = a[0];
                    }

                y += 4;

                for (i = 0; i < 4; ++i)
                    for (j = i + 1; j < 4; ++j)
                    {
                        a[0] = B[x + i][y + j];
                        B[x + i][y + j] = B[x + j][y + i];
                        B[x + j][y + i] = a[0];
                    }

                y -= 4;

                for (i = 4; i < 8; ++i)
                {
                    for (j = 0; j < 8; ++j)
                        a[j] = A[y + i][x + j];
                    for (j = 0; j < 8; ++j)
                        B[x + i][y + j] = a[j];
                }

                x += 4;
                for (i = 0; i < 4; ++i)
                    for (j = i + 1; j < 4; ++j)
                    {
                        a[0] = B[x + i][y + j];
                        B[x + i][y + j] = B[x + j][y + i];
                        B[x + j][y + i] = a[0];
                    }
                
                y += 4;
                for (i = 0; i < 4; ++i)
                    for (j = i + 1; j < 4; ++j)
                    {
                        a[0] = B[x + i][y + j];
                        B[x + i][y + j] = B[x + j][y + i];
                        B[x + j][y + i] = a[0];
                    }

                y -= 4;
                x -= 4;

                for (i = 0; i < 4; ++i)
                {
                    for (j = 0; j < 4; ++j)
                        a[j] = B[x + 4 + i][y + j];
                    for (j = 4; j < 8; ++j)
                        a[j] = B[x + i][y + j];
                    for (j = 4; j < 8; ++j)
                        B[x + i][y + j] = a[j - 4];
                    for (j = 0; j < 4; ++j)
                        B[x + 4 + i][y + j] = a[j + 4]; 
                }

                a[0] = x; x = y; y = a[0];
            }
}

/* 
 * You can define additional transpose functions below. We've defined
 * a simple one below to help you get started. 
 */ 

/* 
 * trans - A simple baseline transpose function, not optimized for the cache.
 */
char trans_desc[] = "Simple row-wise scan transpose";
void trans(int M, int N, int A[N][M], int B[M][N])
{
    int i, j, tmp;

    for (i = 0; i < N; i++) {
        for (j = 0; j < M; j++) {
            tmp = A[i][j];
            B[j][i] = tmp;
        }
    }    

}

/*
 * registerFunctions - This function registers your transpose
 *     functions with the driver.  At runtime, the driver will
 *     evaluate each of the registered functions and summarize their
 *     performance. This is a handy way to experiment with different
 *     transpose strategies.
 */
void registerFunctions()
{
    /* Register your solution function */
    registerTransFunction(transpose_submit, transpose_submit_desc); 

    /* Register any additional transpose functions */
//    registerTransFunction(trans, trans_desc); 

}

/* 
 * is_transpose - This helper function checks if B is the transpose of
 *     A. You can check the correctness of your transpose by calling
 *     it before returning from the transpose function.
 */
int is_transpose(int M, int N, int A[N][M], int B[M][N])
{
    int i, j;

    for (i = 0; i < N; i++) {
        for (j = 0; j < M; ++j) {
            if (A[i][j] != B[j][i]) {
                return 0;
            }
        }
    }
    return 1;
}

