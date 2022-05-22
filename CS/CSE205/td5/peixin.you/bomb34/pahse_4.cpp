#include <cstdio>
#include <iostream>

int func4(int a, int b)
{
    int c = a;
    if (a <= 0)
    {
        return 0;
    }
    int e = b;
    int res = b;
    if (a == 1)
    {
        return res;
    }
    a -= 1;
    res = func4(a, b);
    int d = res + e;
    a = c - 2;
    b = e;
    res = func4(a, b);
    res += d;
    return res;
}

int main()
{
    printf("%d\n", func4(6, 2));
}