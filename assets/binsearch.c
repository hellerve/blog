#include <time.h>
#include <stdio.h>
#include <stdlib.h>

int binarySearch(int* a, int n, int k) {
       int lo = 0;
       int hi = n-1;
       int m;

       while(lo <= hi) {
               m = (lo + hi) / 2;
               #ifdef PREFETCH
               __builtin_prefetch (&a[(m+1+hi)/2], 0, 1);
               __builtin_prefetch (&a[(lo+m-1)/2], 0, 1);
               #endif

               if(a[m] < k) lo = m + 1;
               else if(a[m] == k) return m;
               else if(a[m] > k) hi = m - 1;
       }
       return -1;
}

int main() {
   int n = 1024*1024*512, *a =  malloc(n*sizeof(int)), i, res;

   for (i = 0; i < n; i++) a[i] = i;

   int nl = 1024*1024*8;
   srand(time(NULL));
   int *lookups = malloc(nl * sizeof(int));
   for (i=0;i<nl;i++){
     lookups[i] = rand() % n;
   }
   for (i = 0; i<nl; i++) res = binarySearch(a, n, lookups[i]);

   free(a);
   free(lookups);

   return 0;
}
