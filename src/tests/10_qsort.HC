#include "testhelper.HC"

public U0 quickSort(I64 *arr, I64 hi, I64 lo=0)
{
  if (lo < hi) {
    I64 pivot = arr[hi], idx = lo, i = lo;
    while (i < hi) {
      if (arr[i] <= pivot) {
        I64 tmp = arr[i];
        arr[i] = arr[idx];
        arr[idx] = tmp;
        ++idx;
      }
      ++i;
    }
    arr[hi] = arr[idx];
    arr[idx] = pivot;
    quickSort(arr,idx-1,lo);
    quickSort(arr,hi,idx+1);
  }
}

I32 Main()
{
  "Test - qsort, recursion and default function params: ";
  I64 correct = 0;
  I64 nums[10] = {11,1,2,4,0,5,6,7,8,3};    
  I64 expected[10] = {0,1,2,3,4,5,6,7,8,11};
  quickSort(nums,10-1);

  for (I64 i = 0; i < 10; ++i) {
    if (nums[i] != expected[i]) {
      break;
    } 
    correct++;
  }
  PrintResult(correct == 10, 1);
  "====\n";
  return 0;
}
Main;
