#include "testhelper.HC"

U8 *html_attrs[] = {
  "invalid",
  "html",
  "div",
  "span",
  "h1",
  "h2",
  "h3",
  "h4",
  "h5",
  "h6",
  "nav",
  "strong",
  "italic"
};


Bool TestStaticArray()
{
  I64 tests = 4, correct = 0;
  static U8 *arr[] = {
    "f1",
    "f2",
    "dogs and cats are fun"
  };

  I64 len = sizeof(arr)/sizeof(arr[0]);

  if (len == 3) correct++;
  if (!StrCmp(arr[0],"f1")) correct++;
  if (!StrCmp(arr[1],"f2")) correct++;
  if (!StrCmp(arr[2],"dogs and cats are fun")) correct++;

  return tests == correct;
}

Bool TestGlobalArray()
{
  I64 tests = 13, correct = 0;
  I64 len = sizeof(html_attrs)/sizeof(html_attrs[0]);

  if (len == 12) correct++;
  if (!StrCmp(html_attrs[0], "invalid")) correct++;
  if (!StrCmp(html_attrs[1], "html")) correct++;
  if (!StrCmp(html_attrs[2], "div")) correct++;
  if (!StrCmp(html_attrs[3], "span")) correct++;
  if (!StrCmp(html_attrs[4], "h1")) correct++;
  if (!StrCmp(html_attrs[5], "h2")) correct++;
  if (!StrCmp(html_attrs[6], "h3")) correct++;
  if (!StrCmp(html_attrs[7], "h4")) correct++;
  if (!StrCmp(html_attrs[8], "h5")) correct++;
  if (!StrCmp(html_attrs[9], "h6")) correct++;
  if (!StrCmp(html_attrs[10], "nav")) correct++;
  if (!StrCmp(html_attrs[11], "strong")) correct++;
  if (!StrCmp(html_attrs[12], "italic")) correct++;
  return correct == tests;
}

I32 Main()
{
  "Test - Global and static array\n";
  I64 tests = 2, correct = 0;
  if (TestStaticArray()) correct++;
  if (TestGlobalArray()) correct++;
  PrintResult(correct,tests);
  "====\n";
  return 0;
}
