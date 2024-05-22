#include "testhelper.HC"

#define STRINGS_LEN 9

Bool _TestStrings(U8 **strings, U8 *pattern, I32 expected_matches, I32 case_mode, Bool fuzzy)
{
  I32 matches = 0;
  for (auto i = 0; i < STRINGS_LEN; ++i) {
    auto res = FzF(strings[i],pattern,case_mode,fuzzy);
    if (res) {
      matches++;
      FzFPositionRelease(res);
    }
  }
  return matches == expected_matches;
}

Bool TestNoMatch(U8 **strings)
{
  return _TestStrings(strings,"HEY HOW ARE YOU?",0,FZF_CASE_SMART,TRUE);
}

Bool TestSuffix(U8 **strings)
{
  return _TestStrings(strings,"HC$",2,FZF_CASE_SMART,TRUE);
}

Bool TestSuffixCaseInsensative(U8 **strings)
{
  return _TestStrings(strings,"c$",4,FZF_CASE_IGNORE,TRUE);
}

Bool TestPrefix(U8 **strings)
{
  return _TestStrings(strings,"^He",2,FZF_CASE_SMART,TRUE);
}

Bool TestPrefixCaseInsensative(U8 **strings)
{
  return _TestStrings(strings,"^w",4,FZF_CASE_IGNORE,TRUE);
}

Bool TextMatchExact(U8 **strings)
{
  return _TestStrings(strings,"'hey.c",1,FZF_CASE_RESPECT,FALSE);
}

Bool TextMatchExactCaseInsensative(U8 **strings)
{
  return _TestStrings(strings,"'hey.c",2,FZF_CASE_IGNORE,FALSE);
}

I32 Main()
{
  "Test - FzF fuzzy finding: ";
  I32 tests = 7, correct = 0;
  U8 *strings[STRINGS_LEN] = {
    "Hello.HC",
    "Hey.c",
    "hey.c",
    "woof",
    "foo",
    "WHO",
    "wHo",
    "another.HC",
    "woo"
  };

  if (TestNoMatch(strings)) correct++;
  if (TestSuffix(strings)) correct++;
  if (TestSuffixCaseInsensative(strings)) correct++;
  if (TestPrefix(strings)) correct++;
  if (TestPrefixCaseInsensative(strings)) correct++;
  if (TextMatchExact(strings)) correct++;
  if (TextMatchExactCaseInsensative(strings)) correct++;
  
  PrintResult(correct,tests);
  "====\n";
  return 0;
}
