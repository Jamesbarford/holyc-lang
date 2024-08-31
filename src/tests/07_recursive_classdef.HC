#include "testhelper.HC"

class TestListNode;
class TestListNode
{
  I64 value;
  TestListNode *next, *tail;
};

class TestList
{
  I64 size;
  TestListNode *root;
};

TestList *TestListNew()
{
  TestList *ll = MAlloc(sizeof(TestList));
  ll->size = 0;
  ll->root = NULL;
  return ll;
}

U0 TestListAdd(TestList *ll, I64 value)
{
  TestListNode *ln = MAlloc(sizeof(TestListNode));
  ln->value = value;
  ln->next = ll->root;
  ll->root = ln;
  ll->size++;
}

I32 Main()
{
  "Test - Linked list recusive definition class: ";
  I64 correct = 0, test_cases = 5;
  TestList *ll = TestListNew();
  TestListNode *ln;
  TestListAdd(ll,1);
  TestListAdd(ll,2);
  TestListAdd(ll,3);
  TestListAdd(ll,4);
  ln = ll->root;

  if (ln->value == 4) {
    correct++;
    ln = ln->next;
  }

  if (ln->value == 3) {
    correct++;
    ln = ln->next;
  }

  if (ln->value == 2) {
    correct++;
    ln = ln->next;
  }

  if (ln->value == 1) {
    correct++;
    ln = ln->next;
  }

  if (!ln) {
    correct++;
  }

  PrintResult(correct,test_cases);
  "====\n";
  return 0;
}
