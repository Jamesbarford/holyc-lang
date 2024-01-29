#include "testhelper.HC"

class ListNode
{
  I64 value;
  ListNode *next, *tail;
};

class List
{
  I64 size;
  ListNode *root;
};

List *ListNew()
{
  List *ll = MAlloc(sizeof(List));
  ll->size = 0;
  ll->root = NULL;
  return ll;
}

U0 ListAdd(List *ll, I64 value)
{
  ListNode *ln = MAlloc(sizeof(ListNode));
  ln->value = value;
  ln->next = ll->root;
  ll->root = ln;
  ll->size++;
}

U0 Main()
{
  "Test - Linked list recusive definition class: ";
  I64 correct = 0, test_cases = 5;
  List *ll = ListNew();
  ListNode *ln;
  ListAdd(ll,1);
  ListAdd(ll,2);
  ListAdd(ll,3);
  ListAdd(ll,4);
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
}
