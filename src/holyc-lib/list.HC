#include "./defs.HH"
#include "./memory.HC"

class List;
class List
{
  U0 *value;
  List *next, *prev;
};

U0 ListInit(List *l)
{
  l->value = NULL;
  l->next = l->prev = l;
}

List *ListNew()
{
  List *ll = MAlloc(sizeof(List));
  ll->value = NULL;
  ll->next = ll->prev = ll;
  return ll;
}

U0 ListPrepend(List *head, U0 *value)
{
  List *node = ListNew();
  node->value = value;
  node->prev = head;
  node->next = head->next;
  head->next->prev = node;
  head->next = node;
}

Bool ListEmpty(List *l)
{
  return (l == NULL || l->next == l);
}

U0 ListAppend(List *head, U0 *value)
{
  List *tail = head->prev;
  List *node = ListNew();
  node->value = value;
  node->prev = tail;
  node->next = head;
  tail->next = node;
  head->prev = node;
}

U0 *ListDeque(U0 *l)
{
  if (!l) return NULL;
  List *ll = l(List *);
  if (ll->next == ll) {
    return NULL;
  }
  List *node = ll->next;
  U0 *val = node->value;
  node->next->prev = ll;
  ll->next = ll->next->next;
  Free(node);
  return val;
}

U0 ListInsertBefore(List *ll, List *new_node)
{
  new_node->next = ll;
  new_node->prev = ll->prev;
  ll->prev->next = new_node;
  ll->prev = new_node;
}

U0 ListInsertValueBefore(List *ll, U0 *value)
{
  List *prev = ListNew();
  prev->value = value;
  ListInsertBefore(ll,prev);
}

U0 *ListPop(U0 *l) {
  List *ll = l(List *);
  if (ll->next == ll) {
    return NULL;
  }
  List *node = ll->prev;
  U0 *val = node->value;
  node->prev->next = ll;
  ll->prev = ll->prev->prev;
  Free(node);
  return val;
}

U0 ListRelease(List *ll, U0 (*FreeValue)(U0 *arg))
{
  List *node = ll->next;
  List *next;
  while (node != ll) {
    next = node->next;
    if (FreeValue) {
      FreeValue(node->value);
    }
    Free(node);
    node = next;
  }
  Free(ll);
}

I64 ListCount(List *ll)
{
  if (ll == ll->next) return 0;
  auto node = ll->next;
  I64 count = 0;
  while (node != ll) {
    count++;
    node = node->next;
  }
  return count;
}

/* Merge l2 to the start of l1 and Free l2
 * Example
 * l1 = [1,2,3]
 * l2 = [4,5,6]
 * ListMerge(l1,l2);
 * l1 = [4,5,6,1,2,3] (and linked the otherway too)
 * l2 = nullptr
 */
U0 ListMergePrepend(List *l1, List *l2)
{
  List *h1 = l1->next;

  List *h2 = l2->next;
  List *t2 = l2->prev;

  l1->next = h2;
  h2->prev = l1;
  t2->next = h1;
  h1->prev = t2;
  Free(l2);
}

/* Merge l2 to the end of l1 and Free l2.
 * Example
 * l1 = [1,2,3]
 * l2 = [4,5,6]
 * ListMerge(l1,l2);
 * l1 = [1,2,3,4,5,6] (and linked the otherway too)
 * l2 = nullptr
 * */
U0 ListMergeAppend(List *l1, List *l2)
{
  List *h1 = l1->next;
  List *t1 = l1->prev;

  List *h2 = l2->next;
  List *t2 = l2->prev;

  h2->prev = t1;
  h2->prev->next = h1;
  t1->next = h2;
  t2->next = l1;
  l1->prev = t2;
  Free(l2);
}
