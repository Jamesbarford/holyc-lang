#include "testhelper.HC"

/* Self-referencing class without a forward declaration. 07 covers the
 * forward-decl form `class Node; class Node { Node *next; ... };`; this covers
 * the bare form, which used to raise "Type Node has not been declared". */

class Node
{
  Node *next;
  I64 v;
};

Bool TwoNodeList()
{
  Node *a = MAlloc(sizeof(Node));
  Node *b = MAlloc(sizeof(Node));
  a->v = 1; a->next = b;
  b->v = 2; b->next = NULL;
  Bool ok = a->v == 1 &&
            a->next == b &&
            a->next->v == 2 &&
            a->next->next == NULL;
  Free(b);
  Free(a);
  return ok;
}

class Tree
{
  Tree *left, *right;
  I64 v;
};

Bool SiblingChildren()
{
  Tree *root = MAlloc(sizeof(Tree));
  Tree *l = MAlloc(sizeof(Tree));
  Tree *r = MAlloc(sizeof(Tree));
  root->left = l;  l->v = 10; l->left = l->right = NULL;
  root->right = r; r->v = 20; r->left = r->right = NULL;
  root->v = 1;
  Bool ok = root->left->v == 10 && root->right->v == 20;
  Free(r); Free(l); Free(root);
  return ok;
}

I32 Main()
{
  "Test - Class self-reference (no forward decl): ";
  I64 correct = 0, tests = 2;
  if (TwoNodeList) correct++;
  if (SiblingChildren) correct++;
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
