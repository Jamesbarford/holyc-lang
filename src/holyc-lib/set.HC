/* Implementation of CLRS redblack tree for set operations can also use the 
 * redblack tree as a hashtable if you want */

#define __RB_RED__   0
#define __RB_BLACK__ 1


class RBNode;
class RBNode
{
  U0 *key;
  U0 *value;
  I64 color;
  RBNode *left;
  RBNode *right;
  RBNode *parent;
};

static RBNode *RB_SENTINAL;

class RBTree
{
  RBNode *root;
  I64 size;
  U0 (*free_key)(U0 *);
  U0 (*free_value)(U0 *);
  I64 (*key_cmp)(U0 *, U0 *);
};

RBTree *RBTreeNew(U0 (*free_key)(U0 *), U0 (*free_value)(U0 *), 
                  U0 (*key_cmp)(U0 *, U0 *))
{
  static I64 has_init = 0;
  if (!has_init) {
    has_init = 1;
    RB_SENTINAL = MAlloc(sizeof(RBNode));
  }
  has_init = 1;
  RBTree *tree = MAlloc(sizeof(RBTree));
  tree->free_key = free_key;
  tree->free_value = free_value;
  tree->key_cmp = key_cmp;
  tree->root = RB_SENTINAL;
  tree->size = 0;
  RB_SENTINAL->color = __RB_BLACK__;
  RB_SENTINAL->left = NULL;
  RB_SENTINAL->right = NULL;
  RB_SENTINAL->key = NULL;
  RB_SENTINAL->value = NULL;
  return tree;
}

static RBNode *RBNodeNew(U0 *key, U0 *value)
{
  RBNode *n = MAlloc(sizeof(RBNode));
  n->key = key;
  n->value = value;
  n->color = __RB_RED__;
  n->right = RB_SENTINAL;
  n->left = RB_SENTINAL;
  n->parent = NULL;
  return n;
}

static U0 RBNodeRelease(RBTree *tree, RBNode *n)
{
  if (n && n != RB_SENTINAL) {
    if (tree->free_key) tree->free_key(n);
    if (tree->free_value) tree->free_value(n);
    free(n);
  }
}

static U0 RBLeftRot(RBTree *tree, RBNode *x)
{
  RBNode *y = x->right;
  x->right = y->left;
  if (y->left != RB_SENTINAL) {
    y->left->parent = x;
  }
  y->parent = x->parent;
  if (x->parent == NULL) {
    tree->root = y;
  } else if (x == x->parent->left) {
    x->parent->left = y;
  } else {
    x->parent->right = y;
  }
  y->left = x;
  x->parent = y;
}

static U0 RBRightRot(RBTree *tree, RBNode *y)
{
  RBNode *x = y->left;
  y->left = x->right;
  if (x->right != RB_SENTINAL) {
    x->right->parent = y;
  }
  x->parent = y->parent;
  if (y->parent == NULL) {
    tree->root = x;
  } else if (y == y->parent->right) {
    y->parent->right = x;
  } else {
    y->parent->left = x;
  }
  x->right = y;
  y->parent = x;
}

static RBNode *RBGrandParent(RBNode *n)
{
  if (n == NULL || n->parent == NULL) {
    return NULL;
  }
  return n->parent->parent;
}

U0 RBInsertFixUp(RBTree *tree, RBNode *z)
{
  while (z->parent->color == __RB_RED__) {
    RBNode *y;
    if (z->parent == z->parent->parent->left) {
      y = z->parent->parent->right;
      if (y->color == __RB_RED__) {
        y->color = __RB_BLACK__;
        z->parent->color = __RB_BLACK__;
        z->parent->parent->color = __RB_RED__;
        z = z->parent->parent;
      } else {
        if (z == z->parent->right) {
          z = z->parent;
          RBLeftRot(tree, z);
        }
        z->parent->color = __RB_BLACK__;
        z->parent->parent->color = __RB_RED__;
        RBRightRot(tree, z->parent->parent);
      }
    } else {
      y = z->parent->parent->left;
      if (y->color == __RB_RED__) {
        y->color = __RB_BLACK__;
        z->parent->color = __RB_BLACK__;
        z->parent->parent->color = __RB_RED__;
        z = z->parent->parent;
      } else {
        if (z == z->parent->left) {
          z = z->parent;
          RBRightRot(tree, z);
        }
        z->parent->color = __RB_BLACK__;
        z->parent->parent->color = __RB_RED__;
        RBLeftRot(tree, z->parent->parent);
      }
    }
    if (z == tree->root) {
      break;
    }
  }
  tree->root->color = __RB_BLACK__;
}

U0 RBTreeInsert(RBTree *tree, U0 *key, U0 *value)
{
  RBNode *x = tree->root;
  RBNode *y = NULL;
  RBNode *z = NULL;
  I64 cmp = 0;

  while (x != RB_SENTINAL) {
    y = x;
    cmp = tree->key_cmp(key, x->key);

    if (cmp < 0) {
      x = x->left;
    } else if (cmp > 0) {
      x = x->right;
    } else {
      /* XXX: Should duplicates be allowed? */
      return;
    }
  }

  z = RBNodeNew(key,value);
  z->parent = y;

  if (y == NULL) {
    tree->root = z;
  } else if (tree->key_cmp(z->key, y->key) < 0) {
    y->left = z;
  } else {
    y->right = z;
  }
  tree->size++;

  if (z->parent == NULL) {
    z->color = __RB_BLACK__;
    return;
  }

  if (z->parent->parent == NULL) {
    return;
  }

  RBInsertFixUp(tree,z);
}

static RBNode *RBNodeFind(RBTree *tree, RBNode *n, U0 *key)
{
  RBNode *cur = n;
  I64 cmp = 0;

  while (cur && cur != RB_SENTINAL) {
    cmp = tree->key_cmp(key, cur->key);
    if (cmp == 0) {
      return cur;
    } else if (cmp < 0) {
      cur = cur->left;
    } else {
      cur = cur->right;
    }
  }
  return NULL;
}

U0 *RBTreeGet(RBTree *tree, U0 *key)
{
  RBNode *n = RBNodeFind(tree,tree->root,key);
  if (n) return n->value;
  return n;
}

Bool RBTreeHas(RBTree *tree, U0 *key)
{
  RBNode *n = RBNodeFind(tree, tree->root, key);
  return n != NULL;
}

static U0 RBTransplant(RBTree *tree, RBNode *u, RBNode *v)
{
  if (u->parent == NULL) {
    tree->root = v;
  } else if (u == u->parent->left) {
    u->parent->left = v;
  } else {
    u->parent->right = v;
  }
  v->parent = u->parent;
}

static RBNode *RBNodeMin(RBNode *n)
{
  while (n && n->left) n = n->left;
  return n;
}

static U0 RBTreeFixDelete(RBTree *tree, RBNode *x)
{
  RBNode *w = NULL;
  while (x != NULL && x != tree->root && x->color == __RB_BLACK__) {
    if (x == x->parent->left) {
      w = x->parent->right;
      if (w->color == __RB_RED__) {
        w->color = __RB_BLACK__;
        x->parent->color = __RB_RED__;
        RBLeftRot(tree, x->parent);
        w = x->parent->right;
      }
      if (w->left->color == __RB_BLACK__ && w->right->color == __RB_BLACK__) {
        w->color = __RB_RED__;
        x = x->parent;
      } else {
        if (w->right->color == __RB_BLACK__) {
          w->left->color = __RB_BLACK__;
          w->color = __RB_RED__;
          RBRightRot(tree, w);
          w = x->parent->right;
        }
        w->color = x->parent->color;
        x->parent->color = __RB_BLACK__;
        w->right->color = __RB_BLACK__;
        RBLeftRot(tree, x->parent);
        x = tree->root;
      }
    } else {
      w = x->parent->left;
      if (w->color == __RB_RED__) {
        w->color = __RB_RED__;
        x->parent->color = __RB_BLACK__;
        RBRightRot(tree, x->parent);
        w = x->parent->left;
      }
      if (w->right->color == __RB_BLACK__ && w->left->color == __RB_BLACK__) {
        w->color = __RB_RED__;
        x = x->parent;
      } else {
        if (w->left->color == __RB_BLACK__) {
          w->right->color = __RB_BLACK__;
          w->color = __RB_RED__;
          RBLeftRot(tree, w);
          w = x->parent->left;
        }
        w->color = x->parent->color;
        x->parent->color = __RB_BLACK__;
        w->left->color = __RB_BLACK__;
        RBRightRot(tree, x->parent);
        x = tree->root;
      }
    }
  }
  x->color = __RB_BLACK__;
}

U0 RBTreeDelete(RBTree *tree, U0 *key)
{
  RBNode *cur = tree->root;
  RBNode *z = NULL;
  RBNode *y = NULL;
  RBNode *x = NULL;
  I64 cmp = 0;
  I64 original_color = -1;

  while (cur) {
    cmp = tree->key_cmp(key, x->key);
    if (cmp == 0) {
      z = cur;
    } else if (cmp > 0) {
      cur = cur->right;
    } else {
      cur = cur->left;
    }
  }

  if (z == NULL) {
    return;
  }

  y = z;
  original_color = y->color;

  if (z->left == NULL) {
    x = z->right;
    RBTransplant(tree, z, z->right);
  } else if (z->right == NULL) {
    x = z->left;
    RBTransplant(tree, z, z->left);
  } else {
    y = RBNodeMin(z->right);
    original_color = y->color;
    x = y->right;
    if (y != z->right) {
      RBTransplant(tree, y, y->right);
      y->right = z->right;
      y->right->parent = y;
    } else {
      x->parent = y;
    }
    RBTransplant(tree, z, y);
    y->left = z->left;
    y->left->parent = y;
    y->color = z->color;
  }

  if (original_color == __RB_BLACK__) {
    RBTreeFixDelete(tree, x);
  }

  if (z) {
    RBNodeRelease(tree, z);
  }
  tree->size--;
}

U0 RBTreeForEach(RBTree *tree,
                 U0 *user_data,
                 U0 (*callback)(U0 *user_data,
                                U0 *key, 
                                U0 *value))
{
  RBNode **stack = MAlloc(sizeof(RBNode*)*tree->size);
  I64 sp = 0;
  RBNode *cur = tree->root;

  while (cur) {
    while (cur != NULL && cur != RB_SENTINAL) {
      stack[sp++] = cur;
      cur = cur->left;
    }

    if (sp == 0) break;
    cur = stack[--sp];
    callback(user_data,cur->key,cur->value);
    cur = cur->right;
  }

  Free(stack);
}

U0 RBTreeRelease(RBTree *tree)
{
  if (tree) {
    RBNode **stack = MAlloc(sizeof(RBNode*)*tree->size);
    I64 sp = 0;
    RBNode *cur = tree->root;
    RBNode *next;

    while (cur) {
      while (cur != NULL && cur != RB_SENTINAL) {
        stack[sp++] = cur;
        cur = cur->left;
      }

      if (sp == 0) break;
      cur = stack[--sp];
      if (tree->free_key) tree->free_key(cur->key);
      if (tree->free_value) tree->free_value(cur->value);
      next = cur->right;
      Free(cur);
      cur = next;
    }

    Free(stack);
    Free(tree);
  }
}

class IntSet
{
  RBTree *tree; 
};

I64 IntSetCmp(U0 *k1, U0 *k2)
{
  I64 _k1 = k1(I64);
  I64 _k2 = k2(I64);
  if (_k1 < _k2) return -1;
  if (_k1 > _k2) return 1;
  return 0;
}

IntSet *IntSetNew()
{
  IntSet *set = MAlloc(sizeof(IntSet));
  set->tree = RBTreeNew(NULL,NULL,&IntSetCmp);
  return set;
}

Bool IntSetHas(IntSet *set, I64 key)
{
  return RBTreeHas(set->tree,key(U0*));
}

U0 IntSetAdd(IntSet *set, I64 key)
{
  return RBTreeInsert(set->tree,key(U0*),NULL);
}

U0 IntSetDelete(IntSet *set, I64 key)
{
  RBTreeDelete(set->tree,key(U0*));
}

U0 __IntSetAddToSet(U0 *user_data, U0 *key, U0 *value)
{
  IntSetAdd(user_data(IntSet *),key(I64));
}

IntSet *IntSetUnion(IntSet *s1, IntSet *s2)
{
  IntSet *union_set = IntSetNew;
  RBTreeForEach(s1->tree, union_set, &__IntSetAddToSet);
  RBTreeForEach(s2->tree, union_set, &__IntSetAddToSet);
  return union_set;
}

IntSet *IntSetDiff(IntSet *s1, IntSet *s2)
{
  IntSet *diff_set = IntSetNew;
  I64 sp = 0;
  RBNode **stack = MAlloc(sizeof(RBNode*)*s1->tree->size);
  RBNode *cur = s1->tree->root;

  while (cur != NULL) {
    while (cur != NULL && cur != RB_SENTINAL) {
      stack[sp++] = cur;
      cur = cur->left;
    }

    if (sp == 0) break;
    cur = stack[--sp];
    if (!IntSetHas(s2,cur->key(I64))) {
      IntSetAdd(diff_set,cur->key(I64));
    }
    cur = cur->right;
  }

  Free(stack);
  return diff_set;
}

U0 __IntSetIsEq(U0 *user_data, U0 *key, U0 *value)
{
  if (!IntSetHas(user_data(IntSet *),key(I64))) {
    Bool *has = user_data(Bool *);
    *has = FALSE;
  }
}

Bool IntSetEq(IntSet *s1, IntSet *s2)
{
  if (s1->tree->size != s2->tree->size) return FALSE;
  Bool retval = TRUE;
  RBTreeForEach(s1->tree,s2,&__IntSetIsEq);
  return retval;
}


U0 __IntSetAddToVec(U0 *user_data, U0 *key, U0 *value)
{
  IntVecPush(user_data(IntVec *),key(I64));
}

IntVec *IntSetToVec(IntSet *set)
{
  IntVec *vec = IntVecNew;
  RBTreeForEach(set->tree,vec,&__IntSetAddToVec);
  return vec;
}

IntSet *IntSetSymDiff(IntSet *s1, IntSet *s2)
{
  IntSet *sym_diff_set = IntSetNew();
  I64 max_size = 0;

  if (s2->tree->size > s1->tree->size) {
    max_size = s2->tree->size;
  } else {
    max_size = s1->tree->size;
  }
  if (max_size == 0) return sym_diff_set;

  RBNode **stack = MAlloc(sizeof(RBNode*)*max_size);
  I64 sp = 0;
  RBNode *cur = s1->tree->root;

  /* Add elements from s1 that are not in s2 */
  while (cur != NULL) {
    while (cur != NULL && cur != RB_SENTINAL) {
      stack[sp++] = cur;
      cur = cur->left;
    }

    if (sp == 0) break;
    cur = stack[--sp];
    if (!IntSetHas(s2(IntSet *), cur->key(I64))) {
      IntSetAdd(sym_diff_set, cur->key(I64));
    }
    cur = cur->right;
  }

  sp = 0;
  cur = s2->tree->root;
  /* Add elements from s2 that are not in s1 */
  while (cur != NULL) {
    while (cur != NULL && cur != RB_SENTINAL) {
      stack[sp++] = cur;
      cur = cur->left;
    }

    if (sp == 0) break;
    cur = stack[--sp];
    if (!IntSetHas(s1, cur->key(I64))) {
      IntSetAdd(sym_diff_set, cur->key(I64));
    }
    cur = cur->right;
  }

  Free(stack);
  return sym_diff_set;
}

U8 *IntSetToString(IntSet *set)
{
  U8 *buf = MAlloc(sizeof(U8)*128);
  U8 *dst = buf;
  U8 tmp[64];
  RBNode **stack = MAlloc(sizeof(RBNode*)*set->tree->size);
  I64 sp = 0;

  SPutChar(&dst,'{',&buf);
  RBNode *cur = set->tree->root;

  /* Add elements from s1 that are not in s2 */
  while (cur != NULL) {
    while (cur != NULL && cur != RB_SENTINAL) {
      stack[sp++] = cur;
      cur = cur->left;
    }

    if (sp == 0) break;
    cur = stack[--sp];
    snprintf(tmp,sizeof(tmp),"%lld, ",cur->key(I64));
    OutStr(tmp,&buf,&dst,0,0);

    cur = cur->right;
  }

  if (*dst != '{') {
    dst -= 2;
  }

  Free(stack);
  SPutChar(&dst,'}',&buf);
  return buf;
}

U0 IntSetRelease(IntSet *set)
{
  RBTreeRelease(set->tree);
  Free(set);
}
