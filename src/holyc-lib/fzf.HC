/* This code has been adapted from a C fzf implementation:
 * https://github.com/nvim-telescope/telescope-fzf-native.nvim */
#define FZF_CASE_SMART   0
#define FZF_CASE_IGNORE  1
#define FZF_CASE_RESPECT 2

#define FZF_SCORE_MATCH 16
#define FZF_SCORE_GAP_START -3
#define FZF_SCORE_GAP_EXTENTION -1
#define FZF_BONUS_BOUNDARY  (FZF_SCORE_MATCH / 2)
#define FZF_BONUS_NON_WORD  (FZF_SCORE_MATCH / 2)
#define FZF_BONUS_CAMEL_123  (FZF_BONUS_BOUNDARY + FZF_SCORE_GAP_EXTENTION)
#define FZF_BONUS_CONSECUTIVE  -(FZF_SCORE_GAP_START + FZF_SCORE_GAP_EXTENTION)
#define FZF_BONUS_FIRST_CHAR_MULTIPLIER (2)

#define FZF_CHAR_NON_WORD 0
#define FZF_CHAR_LOWER    1
#define FZF_CHAR_UPPER    2
#define FZF_CHAR_LETTER   3
#define FZF_CHAR_NUMBER   4

public class FzFPosition
{
  I64 *data;
  I64 size;
  I64 cap;
  I64 score;
};

class FzFI16
{
  I16 *data;
  I64 size;
  I64 cap;
  Bool allocated;
};

class FzFI32
{
  I32 *data;
  I64 size;
  I64 cap;
  Bool allocated;
};

class FzFResult
{
  I64 start;
  I64 end;
  I64 score;
};

class FzFSlab
{
  FzFI16 i16;
  FzFI32 i32;
};

class FzFSlabConfig
{
  I64 size_16;
  I64 size_32;
};

class FzFString
{
  U8 *data;
  I64 size;
};

class FzFTerm
{
  FzFResult *(*fn)(FzFResult *res, Bool case_sensitive,
      FzFString *text, FzFString *pattern, FzFPosition *pos, FzFSlab *slab);
  Bool inv;
  U8 *ptr;
  U0 *text;
  Bool case_sensitive;
};

class FzFTermSet 
{
  List *entries;
  I64 size;
  I64 cap;
};

class FzFPattern
{
  List *entries;
  I64 size;
  I64 cap;
  Bool only_inv;
};

class FzFStrSlice 
{
  U8 *data;
  I64 size;
};

U0 FzFFuzzyMatchV1(FzFResult *res, Bool case_sensitive,
    FzFString *text, FzFString *pattern,
    FzFPosition *pos, FzFSlab *slab);

FzFPattern *FzFParsePattern(I32 case_mode,U8 *pattern, Bool fuzzy=TRUE);

U0 FzFPatternRelease(FzFPattern *pattern);

I32 FzFGetScore(U8 *text, FzFPattern *pattern, FzFSlab *slab);

FzFPosition *FzFPosArray(I64 len);
FzFPosition *FzFGetPositions(U8 *text, FzFPattern *pattern,FzFSlab *slab);

public U0 FzFPositionRelease(FzFPosition *pos);

FzFSlab *FzFSlabNew(FzFSlabConfig *config);
FzFSlab *FzFDefaultSlab();
U0 FzFSlabRelease(FzFSlab *slab);

U0 slice_str(FzFStrSlice *s, U8 *input, I64 from, I64 to) {
  s->data = input+from;
  s->size = to-from;
}

U0 slice_str_right(FzFStrSlice *s, U8 *input, I64 to)
{
  slice_str(s, input, 0, to);
}

I32 index_byte(FzFString *str, U8 b) {
  for (auto i = 0; i < str->size; i++) {
    if (str->data[i] == b) {
      return i(I32);
    }
  }
  return -1;
}

I64 leading_whitespaces(FzFString *str)
{ // XXX: extract to strings.HC
  I64 whitespaces = 0;
  for (I64 i = 0; i < str->size; i++) {
    if (!IsSpace(str->data[i](U8))) {
      break;
    }
    whitespaces++;
  }
  return whitespaces;
}

I64 trailing_whitespaces(FzFString *str)
{ // XXX: extract to strings.HC
  I64 whitespaces = 0;
  for (I64 i = str->size - 1; i >= 0; i--) {
    if (!IsSpace(str->data[i](U8))) {
      break;
    }
    whitespaces++;
  }
  return whitespaces;
}

Bool has_suffix(U8 *str, I64 len, U8 *suffix, I64 suffix_len)
{
  FzFStrSlice s;
  slice_str(&s, str, len - suffix_len, len);
  return len >= suffix_len && !StrNCmp(s.data, suffix, suffix_len);
}

U8 *str_replace_char(U8 *str, U8 find, U8 replace) {
  U8 *current_pos = strchr(str, find);
  while (current_pos) {
    *current_pos = replace;
    current_pos = strchr(current_pos, find);
  }
  return str;
}

U8 *str_replace(U8 *orig, U8 *rep, U8 *with) {
  if (!orig || !rep || !with) {
    return NULL;
  }

  U8 *result,*ins,*tmp;

  I64 len_rep = StrLen(rep);
  I64 len_front = 0;
  I64 len_orig = StrLen(orig);
  I64 len_with = StrLen(with);
  I64 count = 0;

  if (len_rep == 0) {
    return NULL;
  }

  ins = orig;
  for (; (tmp = strstr(ins, rep)); ++count) {
    ins = tmp + len_rep;
  }

  tmp = result = MAlloc(len_orig + (len_with - len_rep) * count + 1);
  if (!result) {
    return NULL;
  }

  while (count--) {
    ins = strstr(orig, rep);
    len_front = ins - orig;
    tmp = StrNCpy(orig, len_front) + len_front;
    StrCpy(tmp,with);
    tmp += len_with;
    orig += len_front + len_rep;
    len_orig -= len_front + len_rep;
  }
  MemCpy(tmp,orig,len_orig);
  tmp[len_orig] = 0;
  return result;
}

U8 *str_tolower(U8 *str, I64 size) {
  U8 *lower = MAlloc((size+2)*sizeof(U8));
  for (I64 i = 0; i < size; ++i) {
    lower[i] = ToLower(str[i]);
  }
  lower[size] = '\0';
  return lower;
}

I16 max16(I16 a, I16 b) {
  if (a > b) return a;
  return b;
}

FzFPosition *FzFPosArray(I64 len)
{
  FzFPosition *pos = MAlloc(sizeof(FzFPosition));
  pos->size = 0;
  pos->cap = len;
  if (len > 0) {
    pos->data = MAlloc(len * sizeof(I64));
  } else {
    pos->data = NULL;
  }
  return pos;
}

U0 resize_pos(FzFPosition *pos, I64 add_len, I64 comp) {
  if (!pos) {
    return;
  }
  if (pos->size + comp > pos->cap) {
    if (add_len > 0) {
      pos->cap += add_len;
    } else {
      pos->cap += 1;
    }
    pos->data = ReAlloc(pos->data, sizeof(I64) * pos->cap);
  }
}

U0 append_pos(FzFPosition *pos, I64 value) {
  if (pos) {
    resize_pos(pos, pos->cap, 10);
    pos->data[pos->size] = value;
    pos->size++;
  }
}

U0 insert_range(FzFPosition *pos, I64 start, I64 end) {
  if (!pos) {
    return;
  }

  I32 diff = (end(I32) - start(I32));
  if (diff <= 0) {
    return;
  }

  resize_pos(pos, diff, diff);
  for (I64 i = start; i < end; i++) {
    pos->data[pos->size] = i;
    pos->size++;
  }
}

I32 char_cls_of_ascii(U8 ch) {
  if ('a' <= ch <= 'z') return FZF_CHAR_LOWER;
  if ('A' <= ch <= 'Z') return FZF_CHAR_UPPER;
  if ('0' <= ch <= '9') return FZF_CHAR_NUMBER;
  return FZF_CHAR_NON_WORD;
}

I32 char_cls_of(U8 ch)
{
  return char_cls_of_ascii(ch);
}

I16 bonus_for(I32 prev_cls, I32 cls) {
  if (prev_cls == FZF_CHAR_NON_WORD && cls != FZF_CHAR_NON_WORD) {
    return FZF_BONUS_BOUNDARY;
  }
  if ((prev_cls == FZF_CHAR_LOWER && cls == FZF_CHAR_UPPER) ||
      (prev_cls != FZF_CHAR_NUMBER && cls == FZF_CHAR_NUMBER)) {
    return FZF_BONUS_CAMEL_123;
  }
  if (cls == FZF_CHAR_NON_WORD) {
    return FZF_BONUS_NON_WORD;
  }
  return 0;
}

I16 bonus_at(FzFString *input, I64 idx)
{
  if (idx == 0) {
    return FZF_BONUS_BOUNDARY;
  }
  return bonus_for(char_cls_of(input->data[idx - 1]),
      char_cls_of(input->data[idx]));
}

I32 try_skip(FzFString *input, Bool case_sensitive, U8 b, I32 from)
{
  FzFStrSlice slice;
  slice_str(&slice, input->data, from(I64), input->size);
  FzFString byte_array;
  byte_array.data = slice.data;
  byte_array.size = slice.size;
  I32 idx = index_byte(&byte_array, b);
  if (idx == 0) {
    return from;
  }

  if (!case_sensitive && ('a' <= b <= 'z')) {
    if (idx > 0) {
      FzFStrSlice tmp;
      slice_str_right(&tmp, byte_array.data, idx(I64));
      byte_array.data = tmp.data;
      byte_array.size = tmp.size;
    }
    I32 uidx = index_byte(&byte_array, b - 32);
    if (uidx >= 0) {
      idx = uidx;
    }
  }
  if (idx < 0) {
    return -1;
  }

  return from + idx;
}

I32 ascii_fuzzy_index(FzFString *input, U8 *pattern,
    I64 size, Bool case_sensitive)
{
  I32 first_idx = 0, idx = 0;
  for (I64 pidx = 0; pidx < size; pidx++) {
    idx = try_skip(input, case_sensitive, pattern[pidx], idx);
    if (idx < 0) {
      return -1;
    }
    if (pidx == 0 && idx > 0) {
      first_idx = idx - 1;
    }
    idx++;
  }

  return first_idx;
}

I32 FzFCalculateScore(Bool case_sensitive, FzFString *text, FzFString *pattern,
    I64 sidx, I64 eidx, FzFPosition *pos)
{
  auto M = pattern->size;

  I64 pidx = 0;
  I64 score = 0, consecutive = 0;
  Bool in_gap = FALSE;
  I16 first_bonus = 0;

  resize_pos(pos, M, M);
  I32 prev_cls = FZF_CHAR_NON_WORD;
  if (sidx > 0) {
    prev_cls = char_cls_of(text->data[sidx - 1]);
  }
  for (I64 idx = sidx; idx < eidx; idx++) {
    U8 c = text->data[idx];
    I32 cls = char_cls_of(c);
    if (!case_sensitive) {
      c = ToLower(c);
    }
    if (c == pattern->data[pidx]) {
      append_pos(pos, idx);
      score += FZF_SCORE_MATCH;
      I16 bonus = bonus_for(prev_cls, cls);
      if (consecutive == 0) {
        first_bonus = bonus;
      } else {
        if (bonus == FZF_BONUS_BOUNDARY) {
          first_bonus = bonus;
        }
        bonus = max16(max16(bonus, first_bonus), FZF_BONUS_CONSECUTIVE);
      }
      if (pidx == 0) {
        score += (bonus * FZF_BONUS_FIRST_CHAR_MULTIPLIER)(I32);
      } else {
        score += bonus(I32);
      }
      in_gap = FALSE;
      consecutive++;
      pidx++;
    } else {
      if (in_gap) {
        score += FZF_SCORE_GAP_EXTENTION;
      } else {
        score += FZF_SCORE_GAP_START;
      }
      in_gap = TRUE;
      consecutive = 0;
      first_bonus = 0;
    }
    prev_cls = cls;
  }
  return score;
}

U0 FzFPrefixMatch(FzFResult *res, Bool case_sensitive, FzFString *text,
    FzFString *pattern, FzFPosition *pos, FzFSlab *slab)
{
  I64 M,trimmed_len,start,end;
  I32 score;
  U8 c;
  start = end = score = trimmed_len = 0;
  M = pattern->size;

  if (M == 0) goto no_match;

  if (IsSpace(pattern->data[0])) {
    trimmed_len = leading_whitespaces(text);
  }

  if ((text->size - trimmed_len) < M) goto no_match;

  for (I64 i = 0; i < M; ++i) {
    c = text->data[trimmed_len + i];
    if (!case_sensitive) c = ToLower(c);
    if (c != pattern->data[i]) goto no_match;
  }

  res->end = trimmed_len + M;
  res->start = trimmed_len;
  res->score = FzFCalculateScore(case_sensitive,text,pattern,res->start,
                                 res->end,NULL);
  insert_range(pos,res->start,res->end);
  return;

no_match:
  res->start = -1;
  res->end = -1;
  res->score = 0;
}

U0 FzFSuffixMatch(FzFResult *res, Bool case_sensitive,
    FzFString *text, FzFString *pattern,
    FzFPosition *pos, FzFSlab *slab)
{
  I64 trimmed_len = text->size, M = pattern->size,diff,start,end;
  I32 score;

  if (M == 0 || !IsSpace(pattern->data[M - 1])) {
    trimmed_len -= trailing_whitespaces(text);
  }

  if (M == 0) goto no_match;

  diff = trimmed_len - M;
  if (diff < 0) goto no_match;

  for (I64 i = 0; i < M; ++i) {
    U8 c = text->data[i+diff];
    if (!case_sensitive) c = ToLower(c);
    if (c != pattern->data[i]) goto no_match;
  }

  res->start = trimmed_len - M;
  res->end = trimmed_len;
  res->score = FzFCalculateScore(case_sensitive,text,pattern,res->start,
      res->end,NULL);

  insert_range(pos,res->start,res->end);
  return;

no_match:
  res->start = -1;
  res->end = -1;
  res->score = 0;
  return;
}

U0 FzFEqNaiveMatch(FzFResult *res, Bool case_sensitive, FzFString *text,
    FzFString *pattern, FzFPosition *pos, FzFSlab *slab)
{
  I64 M,N;
  I64 start,end,best_bonus,bonus,pidx;
  I32 best_pos,score;
  U8 c;

  M = pattern->size;
  N = text->size;
  pidx = bonus = 0;
  best_pos = best_bonus = -1;


  if (M == 0)
    goto no_match;
  if (N<M)
    goto no_match;
  if (ascii_fuzzy_index(text,pattern->data,M,case_sensitive) < 0)
    goto no_match;

  for (I64 i = 0; i < N; ++i) {
    c = text->data[i];
    if (!case_sensitive) c = ToLower(c);

    if (c == pattern->data[pidx]) {
      if (pidx == 0) {
        bonus = bonus_at(text,i);
      }
      ++pidx;

      if (pidx == M) {
        if (bonus > best_bonus) {
          best_pos = i;
          best_bonus = bonus;
        }
      }
      if (bonus == FZF_BONUS_BOUNDARY) break;

      i -= pidx-1;
      pidx = 0;
      bonus = 0;
    } else {
      i -= pidx;
      pidx = 0;
      bonus = 0;
    }
  }

  if (best_pos >= 0) {
    auto bp = best_pos;
    res->start = bp - M + 1;
    res->end = bp + 1;
    res->score = FzFCalculateScore(case_sensitive,text,pattern,
        res->start,res->end,NULL);
    insert_range(pos,res->start,res->end);
    return;
  }

no_match:
  res->start = -1;
  res->end = -1;
  res->score = 0;
}

U0 FzFEqMatch(FzFResult *res, Bool case_sensitive, FzFString *text,
    FzFString *pattern, FzFPosition *pos, FzFSlab *slab)
{
  auto M = pattern->size;
  I64 trimmed_len,trimmed_end_len,start,end;
  I32 score;
  Bool match;
  U8 c,pchar;
  
  if (M == 0) {
    start = end = -1;
    score = 0;
    goto out;
  }
  
  trimmed_len = leading_whitespaces(text);
  trimmed_end_len = trailing_whitespaces(text);

  if ((text->size - trimmed_len - trimmed_end_len) != 0) {
    start = end = -1;
    score = 0;
    goto out;
  }

  match = TRUE;
  for (I64 i = 0; i < M; ++i) {
    pchar = pattern->data[i];
    c = text->data[trimmed_len+i];
    if (!case_sensitive) c = ToLower(c);
    if (c != pchar) {
      match = FALSE;
      break;
    }
  }

  if (match) {
    insert_range(pos,trimmed_len,trimmed_len+M);
    start = trimmed_len;
    end = trimmed_len+M;
    score = (FZF_SCORE_MATCH+FZF_BONUS_BOUNDARY) * M + 
      (FZF_BONUS_FIRST_CHAR_MULTIPLIER-1) * FZF_BONUS_BOUNDARY;
  } else {
    start = end = -1;
    score = 0;
  } 

out:
  res->start = start;
  res->end = end;
  res->score = score;
}

U0 FzFFuzzyMatchV1(FzFResult *res, Bool case_sensitive, FzFString *text,
    FzFString *pattern, FzFPosition *pos, FzFSlab *slab)
{
  auto M = pattern->size;
  auto N = text->size;
  I64 start, end;

  if (M == 0) {
    return;
  }
  if (ascii_fuzzy_index(text, pattern->data, M, case_sensitive) < 0) {
    res->start = -1;
    res->end = -1;
    res->score = 0;
    return;
  }

  I64 pidx = 0,sidx = -1, eidx = -1;

  for (I64 idx = 0; idx < N; idx++) {
    U8 c = text->data[idx];
    if (!case_sensitive) {
      c = ToLower(c);
    }
    if (c == pattern->data[pidx]) {
      if (sidx < 0) {
        sidx = idx;
      }
      pidx++;
      if (pidx == M) {
        eidx = (idx + 1)(I32);
        break;
      }
    }
  }

  if (sidx >= 0 && eidx >= 0) {
    start = sidx;
    end = eidx;
    pidx--;
    for (I64 idx = end - 1; idx >= start; idx--) {
      U8 c = text->data[idx];
      if (!case_sensitive) {
        c = ToLower(c);
      }
      if (c == pattern->data[pidx]) {
        pidx--;
        if (pidx < 0) {
          start = idx;
          break;
        }
      }
    }

    auto score = FzFCalculateScore(case_sensitive, text,
        pattern, start, end, pos);
    res->start = start;
    res->end = end;
    res->score = score;
    return;
  }
  res->start = -1;
  res->end = -1;
  res->score = 0;
  return;
}

FzFPattern *FzFParsePattern(I32 case_mode, U8 *pattern, Bool fuzzy=TRUE)
{
  FzFPattern *pat_obj = CAlloc(sizeof(FzFPattern));
  pat_obj->entries = ListNew();
  FzFTerm term;

  I64 pat_len = StrLen(pattern);
  if (pat_len == 0) {
    return pat_obj;
  }

  pattern = StrTrimStart(pattern,pat_len,&pat_len);
  while (has_suffix(pattern, pat_len, " ", 1) &&
      !has_suffix(pattern, pat_len, "\\ ", 2)) {
    pattern[pat_len - 1] = 0;
    pat_len--;
  }

  U8 *pattern_copy = str_replace(pattern, "\\ ", "\t");
  U8 *delim = " ";
  U8 *ptr = strtok(pattern_copy, delim);

  FzFTermSet *set = CAlloc(sizeof(FzFTermSet));
  set->entries = ListNew();

  Bool switch_set = FALSE;
  Bool after_bar = FALSE;
  while (ptr != NULL) {
    FzFResult *(*fn)(FzFResult *res, Bool _case_sensitive,
        FzFString *_text, FzFString *_pattern,
        FzFPosition *_pos, FzFSlab *_slab) = &FzFFuzzyMatchV1;

    Bool inv = FALSE;

    auto len = StrLen(ptr);
    str_replace_char(ptr, '\t', ' ');
    U8 *text = StrNew(ptr);

    U8 *og_str = NULL;
    U8 *lower_text = str_tolower(text, len);
    Bool case_sensitive = case_mode == FZF_CASE_RESPECT ||
                         (case_mode == FZF_CASE_SMART && 
                          StrCmp(text, lower_text) != 0);

    if (!case_sensitive) {
      if (text) {
        Free(text);
      }
      text = lower_text;
      og_str = lower_text;
    } else {
      if (lower_text) {
        Free(lower_text);
      }
    }

    if (!fuzzy) {
      fn = &FzFEqNaiveMatch;
    }
    if (set->size > 0 && !after_bar && StrCmp(text, "|") == 0) {
      switch_set = FALSE;
      after_bar = TRUE;
      ptr = strtok(NULL, delim);
      if (og_str) {
        Free(og_str);
      }
      continue;
    }
    after_bar = FALSE;
    if (!StrNCmp(text, "!", 1)) {
      inv = TRUE;
      fn = &FzFEqNaiveMatch;
      text++;
      len--;
    }

    if (StrCmp(text, "$") != 0 && has_suffix(text, len, "$", 1)) {
      fn = &FzFSuffixMatch;
      text[len - 1] = '\0';
      len--;
    }

    if (!StrNCmp(text, "'", 1)) {
      if (fuzzy && !inv) {
        fn = &FzFEqNaiveMatch;
        text++;
        len--;
      } else {
        fn = &FzFFuzzyMatchV1;
        text++;
        len--;
      }
    } else if (!StrNCmp(text, "^", 1)) {
      if (fn == &FzFSuffixMatch) {
        fn = &FzFEqMatch;
      } else {
        fn = &FzFPrefixMatch;
      }
      text++;
      len--;
    }

    if (len > 0) {
      if (switch_set) {
        ListAppend(pat_obj->entries,set);
        set = MAlloc(sizeof(FzFTermSet));
        set->entries = ListNew();
        set->cap = 0;
        set->size = 0;
      }
      FzFString *text_ptr = MAlloc(sizeof(FzFString));
      text_ptr->data = text;
      text_ptr->size = len;
      FzFTerm *t = MAlloc(sizeof(FzFTerm));
      t->fn = fn;
      t->inv = inv;
      t->ptr = og_str;
      t->case_sensitive = case_sensitive;
      t->text = text_ptr;

      ListAppend(set->entries,t);
      set->size++;
      switch_set = TRUE;
    } else {
      if (og_str) {
        Free(og_str);
      }
    }

    ptr = strtok(NULL, delim);
  }
  if (set->size > 0) {
    ListAppend(pat_obj->entries,set);
    pat_obj->size++;
  } else {
    if (set) {
      if (set->entries) {
        ListRelease(set->entries,&Free);
      }
      Free(set);
    }
  }
  Bool only = TRUE;

  for (auto it = pat_obj->entries->next; it != pat_obj->entries; it = it->next) {
    auto term_set = it->value(FzFTermSet *);
    if (term_set->size > 1) {
      only = FALSE;
      break;
    }

    Bool x = term_set->entries->next->value(FzFTerm *)->inv;
    if (!x) {
      only = FALSE;
      break;
    }
  }

  pat_obj->only_inv = only;
  if (pattern_copy) {
    Free(pattern_copy);
  }
  return pat_obj;
}

static U0 FzFTermSetRelease(U0 *_set)
{
  FzFTermSet *set = _set;
  if (set) {
    ListRelease(set->entries,&Free);
    Free(set);
  }
}

U0 FzFPatternRelease(FzFPattern *pattern)
{
  if (pattern) {
    if (!ListEmpty(pattern->entries)) {
      ListRelease(pattern->entries,&FzFTermSetRelease);
      Free(pattern);
    } else if (pattern) {
      ListRelease(pattern->entries,&Free);
      Free(pattern);
    }
  }
}

I64 FzFGetScore(U8 *text, FzFPattern *pattern, FzFSlab *slab)
{
  if (text == NULL || ListEmpty(pattern->entries)) return 1;

  auto entries = pattern->entries;
  FzFString input;
  input.data = text;
  input.size = StrLen(text);

  if (pattern->only_inv) {
    I64 final = 0;
    for (auto it = entries->next; it != entries; it = it->next) {
      FzFTermSet *term_set = it->value;
      FzFTerm *term = term_set->entries->next->value;
      FzFResult res;
      term->fn(&res,term->case_sensitive,&input,term->text(FzFString *),NULL,slab);
    }
    if (final > 0) {
      return 0;
    }
    return 1;
  }

  I64 total_score = 0;
  for (auto it = entries->next; it != entries; it = it->next) {
    FzFTermSet *term_set = it->value(FzFTermSet *);
    I64 current_score = 0;
    Bool matched = FALSE;
    auto term_entries = term_set->entries;
    for (auto ii = term_entries->next; ii != term_entries; ii = ii->next) {
      FzFTerm *term = ii->value(FzFTerm *);
      auto fzf_str = term->text(FzFString *);

      FzFResult res;
      term->fn(&res,term->case_sensitive,&input,fzf_str,NULL,slab);

      if (res.start >= 0) {
        if (term->inv) {
          continue;
        }
        current_score = res.score;
        matched = TRUE;
        break;
      }

      if (term->inv) {
        current_score = 0;
        matched = TRUE;
      }
    }
    if (matched) {
      total_score += current_score;
    } else {
      total_score = 0;
      break;
    }
  }

  return total_score;
}

U0 FzFPositionRelease(FzFPosition *pos)
{
  if (pos) {
    if (pos->data) {
      Free(pos->data);
    }
    Free(pos);
  }
}

FzFPosition *FzFGetPositions(U8 *text, FzFPattern *pattern, FzFSlab *slab)
{
  FzFString input;
  FzFResult res;
  FzFPosition *all_pos;
  FzFTermSet *term_set;
  FzFString *fzf_str;
  FzFTerm *term;
  Bool matched;

  if (text == NULL || ListEmpty(pattern->entries)) return NULL;

  input.data = text;
  input.size = StrLen(text);
  all_pos = FzFPosArray(0);
  auto entries = pattern->entries;

  for (auto it = entries->next; it != entries; it = it->next) {
    term_set = it->value(FzFTermSet *);
    matched = FALSE;

    auto term_entries = term_set->entries;
    for (auto ii = term_entries->next; ii != term_entries; ii = ii->next) {
      term = ii->value(FzFTermSet *);
      fzf_str = term->text(FzFString *);

      if (term->inv) {
        term->fn(&res,term->case_sensitive,&input,fzf_str,NULL,slab);
        if (res.start < 0) {
          matched = TRUE;
        }
        continue;
      }

      term->fn(&res,term->case_sensitive,&input,fzf_str,all_pos,slab);
      if (res.start >= 0) {
        matched = TRUE;
        break;
      }
    }
    if (!matched) {
      FzFPositionRelease(all_pos);
      return NULL;
    }
  }
  return all_pos;
}

FzFSlab *FzFSlabNew(FzFSlabConfig *config)
{
  FzFSlab *slab = CAlloc(sizeof(FzFSlab));

  slab->i16.data = CAlloc(config->size_16 * sizeof(I16));

  slab->i16.cap = config->size_16;
  slab->i16.size = 0;
  slab->i16.allocated = TRUE;

  slab->i32.data = CAlloc(config->size_32 * sizeof(I32));
  slab->i32.cap = config->size_32;
  slab->i32.size = 0;
  slab->i32.allocated = TRUE;

  return slab;
}

FzFSlab *FzFDefaultSlab()
{
  FzFSlabConfig config;
  config.size_16 = (1<<10) * 100;
  config.size_32 = 1<<11;
  return FzFSlabNew(&config);
}

U0 FzFSlabRelease(FzFSlab *slab)
{
  if (slab) {
    Free(slab->i16.data);
    Free(slab->i32.data);
    Free(slab);
  }
}

public FzFPosition *FzF(U8 *text, U8 *seq, I32 case_mode=FZF_CASE_SMART,
    Bool fuzzy=TRUE)
{
  auto slab = FzFDefaultSlab();
  auto pattern = FzFParsePattern(case_mode,seq,fuzzy);
  if (!pattern) {
    return NULL;
  }
  auto score = FzFGetScore(text,pattern,slab);
  auto pos = FzFGetPositions(text,pattern,slab);
  if (pos) {
    pos->score = score;
  }

  FzFPatternRelease(pattern);
  FzFSlabRelease(slab);
  return pos;
}
