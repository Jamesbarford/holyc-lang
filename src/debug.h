#ifndef DEBUG_H__
#define DEBUG_H__

#include "aostr.h"

AoStr *debugColourInt(long value);
AoStr *debugColourIntAsPtr(void *value);
AoStr *debugColourFloat(double value);
AoStr *debugColourAoStr(AoStr *str);
void debugVectorAoStrStringify(AoStr *buf, AoStr *value);

#endif
