#include "aostr.h"
#include "util.h"
#include "debug.h"

AoStr *debugColourInt(long value) {
    if (is_terminal) {
        return aoStrPrintf(ESC_YELLOW"%ld"ESC_RESET, value);
    } else {
        return aoStrPrintf("%ld", value);
    }
}

AoStr *debugColourIntAsPtr(void *value) {
    return debugColourInt((long)value);
}

AoStr *debugColourFloat(double value) {
    if (is_terminal) {
        return aoStrPrintf(ESC_YELLOW"%f"ESC_RESET, value);
    } else {
        return aoStrPrintf("%f", value);
    }
}

AoStr *debugColourAoStr(AoStr *str) {
    if (is_terminal) {
        return aoStrPrintf(ESC_GREEN"\"%s\""ESC_RESET, str->data);
    } else {
        return aoStrPrintf("\"%s\"", str->data);
    }
}

void debugVectorAoStrStringify(AoStr *buf, AoStr *value) {
    if (is_terminal) {
        aoStrCatFmt(buf, ESC_GREEN"\"%S\""ESC_RESET, value);
    } else {
        aoStrCatFmt(buf, "\"%S\"", value);
    }
}
