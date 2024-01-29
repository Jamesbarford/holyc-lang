#include <memory.HC>
#include <strings.HC>

U8 *ptr = MAlloc(2);
I64 len = 0;
ptr = ReAlloc(ptr,4);
MemCpy(ptr,"hey",3);
len = 3;
ptr[len] = '\0';
"%s\n",ptr;
ptr = ReAlloc(ptr,12);
MemCpy(ptr+len,"woof",4);
ptr[len+4] = '\0';
"%s\n",ptr;
U8 *s = StrPrint(NULL,"%2d",40000);
U8 *s2 = StrPrint(NULL,"%X",0x2d);
"%s\n",s2;

F64 f = 3.142;
U8 *s3 = StrPrint(NULL,"%f",f);
"%f\n",3.2121 + 5.1;
