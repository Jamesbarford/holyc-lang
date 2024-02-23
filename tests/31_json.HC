#include "testhelper.HC"

U0 Main()
{
  "Test - JSON parsing: ";
  I32 tests = 10, correct = 0;
  U8 *raw = "{
    \"hello\": 10,
    \"key\": \"some string\",
    \"id\": null,
    \"bool2\": false,
    \"bool\": true,
    \"array\": [1,   30030303  ,   430  ,

    -14]
  }";

  auto j = JsonParse(raw);
  if (j != NULL) correct++;

  auto obj = JsonSelect(j,".hello:i");
  if (obj) {
    if (obj->int == 10) correct++;
  }

  U8 *str = JsonToString(j);
  "%s\n",str;
  Free(str);

  if ((obj = JsonSelect(j,".key:s"))) {
    if (!StrNCmp(obj->str,"some string",11)) correct++;
  }

  if ((obj = JsonSelect(j,".id"))) {
    if (obj->type == JSON_NULL) correct++;
  }

  if ((obj = JsonSelect(j,".bool"))) {
    if (obj->boolean == TRUE) correct++;
  }

  if ((obj = JsonSelect(j,".bool2"))) {
    if (obj->boolean == FALSE) correct++;
  }

  if ((obj = JsonSelect(j,".array[0]:i"))) {
    if (obj->int == 1) correct++;
  }
  if ((obj = JsonSelect(j,".array[1]:i"))) {
    if (obj->int == 30030303) correct++;
  }
  if ((obj = JsonSelect(j,".array[2]"))) {
    if (obj->int == 430) correct++;
  }
  if ((obj = JsonSelect(j,".array[3]:i"))) {
    if (obj->int == -14) correct++;
  }

  JsonRelease(j);
  PrintResult(correct,tests);
  "====\n";
}
