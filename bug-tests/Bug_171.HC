U0 FN()
{
  "FN()\n";
}

U0 PrintMessage(U8 *msg,
                I64 i,
                U0 (*fn)(I64 x, I64 y) = NULL,
                I64 N)
{
  if (fn) fn();
  "%s i=%d N=%d\n",msg, i, N;
}

U0 Main()
{
  "Starting\n";
  PrintMessage("hello!",24,,4);
}
