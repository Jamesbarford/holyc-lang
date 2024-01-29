#include <iolib.HC>

U0 Main()
{
  "TEST ESCAPE SEQUENCES:\n";
  print("Should be red: $RED$Hello$FG$\n");
  print("Should be green: $GREEN$Hello$FG$\n");
  "======\n";
}
