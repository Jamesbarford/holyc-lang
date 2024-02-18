public U0 PrintResult(I64 correct, I64 total) {
  if (correct != total) "\033[0;31mFAILED: %d/%d\033[0;0m\n", correct, total;
  else "\033[0;32mPASSED: %d/%d\033[0;0m\n", correct, total;
}
