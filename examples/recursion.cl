import stdlib;

func facrec(int x) : int {
  if (x <= 1) {
    return 1;
  }

  return x * facrec(x - 1);
}

func main() : void {
  int f;
  str message;

  f = facrec(10);
  message = "the factorial of 10 is ";

  printStr(message);
  printInt(f);
  printNewline();
}
