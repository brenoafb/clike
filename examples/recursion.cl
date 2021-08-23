func printInt(int x) : void {
  service(1, x);
}

func printChar(byte x) : void {
  service(3, x);
}

func printNewline() : void {
  printChar(10);
}

func facrec(int x) : int {
  if (x <= 1) {
    return 1;
  }

  return x * facrec(x - 1);
}

func main() : void {
  int f;

  f = facrec(10);

  printInt(f);
  printNewline();
}
