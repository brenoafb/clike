import io;

func printInt(int x) : void {
  service(1, x);
}

func main() : void {
  int c;
  int s;

  c = 10;
  s = 0;

  while (c != 0) {
    printInt(c);
    s = s + c;
    c = c - 1;
  }

  printInt(c);
  printInt(s);
}
