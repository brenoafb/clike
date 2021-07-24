import io;

func printInt(int x) : void {
  service(1, x);
}

func main() : void {
  int c;

  c = 100;

  if (c == 0) {
    printInt(1);
  } else {
    printInt(2);
  }

  printInt(c);
}
