import io;

func fac(int n) : int {
  int tmp;
  tmp = 1;

  while (n > 0) {
    tmp = tmp * n;
    n = n - 1;
  }
  return tmp;
}

func main() : void {
  int f;
  str message;
  str empty;

  message = "The factorial of 10 is ";
  empty = "";

  f = fac(10);

  print(message);
  printint(f);
  println(empty);
}
