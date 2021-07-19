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

  f = fac(10);

  print("The factorial of 10 is ");
  printint(f);
  println("");
}
