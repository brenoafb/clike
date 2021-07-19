import io;

func fac(int32 n) : int32 {
  int32 tmp;
  tmp = 1;

  while (n > 0) {
    tmp = tmp * n;
    n = n - 1;
  }
  return tmp;
}

func main() : void {
  int32 f;

  f = fac(10);

  print("The factorial of 10 is ");
  printint(f);
  println("");
}
