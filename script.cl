import io;

func sumOfSquares(int x, int y) : int {
  return x*x + y*y;
}

func main() : void {
  int x;
  int y;
  int z;

  x = 3;
  y = 4;
  z = sumOfSquares(x, y);
}
