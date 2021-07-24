import io;

func printInt(int x) : void {
  service(1, x);
}

func printByte(byte x) : void {
  service(2, x);
}

func printChar(byte x) : void {
  service(3, x);
}

func readByte(ptr addr) : byte {
  return service(12, addr);
}

func writeByte(ptr addr, byte b) : void  {
  service(14, addr, b);
}

func printStr(ptr addr) : void {
  byte b;

  b = readByte(addr);

  while (b) {
    printChar(b);
    addr = addr + 1;
    b = readByte(addr);
  }
}

func printStrLn(ptr addr) : void {
  byte nl;
  nl = 10;
  printStr(addr);
  printChar(nl);
}

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
  byte nl;
  str message;
  str goodbye;
  nl = 10;

  message = "The factorial of 10 is ";
  goodbye = "Goodbye!";

  f = fac(10);

  printStr(message);
  printInt(f);
  printChar(nl);
  printStr(goodbye);
  printChar(nl);
}
