import io;

func printInt(byte x) : void {
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

func println() : void {
  printChar(10);
}

func main() : void {
  str message;
  str sep;
  ptr arr;
  int n;
  byte f;

  message = "Here are some numbers";
  sep = " ";

  arr = 128;
  n = 0;
  f = 0;

  while (n < 10) {
    writeByte(arr, f);
    f = f + 1;
    arr = arr + 1;
    n = n + 1;
  }

  printStr(message);

  arr = 128;
  n = 0;
  while (n < 10) {
    printStr(sep);
    f = readByte(arr);
    printByte(f);
    arr = arr + 1;
    n = n + 1;
  }

  println();
}
