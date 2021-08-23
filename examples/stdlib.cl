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

func printNewline() : void {
  printChar(10);
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
