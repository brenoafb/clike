func printInt(int x) : void {
  service(1, x);
}

func printByte(byte b) : void {
  service(2, b);
}

func printChar(byte c) : void {
  service(3, c);
}

func readInt(ptr addr) : int {
  return service(11, addr);
}

func readByte(ptr addr) : byte {
  return service(12, addr);
}

func writeInt(ptr addr, int x) : void {
  service(13, addr, x);
}

func writeByte(ptr addr, byte b) : void {
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

func printNewline() : void {
  printChar(10);
}
