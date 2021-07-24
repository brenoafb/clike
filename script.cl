import io;

func printByte(byte x) : void {
  service(2, x);
}

func readByte(ptr addr) : byte {
  return service(12, addr);
}

func writeByte(ptr addr, byte b) : void  {
  service(14, addr, b);
}

func main() : void {
  byte c;

  writeByte(0, 1);
  c = readByte(0);

  printByte(c);
}
