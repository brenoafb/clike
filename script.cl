import stdlib;

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

  printNewline();
}
