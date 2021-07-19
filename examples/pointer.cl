include io;

func main() : void {
  ptr p;
  p = alloc(2);       // alloc a 2 byte array
  writeptr(p+1, 1);   // string length
  writeptr(p+1, 97);
  print_string(p);    // should print 'a'
  print_byte(p);      // should print '1'
  print_byte(p+1);    // should print '97'
}
