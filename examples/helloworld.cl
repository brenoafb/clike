import io;

func printint(int x) : void {
  service(1, x);
}

func println(str x) : void {
  service(5, x);
}

func main() : void {
  str message1;
  str message2;
  str message3;

  message1 = "Hello";
  message2 = "World";
  message3 = "Neat!";

  println(message1);
  println(message2);
}
