var result = Err("I'm an error.");

when (result) {
  is Ok => println("I'm ok!"),
  is Err => println("I'm not ok :(")
};