var test_description = "Testing Constant16 opcode with many constants.";

fn test_constant16_usage() {
  // Test that we can handle many constants (this should use Constant16 for some)
  var sum = 100.1 + 100.2 + 100.3 + 100.4 + 100.5 + 100.6 + 100.7 + 100.8 + 100.9 + 101.0 +
            101.1 + 101.2 + 101.3 + 101.4 + 101.5 + 101.6 + 101.7 + 101.8 + 101.9 + 102.0 +
            102.1 + 102.2 + 102.3 + 102.4 + 102.5 + 102.6 + 102.7 + 102.8 + 102.9 + 103.0 +
            103.1 + 103.2 + 103.3 + 103.4 + 103.5 + 103.6 + 103.7 + 103.8 + 103.9 + 104.0 +
            104.1 + 104.2 + 104.3 + 104.4 + 104.5 + 104.6 + 104.7 + 104.8 + 104.9 + 105.0;
  
  // Sum should be approximately 50 * (100.1 + 105.0) / 2 = 50 * 102.55 = 5127.5
  assert_eq(sum, 5127.5);
}

fn test_mixed_constants() {
  // Test mixing strings and numbers to verify string-first allocation works
  var str1 = "hello";
  var num1 = 42.5;
  var str2 = "world";
  var num2 = 17.3;
  
  var result = str1 + str2;
  var numResult = num1 + num2;
  
  assert_eq(result, "helloworld");
  assert_eq(numResult, 59.8);
}