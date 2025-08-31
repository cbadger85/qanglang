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

fn test_many_identifiers() {
  // Test that we can handle many identifiers (should trigger 16-bit identifier opcodes)
  var id001 = 1; var id002 = 2; var id003 = 3; var id004 = 4; var id005 = 5;
  var id006 = 6; var id007 = 7; var id008 = 8; var id009 = 9; var id010 = 10;
  var id011 = 11; var id012 = 12; var id013 = 13; var id014 = 14; var id015 = 15;
  var id016 = 16; var id017 = 17; var id018 = 18; var id019 = 19; var id020 = 20;
  var id021 = 21; var id022 = 22; var id023 = 23; var id024 = 24; var id025 = 25;
  var id026 = 26; var id027 = 27; var id028 = 28; var id029 = 29; var id030 = 30;
  var id031 = 31; var id032 = 32; var id033 = 33; var id034 = 34; var id035 = 35;
  var id036 = 36; var id037 = 37; var id038 = 38; var id039 = 39; var id040 = 40;
  var id041 = 41; var id042 = 42; var id043 = 43; var id044 = 44; var id045 = 45;
  var id046 = 46; var id047 = 47; var id048 = 48; var id049 = 49; var id050 = 50;
  
  // Test global variable access with many identifiers
  var sum = id001 + id002 + id003 + id004 + id005 + id006 + id007 + id008 + id009 + id010 +
            id011 + id012 + id013 + id014 + id015 + id016 + id017 + id018 + id019 + id020 +
            id021 + id022 + id023 + id024 + id025 + id026 + id027 + id028 + id029 + id030 +
            id031 + id032 + id033 + id034 + id035 + id036 + id037 + id038 + id039 + id040 +
            id041 + id042 + id043 + id044 + id045 + id046 + id047 + id048 + id049 + id050;
  
  assert_eq(sum, 1275); // Sum of 1 to 50 = 50 * 51 / 2 = 1275
}

class TestClass {
  prop001 = 1; prop002 = 2; prop003 = 3; prop004 = 4; prop005 = 5;
  prop006 = 6; prop007 = 7; prop008 = 8; prop009 = 9; prop010 = 10;
  prop011 = 11; prop012 = 12; prop013 = 13; prop014 = 14; prop015 = 15;
  prop016 = 16; prop017 = 17; prop018 = 18; prop019 = 19; prop020 = 20;
  prop021 = 21; prop022 = 22; prop023 = 23; prop024 = 24; prop025 = 25;
  prop026 = 26; prop027 = 27; prop028 = 28; prop029 = 29; prop030 = 30;
  
  method001() { return this.prop001; }
  method002() { return this.prop002; }
  method003() { return this.prop003; }
  method004() { return this.prop004; }
  method005() { return this.prop005; }
  method006() { return this.prop006; }
  method007() { return this.prop007; }
  method008() { return this.prop008; }
  method009() { return this.prop009; }
  method010() { return this.prop010; }
  method011() { return this.prop011; }
  method012() { return this.prop012; }
  method013() { return this.prop013; }
  method014() { return this.prop014; }
  method015() { return this.prop015; }
  method016() { return this.prop016; }
  method017() { return this.prop017; }
  method018() { return this.prop018; }
  method019() { return this.prop019; }
  method020() { return this.prop020; }
}

fn test_class_with_many_properties() {
  // Test property access and method calls with many identifiers
  var obj = TestClass();
  
  // Test property access (GetProperty16/SetProperty16)
  assert_eq(obj.prop001, 1);
  assert_eq(obj.prop020, 20);
  assert_eq(obj.prop030, 30);
  
  // Test property assignment
  obj.prop001 = 100;
  obj.prop020 = 200;
  obj.prop030 = 300;
  
  assert_eq(obj.prop001, 100);
  assert_eq(obj.prop020, 200);
  assert_eq(obj.prop030, 300);
  
  // Test method calls (Invoke16)
  assert_eq(obj.method001(), 100);
  assert_eq(obj.method020(), 200);
}

fn test_object_literals_many_keys() {
  // Test object literals with many keys
  var obj = {{
    key001 = "value1",
    key002 = "value2",
    key003 = "value3",
    key004 = "value4",
    key005 = "value5",
    key006 = "value6",
    key007 = "value7",
    key008 = "value8",
    key009 = "value9",
    key010 = "value10",
    key011 = "value11",
    key012 = "value12",
    key013 = "value13",
    key014 = "value14",
    key015 = "value15",
    key016 = "value16",
    key017 = "value17",
    key018 = "value18",
    key019 = "value19",
    key020 = "value20"
  }};
  
  // Test property access on object literals
  assert_eq(obj.key001, "value1");
  assert_eq(obj.key010, "value10");
  assert_eq(obj.key020, "value20");
  
  // Test property assignment on object literals
  obj.key001 = "modified1";
  obj.key020 = "modified20";
  
  assert_eq(obj.key001, "modified1");
  assert_eq(obj.key020, "modified20");
}

class BaseClass {
  baseMethod001() { return "base1"; }
  baseMethod002() { return "base2"; }
  baseMethod003() { return "base3"; }
  baseMethod004() { return "base4"; }
  baseMethod005() { return "base5"; }
  baseMethod006() { return "base6"; }
  baseMethod007() { return "base7"; }
  baseMethod008() { return "base8"; }
  baseMethod009() { return "base9"; }
  baseMethod010() { return "base10"; }
}

class DerivedClass : BaseClass {
  derivedMethod001() { return super.baseMethod001(); }
  derivedMethod002() { return super.baseMethod002(); }
  derivedMethod003() { return super.baseMethod003(); }
  derivedMethod004() { return super.baseMethod004(); }
  derivedMethod005() { return super.baseMethod005(); }
  derivedMethod006() { return super.baseMethod006(); }
  derivedMethod007() { return super.baseMethod007(); }
  derivedMethod008() { return super.baseMethod008(); }
  derivedMethod009() { return super.baseMethod009(); }
  derivedMethod010() { return super.baseMethod010(); }
}

fn test_super_calls_many_methods() {
  // Test super method calls with many identifiers (GetSuper16/SuperInvoke16)
  var obj = DerivedClass();
  
  assert_eq(obj.derivedMethod001(), "base1");
  assert_eq(obj.derivedMethod002(), "base2");
  assert_eq(obj.derivedMethod003(), "base3");
  assert_eq(obj.derivedMethod004(), "base4");
  assert_eq(obj.derivedMethod005(), "base5");
  assert_eq(obj.derivedMethod006(), "base6");
  assert_eq(obj.derivedMethod007(), "base7");
  assert_eq(obj.derivedMethod008(), "base8");
  assert_eq(obj.derivedMethod009(), "base9");
  assert_eq(obj.derivedMethod010(), "base10");
}