var test_description = "Testing that constants index beyond 256 work correctly.";

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

fn test_constant16_usage() {
  var total = 000 + 001 + 002 + 003 + 004 + 005 + 006 + 007 + 008 + 009
            + 010 + 011 + 012 + 013 + 014 + 015 + 016 + 017 + 018 + 019
            + 020 + 021 + 022 + 023 + 024 + 025 + 026 + 027 + 028 + 029
            + 030 + 031 + 032 + 033 + 034 + 035 + 036 + 037 + 038 + 039
            + 040 + 041 + 042 + 043 + 044 + 045 + 046 + 047 + 048 + 049
            + 050 + 051 + 052 + 053 + 054 + 055 + 056 + 057 + 058 + 059
            + 060 + 061 + 062 + 063 + 064 + 065 + 066 + 067 + 068 + 069
            + 070 + 071 + 072 + 073 + 074 + 075 + 076 + 077 + 078 + 079
            + 080 + 081 + 082 + 083 + 084 + 085 + 086 + 087 + 088 + 089
            + 090 + 091 + 092 + 093 + 094 + 095 + 096 + 097 + 098 + 099
            + 100 + 101 + 102 + 103 + 104 + 105 + 106 + 107 + 108 + 109
            + 110 + 111 + 112 + 113 + 114 + 115 + 116 + 117 + 118 + 119
            + 120 + 121 + 122 + 123 + 124 + 125 + 126 + 127 + 128 + 129
            + 130 + 131 + 132 + 133 + 134 + 135 + 136 + 137 + 138 + 139
            + 140 + 141 + 142 + 143 + 144 + 145 + 146 + 147 + 148 + 149
            + 150 + 151 + 152 + 153 + 154 + 155 + 156 + 157 + 158 + 159
            + 160 + 161 + 162 + 163 + 164 + 165 + 166 + 167 + 168 + 169
            + 170 + 171 + 172 + 173 + 174 + 175 + 176 + 177 + 178 + 179
            + 180 + 181 + 182 + 183 + 184 + 185 + 186 + 187 + 188 + 189
            + 190 + 191 + 192 + 193 + 194 + 195 + 196 + 197 + 198 + 199
            + 200 + 201 + 202 + 203 + 204 + 205 + 206 + 207 + 208 + 209
            + 210 + 211 + 212 + 213 + 214 + 215 + 216 + 217 + 218 + 219
            + 220 + 221 + 222 + 223 + 224 + 225 + 226 + 227 + 228 + 229
            + 230 + 231 + 232 + 233 + 234 + 235 + 236 + 237 + 238 + 239
            + 240 + 241 + 242 + 243 + 244 + 245 + 246 + 247 + 248 + 249
            + 250 + 251 + 252 + 253 + 254 + 255 + 256 + 257 + 258 + 259
            + 260 + 261 + 262 + 263 + 264 + 265 + 266 + 267 + 268 + 269
            + 270 + 271 + 272 + 273 + 274 + 275 + 276 + 277 + 278 + 279
            + 280 + 281 + 282 + 283 + 284 + 285 + 286 + 287 + 288 + 289
            + 290 + 291 + 292 + 293 + 294 + 295 + 296 + 297 + 298 + 299
            + 300 + 301 + 302 + 303 + 304 + 305 + 306 + 307 + 308 + 309;
  
      var total_is_number = total is NUMBER;
      assert(total_is_number);
      assert_eq(total, 47895);
}