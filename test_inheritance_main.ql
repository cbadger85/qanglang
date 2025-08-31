class BaseClass {
    baseMethod001() { return "base1"; }
    baseMethod002() { return "base2"; }
    baseMethod003() { return "base3"; }
    baseMethod004() { return "base4"; }
    baseMethod005() { return "base5"; }
}

class DerivedClass : BaseClass {
    derivedMethod001() { return super.baseMethod001(); }
    derivedMethod002() { return super.baseMethod002(); }
    derivedMethod003() { return super.baseMethod003(); }
    derivedMethod004() { return super.baseMethod004(); }
    derivedMethod005() { return super.baseMethod005(); }
}

var obj = DerivedClass();
println("Testing method 5...");
println(obj.derivedMethod005());