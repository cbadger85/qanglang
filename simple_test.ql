// Simple loop test without recursion
var i = 0;
var start = system_time();
while (i < 1000000) {
    i = i + 1;
}
print("Loop completed in ");
print(system_time() - start);
print("ms");
println("");