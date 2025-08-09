var value = "value";

fn concat_value(str) {
    return value + str;
}

for (var i = 0; i < 10; i = i + 1) {
    value = concat_value(value);
}

println(value);