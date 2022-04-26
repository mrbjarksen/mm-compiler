fun f() { writeln("function 1"); };
f();

f = fun () { writeln("function 2"); };
f();

fun f() { writeln("function 3"); };
f();

fun ~(a, b) { abs(a - b) < 1.0e-3; };
writeln(1 ~ 2);
writeln(1 ~ 1.0005);
