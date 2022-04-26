fun f() { return "this is f" };
writeln(f());

writeln((fun () { return "this is a closure" })());
