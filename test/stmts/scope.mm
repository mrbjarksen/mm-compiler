var a = 'a', b = 'b';
writeln("outer: " ++ a ++ " " ++ b);

if (true) {
	var a = 0;
	writeln("inner: " ++ a ++ " " ++ b);
};

writeln("outer again: " ++ a ++ " " ++ b);
