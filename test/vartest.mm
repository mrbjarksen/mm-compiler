
fun f(n) {
	if (n == 0)
	{ 
		writeln("hello");
	}
	else 
	{
		writeln(n);
		f(n-1);
	}
};
f(4);
