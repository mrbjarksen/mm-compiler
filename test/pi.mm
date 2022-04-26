fun ^(x,n)
{
	if( n==0 ) { return 1; };
	if( n%2==0 ) { return (x*x)^(n/2); };
	return x*((x*x)^(n/2));
};

fun arctanrev(h,x)
{
	var summa,xx,i;
	i = 1;
	xx = x*x;
	summa = h = h/x;
	while( h!=0 )
	{
		i = i+2;
		h = h/xx;
		summa = summa-h/i;
		i = i+2;
		h = h/xx;
		summa = summa+h/i;
	};
	summa;
};

var n = int(arrayGet(getArgs(),1));
var h = bigInteger(10)^(n+6);
var pi = 4*(4*arctanrev(h,5)-arctanrev(h,239));
write("π = 3.");
writeln(((pi-3*h)/1000/100+5)/10);

