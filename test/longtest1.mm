fun f(n)
{
	if( n<2 )
	{
		1;
	}
	else
	{
		f(n-1)+f(n-2);
	};
};

writeln(f(33));

f = fun(n)
{
	var i=0,f1=1,f2=1;
	while( i!=n )
	{
		var tmp = f1+f2;
		f1 = f2;
		f2 = tmp;
		i = inc(i);
	};
	f1;
};
writeln(f(33));

f = fun(n)
{
	var i = 0, c = makeChannel();
	startTask(
		fun()
		{
			var f1=1,f2=1;
			while( true )
			{
				var tmp=f1+f2;
				c <- f1;
				f1 = f2;
				f2 = tmp;
			};
		}
	);
	while( i!=n )
	{
		<- c;
		i = inc(i);
	};
	<- c;
};

writeln(f(33));

fun(x){writeln("Halló "++x);}("veröld");

fun inorder(x)
{
	var c,inner;
	c = makeChannel();
	inner = fun(x)
	{
		if( x )
		{
			inner(cadr(x));
			c <- car(x);
			inner(caddr(x));
		};
	};
	startTask(fun(){inner(x);c<-channelEOF();});
	c;
};

fun randomTree(n)
{
	fun inner(i,j)
	{
		if( i==j ) {return null;};
		var k;
		k = i+random(j-i);
		return [k,inner(i,k),inner(k+1,j)];
	};
	return inner(0,n);
};

var c;
var t;
t = randomTree(2000);
;;;writeln(t);
c = inorder(t);
var x;
x = <-c;
while( x!=channelEOF() )
{
	write(x); write(" ");
	x = <- c;
};
writeln();

fun summa(n)
{
	var c;
	c = makeBufferedChannel(1000);
	;;;c = makeChannel();
	fun produce()
	{
		var i;
		i = 0;
		while( i!=n )
		{
			i = i+1;
			c <- i;
		};
		c <- channelEOF();
	};
	startTask(produce);
	var sum,x;
	sum = bigInteger(0);
	while( !isChannelEOF(x= <-c) )
	{
		sum = sum+x;
	};
	sum;
};
var start,end,res,n;
n = 100000000;
start = nanoTime();
res = summa(n);
end = nanoTime();
writeln(n++"+...+3+2+1="++res);
writeln(format("Channel speed: %.0f messages per second",n/((end-start)*1.0e-9)));

