fun feedList(c,x)
{
	while (x)
	{
		c <- head(x);
		sleep(1.0);
		x = tail(x);
	};
};

fun traverse(c,x)
{
	if (isPair(x))
	{
		traverse(c,head(x));
		traverse(c,tail(x));
	}
	elsif (x!=[])
	{
		c <- x;
	};
};

fun ss(x,y)
{
	if (x)
	{
		ss(tail(x),head(x):y);
	}
	else
	{
		y;
	};
};

fun rev(x)
{
	var y;
	while (x)
	{
		y = head(x):y;
		x = tail(x);
	};
	y;
};

fun i(n)
{
	var x;
	while (n>0)
	{
		x = n:x;
		n = dec(n);
	};
	x;
};

fun range(i,j)
{
	var c, k;
	c = makeChannel();
	go
	{
		k = i;
		while (k<=j)
		{
			c <- k;
			k = inc(k);
		};
		c <- channelEOF();
	};
	c;	
};

fun fibo(n)
{
	var i,f1,f2,tmp;
	f1 = 1;
	f2 = 1;
	i = 0;
	while( i!=n )
	{
		tmp = f1+f2;
		f1 = f2;
		f2 = tmp;
		i = i+1;
	};
	f1;
};

fun f(n)
{
	if( n==0 )
	{
		1;
	}
	elsif( n==1 )
	{
		1;
	}
	else
	{
		f(n-1) + f(n-2);
	};
};

fun main()
{
	var x,y,s,c;
	writeln(true&&true);
	writeln(true&&false);
	writeln(false&&false);
	writeln(!(true&&false));
	writeln(!(true||false));
	x = makeChannel();

	go
	{
		feedList(x,["a","b","c","d","e"]);
		x <- channelEOF();
	};
	while (!isChannelEOF(y = <-x))
	{
		writeln(y);
	};

	go
	{
		traverse(x,["a",["b","c"],["d",["e"]]]);
		x <- channelEOF();
	};
	while (!isChannelEOF(y = <-x))
	{
		writeln(y);
	};

	x = [1,2,3];
	y = i(3);
	write(x); write(" == "); write(y); writeln(" ?");
	write(ss(x,null)); writeln(" == [3,2,1] ?");
	write(rev(x)); writeln(" == [3,2,1] ?");
	write(2+3*4); writeln(" == 14 ?");
	write(2*3+4); writeln(" == 10 ?");
	write(1-2-3); writeln(" == -4 ?");
	writeln("fibo(20) = "++fibo(20));
	writeln("fibo(20) = "++f(20));
	s = bigInteger(0);
	c = makeChannel();

	go
	{
		traverse(c,i(1000000));
		c <- channelEOF();
	};
	while (!isChannelEOF(x = <-c))
	{
		s = s+x;
	};
	writeln("1+2+...+1000000="++s);

	c = range(1,1000000);
	s = bigInteger(0);
	while (!isChannelEOF(x = <-c))
	{
		s = s + x;
	};
	writeln("1+2+...+1000000="++s);
};

main();
