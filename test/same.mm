fun bintree(i,j)
{
	var k;
	if( i>=j ) {return null;};
	k = i+random(j-i);
	return [k,bintree(i,k),bintree(k+1,j)];
};

fun traverse(c,x)
{
	if (x)
	{
		traverse(c,cadr(x));
		c <- car(x);
		traverse(c,caddr(x));
	};
};

fun same(t1,t2)
{
	var c1,c2,x1,x2;
	c1 = makeChannel();
	c2 = makeChannel();
	go
	{
		traverse(c1,t1);
		c1 <- channelEOF();
	};
	go
	{
		traverse(c2,t2);
		c2 <- channelEOF();
	};
	while (true)
	{
		x1 = <- c1;
		x2 = <- c2;
		if (isChannelEOF(x1)) {return isChannelEOF(x2);};
		if (isChannelEOF(x2)) {return false;};
		if (x1!=x2) {return false;};
	};
};

fun main()
{
	writeln(same(bintree(1,10),bintree(1,10)));
	writeln(same(bintree(100,10000),bintree(100,10000)));
	writeln(same(bintree(10,10000),bintree(100,10000)));
	writeln(same(bintree(100,1000000),bintree(100,1000000)));
	writeln(same(bintree(100,1000001),bintree(100,1000000)));
};

main();

