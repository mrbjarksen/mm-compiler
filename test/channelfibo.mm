fun main()
{
	var tmp,c,f1,f2,i;
	c = makeChannel();
	go
	{
		f1 = 1;
		f2 = bigInteger(1);
		while(true)
		{
			c <- f1;
			tmp = f1+f2;
			f1 = f2;
			f2 = tmp;
		};
	};
	i = 0;
	while( i<100 )
	{
		i = inc(i);
		writeln(<-c);
	};
};

main();
