fun w(m,s)
{
	<- m;
	writeln(s);
	m <- "release";
};

fun say(m,s,done)
{
	var i;
	i = 0;
	while (i<10)
	{
		sleep(random(2)/1000.0);
		w(m,s);
		i = inc(i);
	};
	done <- 0;
};

fun main()
{
	var mutex,done;
	mutex = makeChannel();
	mutex <- "release";
	done = makeChannel();
	go {say(mutex,"What?",done);};
	go {say(mutex,"Hello",done);};
	startTask(fun(){say(mutex,"World",done);});
	<- done;
	<- done;
	<- done;
};

main();
