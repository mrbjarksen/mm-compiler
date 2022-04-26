var firstdone, seconddone;

go {
	write("these\n");
	write("words\n");
	write("probably\n");
	write("wont\n");
	write("appear\n");
	write("in\n");
	write("order\n");
	firstdone = true;
};

go {
	write("since\n");
	write("both\n");
	write("bodies\n");
	write("are\n");
	write("being\n");
	write("executed\n");
	write("at\n");
	write("once\n");
	seconddone = true;
};

while (!firstdone || !seconddone) {null};
writeln("and now both are done");
