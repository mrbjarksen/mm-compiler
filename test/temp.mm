;;; Notkun: x = fibo(n);
;;; Fyrir:  n er heiltala, 0 <= n.
;;; Eftir:  x er n-ta Fibonacci talan.
fun fibo(n)
{
	var i,f1,f2,tmp;
	f1 = 1;
	f2 = 1;
	i = 0;
	while( i!=n )
	{
		;;; 0 <= i <= n.
		;;; f1 er i-ta Fibonacci talan.
		;;; f2 er (i+1)-ta Fibonacci talan.
		tmp = f1+f2;
		f1 = f2;
		f2 = tmp;
		i = i+1;
	};
	f1;
};

;;; Notkun: x = f(n);
;;; Fyrir:  n er heiltala, 0 <= n.
;;; Eftir:  x er n-ta Fibonacci talan.
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

fun main()
{
	writeln(1:2:3:null);
	writeln("fibo(35)="++fibo(35));
	writeln("fibo(35)="++f(35));
	writeln(1==2);
};

main()

