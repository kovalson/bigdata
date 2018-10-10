def gcd( x: Int, y: Int ): Int = {
	if( y == 0 ) x.abs
	else gcd( y, x % y )
}

def lcm( x: Int, y: Int ) = {
	(x * y).abs / gcd( x, y )
}

def tau( n: Int ) = {
	(1 to n).filter( i => n % i == 0 ).length
}

def sigma( n: Int ): Int = {
	(1 to n).filter( i => n % i == 0 ).sum
}

def sigma2( n: Int, k: Int ) = {
	(1 to n).filter( i => n % i == 0 ).map( i => math.pow( i, k ).toInt ).sum
}

def phi( n: Int ): Int = {
	(1 to n).filter( i => gcd( i, n ) == 1 ).length
}

def phi_test(): Int = {
	Range( 1, 101 ).filter( x => 100 % x == 0 ).map( x => phi( x ) ).sum
}

def isPrime( n: Int ): Boolean = {
	if( n < 2 ) false
	else (2 to n - 1).filter( i => n % i == 0 ).length == 0
}

def isPrime_test(): Int = {
	Range( 1, 1000 ).filter( isPrime ).length
}

println( "GCD", gcd( -15, -25 ) )
println( "LCM", lcm( 12, 18 ) );
println( "tau", tau( 10 ) )
println( "sigma", sigma( 10 ) )
println( "sigma2", sigma2( 10, 2 ) )
println( "phi", phi( 100 ) )
println( "phi_test", phi_test )
println( "isPrime", isPrime( 1 ) )
println( "isPrime_test", isPrime_test )