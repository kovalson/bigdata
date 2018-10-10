class Complex( val re: Double, val im: Double ) {
	override def toString(): String = {
		re + (if (im < 0) (" - " + -im) else (" + " + im)) + "*i"
	}

	def +( c: Complex ): Complex = {
		new Complex( re + c.re, im + c.im )
	}

	def *( c: Complex ): Complex = {
		new Complex( re * c.re - im * c.im, im * c.re + re * c.im )
	}

	def modulus(): Double = {
		math.sqrt( math.pow( re, 2 ) + math.pow( im, 2 ) )
	}
}

var c1 = new Complex( 14, 0 )
var c2 = new Complex( 11, 0 )
println( (c1 * c2).modulus )