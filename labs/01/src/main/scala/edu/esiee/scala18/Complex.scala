package edu.esiee.scala18

object Complex {
  def apply(re: Double, im: Double) = new Complex(re, im)
}

class Complex(val re: Double, val im: Double) {

  def + (b: Complex) = Complex(this.re + b.re, this.im + b.im)
  def - (b: Complex) = Complex(this.re - b.re, this.im - b.im)
  def * (b: Complex) = Complex(this.re * b.re - (this.im*b.im), this.re*b.im + this.im*b.re)
  def unary_- () = Complex(-this.re, -this.im)
  def % (m: Int) = Complex(this.re % m, this.im % m)
  def / (b: Complex) = Complex(this.re/b.re, this.im/b.im)
  override def toString = s"$re+i$im"
}
