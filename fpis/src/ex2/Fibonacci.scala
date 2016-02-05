package ex2

/**
 * @author CHAOLUN
 * 
 * FPIS ex 2.1
 */
object Fibonacci {
  def fib(n : Int) : Int = 
    if (n <= 1) n
    else fib(n-1) + fib(n-2)
  
  def fibTail(n: Int) : Int = {
      def go(n: Int, a: Int, b: Int) : Int = 
        if (n == 0) a
        else go(n - 1, b, a + b)
      return go(n, 0, 1)
  }
}