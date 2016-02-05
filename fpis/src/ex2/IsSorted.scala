package fpis

/**
 * @author CHAOLUN
 * 
 * FIPS ex 2.2
 */
object IsSorted {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    if (as.size <= 1) true
    else ordered(as(0), as(1)) && isSorted(as.slice(1, as.size), ordered)
}