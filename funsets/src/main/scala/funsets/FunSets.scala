package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
    def singletonSet(elem: Int): Set = {
      def singletonSetF(elemF: Int) : Boolean = (elem == elemF)
      singletonSetF
    }
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
    def union(s: Set, t: Set): Set = {
      def unionF(elemF: Int) = s(elemF) || t(elemF)
      unionF
    }
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = {
      def intersectF(elemF: Int) = s(elemF) && t(elemF)
      intersectF
    }
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = {
      def diffF(elemF: Int) : Boolean = s(elemF) && !t(elemF)
      diffF
    }
    
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = {
      def filterF(elemF: Int) : Boolean = {
        if(s(elemF)) return p(elemF)
        false
      }
      filterF
    }
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -bound) true
      else if (contains(s,a)) if(p(a)) iter(a-1) else false
      else iter(a-1)
    }
    iter(bound)
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Set, p: Int => Boolean): Boolean = {
      def iter(a: Int): Boolean = {
        if(a < -bound) false
        else if (contains(s,a)) if(p(a)) true else iter(a-1)
        else iter(a-1)
      }
      iter(bound)
    }
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: Int => Int): Set = {
      def mapF(a: Int) : Boolean = {
        def work(b: Int) : Boolean = {
          if(b < -bound) false
          else if(contains(s,b)) if(f(b) == a) true else work(b-1)
          else work(b-1)
        }
        work(bound)
      }
      mapF
    }
  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
