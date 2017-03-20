package recfun

object Main {
  /**def main(args: Array[String]) {
     println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Parenthesis balancing")
    var st = "())("
    if(balance(st.toList)) println("yay") else println("no")

    println("Count change")
    println(countChange(4, List(1,2))) 
  }
  */

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c < 0 || r < 0)
        throw new IllegalArgumentException("Inputs less than 0 are not allowed")
      else {
        if(c == 0 || c == r)
          1
        else
          pascal(c-1, r-1) + pascal(c, r-1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      var ct=0
      def bal(lst: List[Char]): Boolean = {
        if(lst.isEmpty) {
          if(ct == 0) return true else return false
        }
        if(lst.head == '(' ) ct+=1
        else if(lst.head == ')' ) {
          if(ct > 0) ct-=1 else return false
        }
        bal(lst.tail)
      }
      bal(chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money < 0 || coins.isEmpty) return 0
      if(money == 0)return 1
      countChange(money-coins.head, coins) + countChange(money, coins.tail)
    }
  }
