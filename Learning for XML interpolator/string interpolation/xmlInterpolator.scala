// This interpolator just returns an XML literal

object xmlInterpolator {
   
   implicit class xmlHelper (val sc: StringContext) extends AnyVal {
    def xml(args: Any*): scala.xml.Elem = {
        val strings = sc.parts.iterator
        val expressions = args.iterator
        var buf = new StringBuffer(strings.next)
        while (strings.hasNext) {
            buf append expressions.next
            buf append strings.next
        }
        scala.xml.XML.loadString(buf.toString)
    }
}
   
   
   
   def main (args: Array[String]) {
      val s = "yay"
      val x = xml"""<parent><test>$s"yo"</test></parent>"""
      println(x)
   }
}