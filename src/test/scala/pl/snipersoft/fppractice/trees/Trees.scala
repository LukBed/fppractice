package pl.snipersoft.fppractice.trees

object Trees {
  val aba = BNode("aba")
  val ab = BNode("ab", aba)
  val aaab = BNode("aaab")
  val aaaa = BNode("aaaa")
  val aab = BNode("aab")
  val aaa = BNode("aaa", aaaa, aaab)
  val aa = BNode("aa", aaa, aab)
  val a: BTree[String] = BNode("a", aa, ab)
}
