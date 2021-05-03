package pl.snipersoft.fppractice.strings

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RansomNoteSpec extends AnyFunSuite with Matchers {
  val ransomNotes = List("Aaabc2 defghh1", "aghh1bc2 daeAf", "a bc2 defg hhA a1")
  val validMagazines = List("Aaabc2 defghh1", "a hh1 bc2 defg Aa", "Aaabc2defghh1")
  val invalidMagazines = List("12", "aabcdefghh", "Aa2abcdfgh1h", "qwerty")

  ransomNotes.foreach(note => {
    validMagazines.foreach(magazine => {
      test(s"should create random note '$note' from '$magazine'") {
        StringUtils.canBuildRansomNote(note, magazine) shouldBe true
      }
    })
    invalidMagazines.foreach(magazine => {
      test(s"should not create random note '$note' from '$magazine'") {
        StringUtils.canBuildRansomNote(note, magazine) shouldBe false
      }
    })
  })
}
