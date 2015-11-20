package cbb.gmtrainner

import java.util.regex.Pattern

trait Matrices {
  val FILTER = Pattern.compile("[ACTG]+")
  def process(seqs: List[String])
}
