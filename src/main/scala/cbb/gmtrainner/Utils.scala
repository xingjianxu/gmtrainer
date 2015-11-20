package cbb.gmtrainner

import cbb.gmtrainner.Strand._

object Utils {
  def extactSeq(seq: String, feature: Feature): String = {
    extactSeq(seq, feature.start, feature.end, feature.strand)
  }

  def extactSeq(seq: String, start: Int, end: Int, strand: Strand.Value): String = {
    val subseq = seq.substring(start-1, end)
    if (strand == ANTISENSE) {
      complementReverseSeq(subseq)
    } else {
      subseq
    }
  }

  def complementReverseSeq(seq: String): String = {
    seq.reverse.map {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'C' => 'G'
      case 'G' => 'C'
      case x => x
    }
  }

  def printSeq(seq: String): Unit = {
    seq.grouped(60).foreach(println(_))
  }
}
