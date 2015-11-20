package cbb.gmtrainner

import scala.collection.mutable
import Strand._

case class Gene(accession: String, seq: String, features: List[Feature]) {
  var exons = mutable.ListBuffer[String]()
  var introns = mutable.ListBuffer[String]()

  for (i <- features.indices) {
    val feature = features(i)
    exons append Utils.extactSeq(seq, feature)
    if (i < features.size - 1) {
      val nextFeature = features(i+1)
      introns append Utils.extactSeq(seq, feature.end+1, nextFeature.start-1, feature.strand)
    }
  }

  val head = features.head
  val last = features.last

  val reverse = head.strand == ANTISENSE
  val tseq = if (reverse) Utils.complementReverseSeq(seq) else seq

  var utr5 = Utils.extactSeq(seq, 1, head.start-1, SENSE)
  var utr3 = Utils.extactSeq(seq, last.end+1, seq.length, SENSE)

  if (reverse) {
    exons = exons.reverse
    introns = introns.reverse
    utr5 = Utils.extactSeq(seq, last.end+1, seq.length, ANTISENSE)
    utr3 = Utils.extactSeq(seq, 1, head.start-1, ANTISENSE)
  }

  def cds_seq: String = exons.mkString

  def introns_seq: String = introns.mkString

  def intron(idx: Int): String = {
    introns(idx)
  }

  def donors(flank: Int):List[String] = {
    if (features.head.ftype == "Single") {
      return List[String]()
    }

    val fd = mutable.ListBuffer[String]()
    val q = "GT"
    var index = tseq.indexOf(q)
    while (index >= 0) {
      if ( (!reverse && features.exists((f) => f.end == index && f!= features.last) )
        || (reverse && features.exists((f) => f.start == tseq.length-index+1 && f!= features.head) ) ) {
        val start = index - flank - 1
        val end = index + flank - 1
        val s = safeSub(index, start, end, tseq)
        fd.append(s)
      }
      index = tseq.indexOf(q, index + 1)
    }
    fd.toList
  }

  def falseDornors(flank: Int): List[String] = {
    val fd = mutable.ListBuffer[String]()
    val q = "GT"
    var index = seq.indexOf(q)
    while (index >= 0) {
      if ( !features.exists((f) => f.end == index) ) {
        val start = index - flank - 1
        val end = index + flank - 1

        fd.append(safeSub(index, start, end, seq))
      }

      index = seq.indexOf(q, index + 1)
    }
    fd.toList
  }

  def acceptors(flank: Int): List[String] = {
    val fd = mutable.ListBuffer[String]()
    val q = "AG"
    var index = tseq.indexOf(q)
    while (index >= 0) {
      if ( (!reverse && features.exists((f) => f.start-3 == index && f!= features.head))
        || (reverse && features.exists((f) => f.end+3 == tseq.length-index+1 && f!= features.last)) ) {
        val start = index - flank + 2
        val end = index + flank + 2
        fd.append(safeSub(index, start, end, tseq))
      }

      index = tseq.indexOf(q, index + 1)
    }
    fd.toList
  }

  def falseAcceptors(flank: Int): List[String] = {
    val fd = mutable.ListBuffer[String]()
    val q = "AG"
    var index = seq.indexOf(q)
    while (index >= 0) {
      if ( !features.exists((f) => f.start-3 == index) ) {
        val start = index - flank + 2
        val end = index + flank + 2

        fd.append(safeSub(index, start, end, seq))
      }

      index = seq.indexOf(q, index + 1)
    }
    fd.toList
  }

  def startSite(flank: Int): String = {
    var index = 0
    var start = 0
    var end = 0
    if (reverse) {
      index = tseq.length - features.last.end
      start = index - flank
      end = index + flank
    } else {
      index = features.head.start - 2
      start = index - flank + 1
      end = index + flank + 1
    }

    safeSub(index, start, end, tseq)
  }

  def falseStarters(flank: Int): List[String] = {
    val fd = mutable.ListBuffer[String]()
    val q = "ATG"
    var index = seq.indexOf(q)
    while (index >= 0) {
      if ( !features.exists((f) => f.start-1 == index )) {
        val start = index - flank
        val end = index + flank + 2

        fd.append(safeSub(index, start, end, seq))
      }

      index = seq.indexOf(q, index + 1)
    }
    fd.toList
  }

  def safeSub2(x: String, subAt: Int, leftPad: Boolean, idx: Int=0): String = {
    var prefix = ""
    var content = ""
    var suffix = ""
    var j = subAt

    if (leftPad) {
      if (subAt < 0) {
        prefix = "N"*(-subAt)
        j = 0
      }
      content = x.substring(j)
    } else {
      if (subAt > x.length) {
        j = x.length
        val remain = subAt-j
        suffix = "N"*(remain)
      }
      content = x.substring(0, j)
    }

    prefix + content + suffix
  }

  def safeSub(index:Int, start: Int, end: Int, seq: String): String = {
    var prefix = ""
    var suffix = ""
    var sstart = start
    var send = end

    if (sstart < 0) {
      prefix = "N"*(-sstart)
      sstart = 0
    }
    if (send > seq.length) {
      suffix = "N"*(send - seq.length)
      send = seq.length
    }

    prefix + seq.substring(sstart, send) + suffix
  }

}
