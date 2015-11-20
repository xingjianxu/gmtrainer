package cbb.gmtrainner

import java.io.File

import cbb.gmtrainner.Strand._
import org.biojava.nbio.core.sequence.io.FastaReaderHelper

import scala.collection.mutable
import scala.io.Source

object GenesLoader {

  def load(faFile: String, gffFile: String): mutable.Map[String, Gene] = {
    val seqsMap = FastaReaderHelper.readFastaDNASequence(new File(faFile))

    val genes = mutable.Map[String, Gene]()

    val agrGff = Source.fromFile(gffFile).getLines().map((line) => {
      val annots = line.split("\\s")
      val seqId = annots(0)
      val ftype = annots(2)
      val start = annots(3).toInt
      val end = annots(4).toInt
      val strand = if (annots(6) == "+") SENSE else ANTISENSE

      (seqId, Feature(ftype, start, end, strand))
    }).toList.groupBy(_._1)

    agrGff.foreach {case (seqId, x) =>
      genes.put(seqId, Gene(seqId, seqsMap.get(seqId).toString, x.map(_._2)))
    }

    genes
  }
}
