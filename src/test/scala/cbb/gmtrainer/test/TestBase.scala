package cbb.gmtrainer.test

import cbb.gmtrainner.{MarkovMatCaculator, PWMCaculator, GenesLoader}
import org.scalatest.{Matchers, FlatSpec}

class TestBase extends FlatSpec with Matchers {
  val SENSE_ACCESSION = "10054.m000245"
  val ANTISENSE_ACCESSION = "10054.m000225"
  val SINGLE_ACCESSION = "10054.m000231"

  val genes = GenesLoader.load("testdata/pmarinus4training.fa", "testdata/pmarinus4training.gff")
  val pwmCalc = PWMCaculator(genes.values.toList)
  val markovCalc = MarkovMatCaculator(genes.values.toList, 5)

}
