package cbb.gmtrainer.test

class GeneTest extends TestBase {

  it should "GenesLoader.load" in {
    genes.size should be (765)
  }

  it should "Dornors" in {
    val all = genes.values.flatMap( _.donors(30))
    all should contain ("CGAGACGCTTCTACTCCTCATTGCTTTGAAAGTAAGACATGAAGTCAGCGCTTCTATTGC")
    all.size should be (4266)

    val f_sense = genes.get(SENSE_ACCESSION).get.donors(30)
    f_sense.size should be (4)
    f_sense should contain ("GAGTGGTGGAGTGTTCAACCAGGCTGCTCAGGTGGGGGACATTTGTGTCGTGTGATTTTC")

    val f_antisense = genes.get(ANTISENSE_ACCESSION).get.donors(30)
    f_antisense.size should be (23)
    f_antisense should contain ("GAGTTTGCATGCAAGGCAGTATCTTCTATAGGTAGTGTCCATCAAACGATAATTTCAATC")

    genes("11305.m000835").donors(30) should contain ("CCTACTACTGTACCTCGTGGTGCTCCTGCTGGTCAGANNNNNNNNNNNNNNNNNNNNNNN")
  }

  it should "falseDornors" in {
    genes.values.flatMap( _.falseDornors(30)).size should be (979004)

    val f_sense = genes.get(SENSE_ACCESSION).get.falseDornors(30)
    f_sense should contain ("TGAACTGAATTTGCTCGAATGTCCCATTTCAGTGTGTGTTGCCTAGCAGCATAGCTGTTC")
    f_sense.size should be (1220)

    val f_antisense = genes.get(ANTISENSE_ACCESSION).get.falseDornors(30)
    f_antisense should contain ("GAAAAAAGTCCGATATTCGCGGCGATATGCAGTAGTCGCGAATTGTGTTTTTCACCAGTG")
    f_antisense.size should be (1652)
  }

  it should "Acceptors" in {
    genes.values.flatMap( _.acceptors(30)).size should be (4277)

    val f_sense = genes(SENSE_ACCESSION).acceptors(30)
    f_sense.size should be (4)
    f_sense should contain ("ATGGTCGATTGGTGATGGTTTTACATTCAGATACGTCGAGCACTTCTGGCAGATCATCAA")

    val f_antisense = genes(ANTISENSE_ACCESSION).acceptors(30)
    f_antisense.size should be (23)
    f_antisense should contain ("TGCACTTTTCTGTTTTTTACTTACATTCAGGCCGAATAGCACTGAAGTTGGAGGCAGCGG")

    val f_singles = genes(SINGLE_ACCESSION).acceptors(30)
    f_singles.size should be (0)

    genes("11965.m000235").acceptors(30) should contain ("NNNNNNNNNNNNNNNNNNNNNNNNCCACAGGTCGTTCGTAAGAATCTAAAGGTCCGCCTT")
    genes("16248.m000546").acceptors(30) should contain ("NNNNNNNNNNNNNGTGTTTTTCGTCCAGAGGTGTCCTATCAGAAGTCGGTGAAACGTAAT")
  }

  it should "falseAcceptors" in {
    genes.values.flatMap( _.falseAcceptors(30)).size should be (1087071)

    val f_sense = genes.get(SENSE_ACCESSION).get.falseAcceptors(30)
    f_sense should contain ("AACTGAATTTGCTCGAATGTCCCATTTCAGTGTGTGTTGCCTAGCAGCATAGCTGTTCTT")
    f_sense.size should be (1330)

    val f_antisense = genes.get(ANTISENSE_ACCESSION).get.falseAcceptors(30)
    f_antisense should contain ("CTAAGTCGTCGAACATAAGAGCAAAATCAGCGAAGCCAGGGCAGGGATGAGCTCGGTGAA")
    f_antisense.size should be (1929)
    genes.get("519.m001226").get.falseAcceptors(30) should contain ("NNNNNNNNNNNNNNNNNAGAGAGATTCGAGGGCATCGCCGAGGGCGAACGGTATATAGTG")
  }

  it should "Starters" in {
    genes.values.flatMap( _.falseStarters(30)).size should be (304456)

    val f_sense = genes.get(SENSE_ACCESSION).get.startSite(30)
    f_sense should be ("TCATTGTTGTTGAGTAAGCCGTTTAAAACTATGTCTGCTGGACCTCATGTTCTCCCCCCT")

    val f_antisense = genes.get(ANTISENSE_ACCESSION).get.startSite(30)
    f_antisense should be ("GAACAGGTTGTTACTGTTGTGGTTTTGATAATGGGGGACGCGCGATACTTCAGTTCGTCG")

    val f_singles = genes.get(SINGLE_ACCESSION).get.startSite(30)
    f_singles should be ("TCAGTTTTTTCTCTGTGTAATTGACCTCCAATGACTGACTACAAGGTTGCTGATATTGGG")

    genes("10884.m000192").startSite(30) should be ("NNNNNNNNNNNNNNNNTCAGTTGTGGTGGAATGGGAGATGAGGAGGCGGTAATTTCGTGG")
  }

  it should "falseStarters" in {
    val f_sense = genes.get(SENSE_ACCESSION).get.falseStarters(30)
    f_sense should contain ("CATGTTGCATATTGAAATGTAGTTGAATAAATGGTTTGTCAGGGACGTTCGACATGTTTCAA")
    f_sense.size should be (351)

    val f_antisense = genes.get(ANTISENSE_ACCESSION).get.falseStarters(30)
    f_antisense should contain ("CAGTACCCATGCACACAGCACAGTTTACATATGTTCTATAGGACAGTGACCTCATACATCAG")
    f_antisense.size should be (541)
  }


}
