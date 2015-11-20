package cbb.gmtrainer.test

class MarkovMatTest extends TestBase {

  it should "calc" in {
    markovCalc.caculate()

    markovCalc.unframeMatrices.initialMatrix("ACCTT")._2 should be (282)
    f"${markovCalc.unframeMatrices.initialMatrix("ACCTT")._1}%.8f" should be ("0.00085298")

    markovCalc.unframeMatrices.transitionMatrix("CTAGCG")._2 should be (30)
    f"${markovCalc.unframeMatrices.transitionMatrix("CTAGCG")._1}%.5f" should be ("0.17964")

    markovCalc.frameMatrices.initialMatrix.get("CAATC", 1)._2 should be (136)
    f"${markovCalc.frameMatrices.initialMatrix.get("CAATC", 1)._1}%.8f" should be ("0.00051174")

    markovCalc.frameMatrices.transitionMatrix.get("CCCACC", 2)._2 should be (32.25)
    f"${markovCalc.frameMatrices.transitionMatrix.get("CCCACC", 2)._1}%.5f" should be ("0.33594")

  }

}
