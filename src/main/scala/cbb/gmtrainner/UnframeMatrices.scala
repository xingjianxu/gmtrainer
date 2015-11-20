package cbb.gmtrainner

import scala.collection.mutable

case class UnframeMatrices(korder: Int) extends Matrices {

  val PCOUNT_T = 5
  val PCOUNT_I = 4*PCOUNT_T

  val transitionMatrix = mutable.Map[String, (Double, Double)]()
  val initialMatrix = mutable.Map[String, (Double, Double)]()

  override def process(seqs: List[String]): Unit = {
    var l = 0d
    seqs.foreach((x) => {
      if (FILTER.matcher(x).matches()) {
        l += x.length - korder
        x.sliding(korder+1).foreach((e) => {
          feed(e)
        })
      }
    })
    l = l + Math.pow(4, korder)

    for (key <- initialMatrix.keys) {
      val n = initialMatrix(key)
      initialMatrix.put(key, (n._2/l, n._2))
    }

    for (key <- transitionMatrix.keys) {
      val n = transitionMatrix(key)
      transitionMatrix.put(key, (n._2/initialMatrix(key.init)._2, n._2))
    }
  }

  def feed(kplus1mer: String) = {
    val v1 = transitionMatrix.getOrElseUpdate(kplus1mer, (0, PCOUNT_T))
    transitionMatrix.put(kplus1mer, (0, v1._2+1))

    val v2 = initialMatrix.getOrElseUpdate(kplus1mer.init, (0, PCOUNT_I))
    initialMatrix.put(kplus1mer.init, (0, v2._2+1))
  }

}
