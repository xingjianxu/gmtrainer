package cbb.gmtrainner

import collection.JavaConversions._

import com.google.common.collect.HashBasedTable

case class FrameMatrices(korder: Int) extends Matrices {
  val PCOUNT_T = 0.25d
  val PCOUNT_I = 4*PCOUNT_T

  val transitionMatrix = HashBasedTable.create[String, Int, (Double, Double)]()
  val initialMatrix = HashBasedTable.create[String, Int, (Double, Double)]()

  override def process(seqs: List[String]): Unit = {
    var l = 0d
    seqs.foreach((x) => {
      if (FILTER.matcher(x).matches()) {
        l += x.length - korder
        x.sliding(korder+1).zipWithIndex.foreach((e) => {
          feed(e._1, e._2)
        })
      }

    })

    l = l + 3*math.pow(4, korder)
    l /= 3

    for (cell <- initialMatrix.cellSet()) {
      val n = cell.getValue
      initialMatrix.put(cell.getRowKey, cell.getColumnKey, (n._2/l, n._2))
    }

    for (cell <- transitionMatrix.cellSet()) {
      val n = cell.getValue
      transitionMatrix.put(cell.getRowKey, cell.getColumnKey, (n._2 / initialMatrix.get(cell.getRowKey.init, cell.getColumnKey)._2, n._2))
    }

  }

  def feed(kplus1mer: String, i: Int) = {
    val im = i % 3
    val acc1 = if (transitionMatrix.contains(kplus1mer, im)) { transitionMatrix.get(kplus1mer, im)._2 } else { PCOUNT_T }
    transitionMatrix.put(kplus1mer, im, (0, acc1+1))

    val kinit = kplus1mer.init
    val acc2 = if (initialMatrix.contains(kinit, im)) { initialMatrix.get(kinit, im)._2 } else { PCOUNT_I }
    initialMatrix.put(kinit, im, (0, acc2+1))
  }

}
