package cbb.gmtrainner

case class PWMCaculator(genes: List[Gene]) {

  def freq(f: (Gene)=>List[String]): Array[Array[Double]] = {
    val cMat = Array.fill[Array[Int]](60)(Array.fill[Int](4)(0))
    val fMat = Array.fill[Array[Double]](60)(Array.fill[Double](4)(0d))

    val afd = genes.flatMap((g) => f(g))
    afd.foreach((x) => {
      cMat.zip(x).foreach( (e) => {
        e._2 match {
          case 'A' => e._1(0) += 1
          case 'C' => e._1(1) += 1
          case 'G' => e._1(2) += 1
          case 'T' => e._1(3) += 1
          case _ => false
        }
      })
    })

    cMat.zip(fMat).foreach( (e) => {
      val sum = e._1.sum.toDouble
      e._2(0) = e._1(0) / sum
      e._2(1) = e._1(1) / sum
      e._2(2) = e._1(2) / sum
      e._2(3) = e._1(3) / sum
    })
    fMat
  }

  def information(fMat: Array[Array[Double]], fMatBackground: Array[Array[Double]]): List[Double] = {
    fMat.zip(fMatBackground).map((e) => {
      e._1.zip(e._2).map((x) => ic(x._1, x._2)).sum
    }).toList
  }

  def ic(a: Double, b: Double): Double = {
    if (a!=0 && b != 0) {
      a * Math.log(a / b) / Math.log(2)
    } else {
      0.0
    }
  }

  def informativeBoundary(a: List[Double]): (Int, Int) = {
    (0, 0)
  }

  def pwm(r: Array[Array[Double]], b: Array[Array[Double]], bound: (Int, Int)): List[Array[Double]] = {
    r.zip(b).toList.map((e) => {
      e._1.zip(e._2).map((x) => caculate(x._1, x._2) )
    })
  }

  def caculate(re: Double, be: Double): Double = {
    if (be == 1) {
      0
    } else if (re!=0 && be!=0) {
      Math.log(re/be)
    } else{
      -9999
    }
  }

}
