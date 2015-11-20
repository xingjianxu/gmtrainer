package cbb.gmtrainner

import breeze.stats.DescriptiveStats

object BoundaryDetector {

  def detect(list: List[Double]): (Int, Int) = {
    val outlier = DescriptiveStats.percentile(list, 0.8)
    val a = list.indexWhere(_ > outlier)
    val b = list.lastIndexWhere(_ > outlier)
    (a, b)
  }


}
