package cbb.gmtrainner

case class MarkovMatCaculator(genes: List[Gene], korder: Int) {

  val frameMatrices = FrameMatrices(korder)
  val unframeMatrices = UnframeMatrices(korder)

  def caculate() = {
    unframeMatrices.process( genes.flatMap(_.introns) )
    frameMatrices.process( genes.map((x) => {
      val s = x.cds_seq
      s.substring(0, s.length-3)
    }) )
  }

}
