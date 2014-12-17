package wandou.indicator

import wandou.math.timeseries.TBaseSer
//import wandou.securities.model.Sec
//import wandou.collection.ArrayList
//import wandou.securities.dataserver.RichInfo
//import wandou.securities.InfoPointSer
//import java.util.logging.Logger

class InfoPointIndicator(_baseSer: TBaseSer) extends Indicator(_baseSer) {

  //
  //  sname = "INFO"
  //  lname = "INFO"
  //  isOverlapping = true
  //
  //  private var infoSer: InfoPointSer = _
  //
  //  val exists = TVar[Double]("I", Plot.Info)
  //  val infos = TVar[ArrayList[RichInfo]]("I", Plot.None)
  //
  //  override def set(baseSer: TBaseSer) {
  //    // set baseSer to mfSer. @Note, this.freq is not set yet before super.set(mfSer)
  //    val sec = baseSer.serProvider.asInstanceOf[Sec]
  //    val freq = baseSer.freq
  //    val infoSer = sec.infoPointSerOf(freq).get
  //    if (!infoSer.isLoaded) {
  //      sec.loadInfoPointSer(infoSer)
  //    }
  //
  //    this.infoSer = infoSer
  //
  //    super.set(infoSer)
  //  }
  //
  protected def compute(fromIdx: Int, size: Int) {
    //    var i = fromIdx
    //    while (i < size) {
    //      val info = infos(i)
    //      if (info != null && !info.isEmpty) {
    //        exists(i) = 0
    //      }
    //
    //      i += 1
    //    }
  }
}
