package wandou.util

/**
 *
 * @author  Caoyuan Deng
 * @version 1.0, November 24, 2006, 5:06 PM
 * @since   1.0.4
 */
trait ChangeObserver {
  type Updater = PartialFunction[ChangeSubject, Unit]

  val updater: Updater

  def observe(subject: ChangeSubject) {
    subject.addObserver(this, this)
  }

  def unObserve(subject: ChangeSubject) {
    subject.removeObserver(this)
  }
}
