package cbb.gmtrainer.test

import cbb.gmtrainner.BoundaryDetector

class PWMCaculatorTest extends TestBase {

  val r = pwmCalc.freq(_.acceptors(30))
  val b = pwmCalc.freq(_.falseAcceptors(30))
  val x = pwmCalc.information(r, b)

  it should "freq" in {
    f"${r(1)(3)}%.4f" should be ("0.3241")
    f"${b(7)(2)}%.4f" should be ("0.2409")

    f"${r(6)(2)}%.4f" should be ("0.2404")
  }

  it should "information" in {
    x.size should be (60)
    f"${x(4)}%.3f" should be ("0.021")
    f"${x(26)}%.3f" should be ("0.456")
  }

  it should "pwm" in {
    val x = pwmCalc.pwm(pwmCalc.freq(_.donors(30)), pwmCalc.freq(_.falseDornors(30)), (29, 36))
    x.size should be (60)
    f"${x(1)(2)}%.3f" should be ("0.135")
  }

}
