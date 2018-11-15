// Alex Rose
//-------------------------------------------------------------------------

// Testing new EL3 programs
//
import org.scalatest.FunSuite
import Interp3._

class TestNew extends FunSuite {

  // Put your program code inside the triple quotes

  // multi-arg functions
  val f1Code = """(let f1 (fun x (fun y (- x y))) (@ (@ f1 4) 2))"""
  val f2Code = """(let f2 (fun x (fun y (fun z (+ x (- y z))))) (@ (@ (@ f2 2) 3) 4))"""


  // fibonacci (regular)
  val fibCode = """(letRec fib (fun n (if (<= n 2) 1 (+ (@ fib (- n 1)) (@ fib (- n 2))))) (@ fib 10))"""

  // fibonacci (tail-recursive)
  val fibtCode = """(let fibt (fun n (letRec helper (fun m (fun a (fun b (if (<= m 2) a (@ (@ (@ helper (- m 1)) (+ a b)) a))))) (@ (@ (@ helper n) 1) 1))) (@ fibt 10))"""

  // fibonacci (cps)
  val fibcCode = """()"""

  // Test the programs with heap storage

  test("multi-arg functions") {
    assertResult(2) { process(f1Code,true) }
    assertResult(1) { process(f2Code,true) }
  }

  test("fibonacci (regular)") {
    assertResult(55) { process(fibCode,true) }
  }

  test("fibonacci (tail-recursive)") {
    assertResult(55) { process(fibtCode,true) }
  }

  test("fibonacci (cps)") {
    assertResult(55) { process(fibcCode,true) }
  }
}
