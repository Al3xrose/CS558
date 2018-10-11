//Alex Rose
import org.scalatest.FunSuite
import Machine0._

class TestMachine0 extends FunSuite {

  val divrem = Divrem
  val plus = Plus
  val pop = Pop
  val times = Times
  val swap = Swap

  val p1 : Program = List(Const(1), Const(3), Const(-2), plus, plus)
  
  val p2 : Program = List(Const(2), Const(-3), times, Const(5), Const(3), divrem, pop, Const(-1), times, plus)

  val p3 : Program = List(Const(-2), Const(3), divrem, pop, Const(3), times, Const(-2), Const(3), divrem, swap, pop, plus)


  test("first program result should be 2") {
    assert(exec(p1) == 2)
  }

  test("second program result should be -7") {
    assert(exec(p2) == -7)
  }

  test("third program result should be -2") {
    assert(exec(p3) == -2)
  }
    
}
