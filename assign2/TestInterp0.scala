//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// Testing EL0 interpreter
//
import org.scalatest.FunSuite
import Interp0._

class TestInterp0 extends FunSuite {
  
  test("correctly interpret an expression involving add, mul, div") {
    assertResult(6)(process("(+ (* (/ 3 4) 5) 6)"))
  }
	
  test("correctly interpret an expression involving sub") {
    assertResult(1)(process("(- (- (- 10 3) 3) 3)"))
  }
	
  // test non-zero inputs for mod, 0 in each arg position, negative
  // args in each arg pos
  test("correctly interpret an expression involving rem"){
    assertResult(0)(process("(% 10 2)"))
  } 
  
  test("correct behavior for div by 0") {
    intercept[InterpException]{process("(/ 10 0)")}
  }
  
  test("correct behavior for rem by 0"){
    intercept[InterpException]{process("(% 10 0)")}
  } 
  
  test("correct behavior for rem positive by negative"){
    assertResult(1)(process("(% 10 -3)"))
  } 
  
  test("correct behavior for rem negative by positive") {
    assertResult(-1)(process("(% -10 3)"))
  }
  
  test("correct behavior for rem negative by negative") {
    assertResult(-1)(process("(% -10 -3)"))
  }
	
  test ("correctly interpret a complicated arithmetic expression") {
    assertResult(5)(process("(* 1 (+ 2 (- 3 (/ 4 (% 6 7)))))"))
  }

  test("Another complicated arithmetic expression") {
    assertResult(-47)(process("(+ 43 (- -92 (% -56 -3)))"))
  }
	
}
