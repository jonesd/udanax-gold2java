package org.abora.gold.java;

import junit.framework.TestCase;


public class AboraSupportTest extends TestCase {

	public void testModulo() {
		// \\ aNumber 
		//	"modulo. Remainder defined in terms of //. Answer a Number with the 
		//	same sign as aNumber. e.g. 9\\4 = 1, -9\\4 = 3, 9\\-4 = -3, 0.9\\0.4 = 0.1."
		//
		//	^self - (self // aNumber * aNumber)
		// From Squeak Smalltalk 3.6
		
		assertEquals(1, AboraSupport.modulo(9, 4));
		assertEquals(3, AboraSupport.modulo(-9, 4));
		assertEquals(-3, AboraSupport.modulo(9, -4));
		assertEquals(-1, AboraSupport.modulo(-9, -4));
	}
	
	public void testQuotient() {
		assertEquals(2, AboraSupport.quotient(9, 4));
		assertEquals(-3, AboraSupport.quotient(-9, 4));
		assertEquals(-3, AboraSupport.quotient(9, -4));
		assertEquals(2, AboraSupport.quotient(-9, -4));
	}
}
