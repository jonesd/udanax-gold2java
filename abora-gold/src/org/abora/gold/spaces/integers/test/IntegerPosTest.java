package org.abora.gold.spaces.integers.test;

import java.io.PrintWriter;
import java.io.StringWriter;

import junit.framework.TestCase;

import org.abora.gold.spaces.integers.IntegerPos;
import org.abora.gold.xpp.basic.Heaper;



public class IntegerPosTest extends TestCase {

	public void testMakeInt() {
		IntegerPos pos = IntegerPos.make(100);
		
		assertEquals(100, pos.asIntegerVar());
	}
	
	public void testZero() {
		assertEquals(0, IntegerPos.zero().asIntegerVar());
	}
	
	public void testIsEqual() {
		IntegerPos pos100 = IntegerPos.make(100);
		
		assertTrue("pos100==pos100", pos100.isEqual(pos100));
		assertTrue("pos100=pos100", pos100.isEqual(IntegerPos.make(100)));
		assertFalse("pos100~=pos99", pos100.isEqual(IntegerPos.make(99)));
		assertFalse("pos100~=null", pos100.isEqual(null));
		
		//TODO isEqual for non-IntegerPos
	}
	
	public void testPrintOn() {
		testPrintOn("I(0)", IntegerPos.make(0));
		testPrintOn("I(987)", IntegerPos.make(987));
		testPrintOn("I(-987)", IntegerPos.make(-987));
	}
	
	protected void testPrintOn(String expected, Heaper heaper) {
		StringWriter stringWriter = new StringWriter();
		PrintWriter printWriter = new PrintWriter(stringWriter);
		heaper.printOn(printWriter);
		assertEquals(expected, stringWriter.toString());
	}
}
