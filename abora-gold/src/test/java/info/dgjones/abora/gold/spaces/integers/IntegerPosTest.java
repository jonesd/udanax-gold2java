/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.gold.spaces.integers;

import java.io.PrintWriter;
import java.io.StringWriter;

import junit.framework.TestCase;

import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xpp.basic.Heaper;



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
