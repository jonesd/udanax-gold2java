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
package info.dgjones.abora.ug2java.javatoken;

import junit.framework.TestCase;



public class IntegerLiteralTest extends TestCase {

	public void testValue() {
		// Numbers taken from "The Java Language Specification Second Edition", James Gosling, etc 3.10.1 pg.21
		assertEquals("0", new IntegerLiteral(0).value);
		assertEquals("00", new IntegerLiteral(0, 8).value);
		assertEquals("0x0", new IntegerLiteral(0, 16).value);

		assertEquals("1", new IntegerLiteral(1).value);
		assertEquals("01", new IntegerLiteral(1, 8).value);
		assertEquals("0x1", new IntegerLiteral(1, 16).value);

		assertEquals("-1", new IntegerLiteral(-1).value);
		assertEquals("037777777777", new IntegerLiteral(-1, 8).value);
		assertEquals("0xffffffff", new IntegerLiteral(-1, 16).value);

		assertEquals("2147483647", new IntegerLiteral(Integer.MAX_VALUE).value);
		assertEquals("017777777777", new IntegerLiteral(Integer.MAX_VALUE, 8).value);
		assertEquals("0x7fffffff", new IntegerLiteral(Integer.MAX_VALUE, 16).value);

		assertEquals("-2147483648", new IntegerLiteral(Integer.MIN_VALUE).value);
		assertEquals("020000000000", new IntegerLiteral(Integer.MIN_VALUE, 8).value);
		assertEquals("0x80000000", new IntegerLiteral(Integer.MIN_VALUE, 16).value);

		assertEquals("9223372036854775807L", new IntegerLiteral(Long.MAX_VALUE).value);
		assertEquals("0777777777777777777777L", new IntegerLiteral(Long.MAX_VALUE, 8).value);
		assertEquals("0x7fffffffffffffffL", new IntegerLiteral(Long.MAX_VALUE, 16).value);

		assertEquals("-9223372036854775808L", new IntegerLiteral(Long.MIN_VALUE).value);
		assertEquals("01000000000000000000000L", new IntegerLiteral(Long.MIN_VALUE, 8).value);
		assertEquals("0x8000000000000000L", new IntegerLiteral(Long.MIN_VALUE, 16).value);
		
		// Dont generate -1L
}
}
