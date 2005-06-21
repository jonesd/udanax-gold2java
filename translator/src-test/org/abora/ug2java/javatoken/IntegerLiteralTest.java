package org.abora.ug2java.javatoken;

import junit.framework.TestCase;



public class IntegerLiteralTest extends TestCase {

	public void testValue() {
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
