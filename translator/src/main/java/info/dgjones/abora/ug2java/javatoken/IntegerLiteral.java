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




public class IntegerLiteral extends JavaLiteral {

	private final long longValue;
	private final int radix;
	
	public IntegerLiteral(long longValue) {
		this(longValue, 10);
	}

	public IntegerLiteral(long longValue, int radix) {
		super(generateString(longValue, radix));
		this.longValue = longValue;
		this.radix = radix;
	}

	public int getIntValue() {
		return (int)longValue;
	}

	public long getLongValue() {
		return longValue;
	}
	
	public int getRadix() {
		return radix;
	}
	
	
	private static String generateString(long longValue, int radix) {
		String generate = "";
		boolean outsideIntegerRange = longValue > Integer.MAX_VALUE || longValue < Integer.MIN_VALUE;
		
		if (radix == 16) {
			generate += "0x";
			generate += outsideIntegerRange ? Long.toHexString(longValue) : Integer.toHexString((int)longValue); 
		} else if (radix == 8) {
			generate += "0";
			generate += outsideIntegerRange ? Long.toOctalString(longValue) : Integer.toOctalString((int)longValue);
		} else {
			radix = 10;
			generate += Long.toString(longValue);
		}
		
		if (outsideIntegerRange) {
			generate +="L";
		}
		return generate;
	}

}
