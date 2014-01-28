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
package info.dgjones.abora.ug2java.stscanner;

public class ScannerToken {
	public int tokenType = -1;
	public String tokenString = null;
	public long tokenInt;
	public int tokenIntRadix;
	public double tokenDouble;

	public static final int TOKEN_STRING = 1;
	public static final int TOKEN_INTEGER = 2;
	public static final int TOKEN_DOUBLE = 3;
	public static final int TOKEN_COMMENT = 4;
	public static final int TOKEN_WORD = 5;
	public static final int TOKEN_KEYWORD = 6;
	public static final int TOKEN_BLOCK_START = 7;
	public static final int TOKEN_BLOCK_END = 8;
	public static final int TOKEN_TYPE_START = 9;
	public static final int TOKEN_TYPE_END = 10;
	public static final int TOKEN_END = 11;
	public static final int TOKEN_BRACKET_START = 12;
	public static final int TOKEN_BRACKET_END = 13;
	public static final int TOKEN_SYMBOL = 14;
	public static final int TOKEN_STATEMENT_END = 15;
	public static final int TOKEN_CHUNK = 17;
	public static final int TOKEN_CHARACTER = 18;
	public static final int TOKEN_RETURN = 19;
	public static final int TOKEN_TEMPS = 20;
	public static final int TOKEN_ASSIGNMENT = 21;
	public static final int TOKEN_BINARY = 22;
	public static final int TOKEN_BLOCK_TEMP = 23;
	public static final int TOKEN_CASCADE = 24;

	/**
	 * ScannerToken constructor comment.
	 */
	public ScannerToken() {
		super();
	}

	/**
	 * ScannerToken constructor comment.
	 */
	public ScannerToken(int tokenType) {
		super();

		this.tokenType = tokenType;
	}

	/**
	 * ScannerToken constructor comment.
	 */
	public ScannerToken(int tokenType, double value) {
		this(tokenType);

		this.tokenDouble = value;
	}

	/**
	 * ScannerToken constructor comment.
	 */
	public ScannerToken(int tokenType, long value, int radix) {
		this(tokenType);

		this.tokenInt = value;
		this.tokenIntRadix = radix;
	}

	/**
	 * ScannerToken constructor comment.
	 */
	public ScannerToken(int tokenType, String value) {
		this(tokenType);

		this.tokenString = value;
	}

	public void checkType(int requiredType) {
		if (tokenType != requiredType) {
			throw new IllegalStateException("Expected token of type:" + requiredType + " but was:" + tokenType);
		}
	}

	public void checkType(int requiredType1, int requiredType2) {
		if (tokenType != requiredType1 && tokenType != requiredType2) {
			throw new IllegalStateException("Expected token of type:" + requiredType1 + " or " + requiredType2 + " but was:" + tokenType);
		}
	}
}