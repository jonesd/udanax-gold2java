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

public class ChunkParser extends Object {
	final private String chunk;
	private int index = 0;

	public ChunkParser(String chunk) {
		super();

		this.chunk = chunk;
	}
	public boolean atEnd() {
		return index >= chunk.length();
	}
	public void moveToWord(String word) {
		while (!atEnd() && !nextWord().equals(word));
	}
	public char next() {
		char c = peek();
		index++;
		return c;
	}
	public String nextSmalltalkWord() {
		skipSmalltalkComment();
		return nextWord();
	}
	public String nextWord() {
		skipWhitespace();
		StringBuffer buffer = new StringBuffer();
		while (!atEnd()) {
			char c = peek();
			if (!Character.isJavaLetterOrDigit(c) && c != ':' && c != '-' && c != '.') {
				if (buffer.length() == 0) {
					buffer.append(next());
				}
				break;
			}
			buffer.append(next());
		}
		return buffer.toString();
	}
	public char peek() {
		return chunk.charAt(index);
	}
	public String peekWord() {
		int startingIndex = index;
		String word = nextWord();
		index = startingIndex;
		return word;
	}
	public void skipSmalltalkComment() {
		int startingIndex;
		do {
			startingIndex = index;
			skipWhitespace();
			if (!atEnd()) {
				char c = peek();
				if (c == '"') {
					next();
					while ((c = next()) != '"');
				}
			}
		}
		while (startingIndex != index);
	}
	public void skipWhitespace() {
		while (!atEnd()) {
			char c = peek();
			if (!Character.isWhitespace(c)) {
				return;
			} else {
				next();
			}
		}
	}
}
