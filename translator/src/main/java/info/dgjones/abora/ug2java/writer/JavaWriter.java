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
package info.dgjones.abora.ug2java.writer;

import java.io.PrintWriter;



public class JavaWriter {
	private final PrintWriter printWriter;
	private final Indentation indentation;
	
	private boolean atStartOfLine = true;
	private char lastCharacter = ' ';
	
	public JavaWriter(PrintWriter printWriter, Indentation indentation) {
		super();
		this.printWriter = printWriter;
		this.indentation = indentation;
	}
	
	public void flush() {
		printWriter.flush();
	}
	
	public void append(String text) {
		if (atStartOfLine) {
			indentation.write(printWriter);
			atStartOfLine = false;
			lastCharacter = ' ';
		}
		printWriter.print(text);
		if (text.length() > 0) {
			lastCharacter = text.charAt(text.length() - 1);
		}
	}
	
	public void newLine() {
		printWriter.print(JavaWriter.lineSeparator());
		atStartOfLine = true;
		lastCharacter = ' ';
	}
	
	public char getLastCharacter() {
		return lastCharacter;
	}
	
	public void writeLeadingSpaceIfRequired() {
		if (!Character.isWhitespace(lastCharacter)) {
			append(" ");
		}
	}
	
	public void increase() {
		indentation.increase();
	}
	
	public void decrease() {
		indentation.decrease();
	}
	
	public void ensureAtStartOfLine() {
		if (!atStartOfLine) {
			newLine();
		}
	}

	public static String lineSeparator() {
		return System.getProperty("line.separator");
	}

}
