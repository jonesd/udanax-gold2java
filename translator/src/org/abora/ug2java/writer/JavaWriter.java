package org.abora.ug2java.writer;

import java.io.PrintWriter;

import org.abora.ug2java.JavaClass;



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
