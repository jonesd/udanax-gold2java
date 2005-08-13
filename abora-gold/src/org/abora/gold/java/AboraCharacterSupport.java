package org.abora.gold.java;



public class AboraCharacterSupport {

	public AboraCharacterSupport() {
		super();
	}

	public static char cr() {
		return '\r';
	}

	public static char tab() {
		return '\t';
	}

	public static char backspace() {
		return '\b';
	}
	
	public static char nullx() {
		return '\u0000';
	}

	public static String println() {
		throw new UnsupportedOperationException();
	}

	public static boolean isSeparator(char c) {
		//TODO this is a little different from the Squeaks Character.isSeparator - significant?
		return Character.isWhitespace(c);
	}

}
