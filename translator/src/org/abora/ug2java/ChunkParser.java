/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ug2java;

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
