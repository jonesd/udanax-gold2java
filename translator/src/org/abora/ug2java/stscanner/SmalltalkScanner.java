/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.stscanner;

public class SmalltalkScanner {
	private String contents;
	private int index = 0;

	public ScannerToken token;

	public SmalltalkScanner(String contents) {
		super();

		this.contents = contents;

		advance();
	}

	public ScannerToken advance() {
		token = null;

		char c = '\000';
		while (!atEnd() && (Character.isWhitespace(c = peek()))) {
			next();
		}
		if (atEnd()) {
			token = new ScannerToken(ScannerToken.TOKEN_END);
		} else if (c == '"') {
			token = readComment();
		} else if (Character.isDigit(c) || (c == '-' && Character.isDigit(peek2()))) {
			token = readNumber();
		} else if (c == '[') {
			next();
			token = new ScannerToken(ScannerToken.TOKEN_BLOCK_START);
		} else if (c == ']') {
			next();
			token = new ScannerToken(ScannerToken.TOKEN_BLOCK_END);
		} else if (c == '{') {
			next();
			token = new ScannerToken(ScannerToken.TOKEN_TYPE_START);
		} else if (c == '}') {
			next();
			token = new ScannerToken(ScannerToken.TOKEN_TYPE_END);
		} else if (c == '(') {
			next();
			token = new ScannerToken(ScannerToken.TOKEN_BRACKET_START);
		} else if (c == ')') {
			next();
			token = new ScannerToken(ScannerToken.TOKEN_BRACKET_END);
		} else if (c == '#') {
			token = readSymbol();
		} else if (c == '\'') {
			token = readString();
		} else if (c == '$') {
			token = readCharacter();
		} else if (c == '!') {
			next();
			token = new ScannerToken(ScannerToken.TOKEN_CHUNK);
		} else if (c == '_') {
			next();
			token = new ScannerToken(ScannerToken.TOKEN_ASSIGNMENT);
		} else if (c == ':' && peek2() == '=') {
			next();
			next();
			token = new ScannerToken(ScannerToken.TOKEN_ASSIGNMENT);
		} else if (Character.isJavaIdentifierStart(c)) {
			token = readId();
		} else if (c == '.') {
			next();
			token = new ScannerToken(ScannerToken.TOKEN_STATEMENT_END);
		} else if (c == '^') {
			next();
			token = new ScannerToken(ScannerToken.TOKEN_RETURN);
		} else if (c == '|') {
			next();
			token = new ScannerToken(ScannerToken.TOKEN_TEMPS);
		} else if (c == ';') {
			next();
			token = new ScannerToken(ScannerToken.TOKEN_CASCADE);
		} else if (isBinaryChar(c)) {
			token = readBinary();
		} else if (c == ':') {
			token = readBlockTemp();
		} else {
			throw new IllegalStateException("Scanner could not interpret char:" + c);
		}

		return token;
	}

	public ScannerToken advanceAndCheckType(int requiredType) {
		ScannerToken token = advance();
		token.checkType(requiredType);
		return token;
	}

	public ScannerToken advanceAndCheckType(int requiredType1, int requiredType2) {
		ScannerToken token = advance();
		token.checkType(requiredType1, requiredType2);
		return token;
	}

	private boolean atEnd() {
		return index >= contents.length();
	}

	private boolean isBinaryChar(char c) {
		return c == '=' || c == '<' || c == '>' || c == '+' || c == '-' || c == '*' || c == '/' || c == '~' || c == '@' || c == '\\' || c == ',';
	}

	private char next() {
		char c = peek();
		index++;
		return c;
	}

	private char peek() {
		return contents.charAt(index);
	}

	private char peek2() {
		return contents.charAt(index + 1);
	}

	private ScannerToken readBinary() {
		String value = String.valueOf(next());
		if (isBinaryChar(peek())) {
			value = value + next();
		}
		return new ScannerToken(ScannerToken.TOKEN_BINARY, value);
	}

	private ScannerToken readBlockTemp() {
		next();
		ScannerToken id = readId();
		id.tokenType = ScannerToken.TOKEN_BLOCK_TEMP;
		return id;
	}

	private ScannerToken readCharacter() {
		next();
		char c = next();
		String s;
		if (c == '\\') {
			s = "\\\\";
		} else if (c == '\'') {
			s = "\\\'";
		} else {
			s = String.valueOf(c);
		}
		return new ScannerToken(ScannerToken.TOKEN_CHARACTER, s);
	}

	private ScannerToken readComment() {
		next();
		int start = index;
		char c;
		while ((c = next()) != '"');

		String value = contents.substring(start, index - 1);
		return new ScannerToken(ScannerToken.TOKEN_COMMENT, value);
	}

	private ScannerToken readId() {
		return readId(true);
	}
	
	private ScannerToken readId(boolean stopAtColon) {
		int start = index;
		next();
		char c;
		while (Character.isJavaIdentifierPart(c = peek()) || c == ':' || (c == '.' && Character.isJavaIdentifierStart(peek2()))) {
			next();
			if (c == ':' && stopAtColon) {
				break;
			}
		}

		String value = contents.substring(start, index);
		if (value.indexOf('.') != -1) {
			if (value.startsWith("create.")) {
				boolean isKeyword = value.endsWith(":");
				value = value.substring(0, value.indexOf('.'));
				//FIXMEvalue = value.replaceAll("\\.", "");
				if (isKeyword) {
					value = value + ":";
				}
			}
		}
		//	value = value.replace('.', '_');
		value = value.replaceAll("\\.", "");

		if (value.endsWith(":")) {
			return new ScannerToken(ScannerToken.TOKEN_KEYWORD, value);
		} else {
			return new ScannerToken(ScannerToken.TOKEN_WORD, value);
		}
	}

	private ScannerToken readNumber() {
		int start = index;
		next();
		char c;
		while (Character.isLetterOrDigit(c = peek()) || (c == '.' && Character.isLetterOrDigit(peek2()))) {
			next();
		}
		String number = contents.substring(start, index);
		if (number.indexOf('.') == -1) {
			int radix = 10;
			int radixIndex = number.indexOf('r');
			if (radixIndex != -1) {
				radix = Integer.parseInt(number.substring(0, radixIndex));
			}
			long value = Long.parseLong(number.substring(radixIndex + 1), radix);
			return new ScannerToken(ScannerToken.TOKEN_INTEGER, value, radix);
		} else {
			double value = Double.parseDouble(number);
			return new ScannerToken(ScannerToken.TOKEN_DOUBLE, value);
		}
	}

	private ScannerToken readString() {
		next();
		int start = index;
		char c;
		do {
			c = next();
			while (c == '\'' && !atEnd() && peek() == '\'') {
				next();
				c = next();
			}
		} while (c != '\'');

		String value = contents.substring(start, index - 1);
		value = value.replaceAll("''", "'");
		return new ScannerToken(ScannerToken.TOKEN_STRING, value);
	}

	private ScannerToken readSymbol() {

		next();
		ScannerToken id;
		char c = peek();
		if (c == '\'') {
			id = readString();
			id.tokenString = id.tokenString.replace('.', '_');
			id.tokenString = id.tokenString.replace(' ', '_');
			id.tokenString = id.tokenString.replace(':', '_');
		} else if (c == '(') {
			id = new ScannerToken(ScannerToken.TOKEN_SYMBOL, "(");
		} else {
			id = readId(false);
		}
		id.tokenType = ScannerToken.TOKEN_SYMBOL;
		return id;
	}
}