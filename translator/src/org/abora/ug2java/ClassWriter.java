/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ug2java;

import java.util.*;
import java.io.*;

import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.stscanner.ChunkDetails;
import org.abora.ug2java.stscanner.ChunkParser;
import org.abora.ug2java.stscanner.ScannerToken;
import org.abora.ug2java.stscanner.SmalltalkScanner;
import org.abora.ug2java.transform.TransformMethod;

public class ClassWriter {
	public String className;
	public String superclassName;
	public String classCategory;
	public String comment;
	public Vector classQuotes = new Vector();
	public Vector instanceMethods = new Vector();
	public Vector classMethods = new Vector();
	public boolean quoteSmalltalk = true;

	private SortedSet importedPackages = new TreeSet();
	public Hashtable packageLookup = new Hashtable();
	
	private TransformMethod methodTransformer = new TransformMethod(this);

	static final String CATEGORY_SEPARATOR = "-";
	static final String PACKAGE_SEPARATOR = ".";

	private int JAVADOC_MARGIN = 90;
	
	private static final boolean INCLUDE_METHOD_BODIES = true;

	static final Hashtable LOOKUP_TYPES = new Hashtable();
	{
		LOOKUP_TYPES.put("BooleanVar", "boolean");
		LOOKUP_TYPES.put("Boolean", "boolean");
		LOOKUP_TYPES.put("IntegerVar", "IntegerVar");
		LOOKUP_TYPES.put("UInt32", "int");
		LOOKUP_TYPES.put("Int32", "int");
		LOOKUP_TYPES.put("UInt8", "byte");
		LOOKUP_TYPES.put("Int8", "byte");
		//		LOOKUP_TYPES.put("UInt8Array", "byte[]");
		LOOKUP_TYPES.put("Uint3", "byte");
		LOOKUP_TYPES.put("UInt4", "byte");
		LOOKUP_TYPES.put("Int4", "byte");
		LOOKUP_TYPES.put("IEEEDoubleVar", "double");
		LOOKUP_TYPES.put("IEEEFloatVar", "float");
		LOOKUP_TYPES.put("IEEE64", "double");
		LOOKUP_TYPES.put("IEEE32", "float");
		
		// total guess work		
		LOOKUP_TYPES.put("size_U_t", "int");
		LOOKUP_TYPES.put("size", "int");
		LOOKUP_TYPES.put("UNKNOWN", "Object");

		LOOKUP_TYPES.put("ostream", "PrintWriter");
	}

	static final Set JAVA_KEYWORDS = new HashSet();
	{
		JAVA_KEYWORDS.add("import");
		JAVA_KEYWORDS.add("class");
		JAVA_KEYWORDS.add("int");
		JAVA_KEYWORDS.add("package");
		JAVA_KEYWORDS.add("extends");
		JAVA_KEYWORDS.add("byte");
		JAVA_KEYWORDS.add("char");
		JAVA_KEYWORDS.add("float");
		JAVA_KEYWORDS.add("double");
		JAVA_KEYWORDS.add("long");
		JAVA_KEYWORDS.add("instanceof");
		JAVA_KEYWORDS.add("final");
		JAVA_KEYWORDS.add("public");
		JAVA_KEYWORDS.add("protected");
		JAVA_KEYWORDS.add("private");
		JAVA_KEYWORDS.add("static");
		JAVA_KEYWORDS.add("abstract");
		JAVA_KEYWORDS.add("interface");
		JAVA_KEYWORDS.add("synchonized");

		JAVA_KEYWORDS.add("do");
	}

	static final Hashtable OVERRIDE_RETURN_TYPE = new Hashtable();
	{
		OVERRIDE_RETURN_TYPE.put("actualHashForEqual", "int");
		OVERRIDE_RETURN_TYPE.put("make", "Heaper");
		OVERRIDE_RETURN_TYPE.put("isEqual", "boolean");
		OVERRIDE_RETURN_TYPE.put("isUnlocked", "boolean");
		OVERRIDE_RETURN_TYPE.put("displayString", "String");
		OVERRIDE_RETURN_TYPE.put("exportName", "String");
	}

	static final Set OVERRIDE_STATIC = new HashSet();
	{
		OVERRIDE_STATIC.add("asOop");
		OVERRIDE_STATIC.add("getCategory");
		OVERRIDE_STATIC.add("passe");
		OVERRIDE_STATIC.add("unimplemented");
	}

	public static final Hashtable OVERRIDE_CALLS = new Hashtable();
	{
		OVERRIDE_CALLS.put("atStore", "store");
		OVERRIDE_CALLS.put("atStoreInt", "storeInt");
		OVERRIDE_CALLS.put("atStoreInteger", "storeInteger");
		OVERRIDE_CALLS.put("atStoreIntegerVar", "storeIntegerVar");
		OVERRIDE_CALLS.put("atStoreMany", "storeMany");
		OVERRIDE_CALLS.put("atStoreValue", "storeValue");
		OVERRIDE_CALLS.put("atStoreValue", "storeValue");
		OVERRIDE_CALLS.put("atStoreUInt", "storeUInt");
	}
	/**
	 * ClassWriter constructor comment.
	 */
	public ClassWriter(Hashtable packageLookup) {
		super();
		this.packageLookup = packageLookup;
	}

	protected String appendKeyword(String existingKeywords, String newKeyword) {
		String w = newKeyword.substring(0, newKeyword.length() - 1);
		if (existingKeywords.length() != 0) {
			w = Character.toUpperCase(w.charAt(0)) + w.substring(1, w.length());
		}
		if (existingKeywords.length() > 0 && w.equals("With")) {
			// ignore
		} else {
			existingKeywords = existingKeywords + w;
		}
		return existingKeywords;
	}

	protected boolean expressionIsEmptyOrComments(Vector expression) {
		for (Enumeration e = expression.elements(); e.hasMoreElements();) {
			JavaToken token = (JavaToken) e.nextElement();
			if (!(token instanceof JavaComment)) {
				return false;
			}
		}
		return true;
	}

	protected int findStartOfExpression(Vector expression) {
		int startIndex = 0;
		while (startIndex < expression.size()) {
			JavaToken test = (JavaToken) expression.elementAt(startIndex);
			if (((test instanceof JavaKeyword) && test.value.equals("return"))
				|| (test instanceof JavaComment)
				|| (test instanceof JavaAssignment)
				|| (startIndex + 1 < expression.size() && (expression.elementAt(startIndex + 1) instanceof JavaAssignment))) {
				startIndex++;
			} else {
				break;
			}
		}
		return startIndex;
	}

	protected String getJavaSafeWord(String element) {
		if (JAVA_KEYWORDS.contains(element)) {
			element = element + "x";
		}
		if (element.equals("=")) {
			element = "equalsX";
		}
		return element;
	}

	protected String getPackage() {
		return transformCategory(PACKAGE_SEPARATOR);
	}

	protected String getPackageDirectory() {
		return transformCategory(File.separator);
	}

	public void includeImportForType(String type) {
		String importPackage = (String) packageLookup.get(type);
		if (importPackage != null) {
			importedPackages.add(importPackage + "." + type);
		}
	}

	public static String lineSeparator() {
		return System.getProperty("line.separator");
	}

	protected String lookupType(String xanaduType) {

		String type = (String) LOOKUP_TYPES.get(xanaduType);
		if (type == null) {
			type = xanaduType;
		}

		//TODO ugly double duty
		includeImportForType(type);

		return type;
	}

	protected String nextType(ChunkParser parser) {
		String type;
		parser.skipWhitespace();
		if (parser.peek() == '{') {
			type = parser.nextWord();
			type = readBracketType(parser, type);
			if (type.equals("void")) {
				type = "VoidStar";
				type = lookupType(type);
			}
		} else {
			type = "Object";
		}
		return type;
	}

	protected String overrideReturnType(String methodName, String returnType) {
		if (OVERRIDE_RETURN_TYPE.containsKey(methodName)) {
			returnType = (String) OVERRIDE_RETURN_TYPE.get(methodName);
			returnType = lookupType(returnType);
		}
		return returnType;
	}

	protected String parseJavaSafeVarNameDeclaration(SmalltalkScanner scanner) {
		scanner.token.checkType(ScannerToken.TOKEN_WORD);
		String varName = scanner.token.tokenString;
		scanner.advance();
		while (scanner.token.tokenType == ScannerToken.TOKEN_STATEMENT_END) {
			// work around the . separated names in x++
			scanner.advanceAndCheckType(ScannerToken.TOKEN_WORD);
			varName = varName + scanner.token.tokenString;
			scanner.advance();
		}
		return getJavaSafeWord(varName);
	}

	protected String parseParameterType(SmalltalkScanner scanner) {
		return parseType(scanner, "Object");
	}

	protected String parseReturnType(SmalltalkScanner scanner) {
		String type = parseType(scanner, "void");
		if (type.equals("INLINE") || type.equals("NOACK")) {
			type = "void";
		}
		return type;
	}

	protected Vector parseTemps(SmalltalkScanner scanner) {
		Vector tokens = new Vector();

		while (scanner.token.tokenType != ScannerToken.TOKEN_TEMPS) {
			String tempName = scanner.token.tokenString;
			scanner.advance();

			String tempType = parseParameterType(scanner);
			tokens.add(new JavaType(tempType));
			tokens.add(new JavaIdentifier(tempName));
			tokens.add(new JavaStatementTerminator());
		}
		scanner.advance();
		return tokens;
	}

	protected void parseTemps(SmalltalkScanner scanner, PrintWriter writer) {
		while (scanner.token.tokenType != ScannerToken.TOKEN_TEMPS) {
			String tempName = scanner.token.tokenString;
			scanner.advance();
			String tempType = parseParameterType(scanner);
			writer.println(tempType + " " + tempName + ";");
		}
		scanner.advance();
	}

	protected String parseType(SmalltalkScanner scanner, String missingType) {
		String type = missingType;

		if (scanner.token.tokenType == ScannerToken.TOKEN_TYPE_START) {
			scanner.advance();
			if (scanner.token.tokenType == ScannerToken.TOKEN_BRACKET_START) {
				scanner.advance();
			}
			scanner.token.checkType(ScannerToken.TOKEN_WORD, ScannerToken.TOKEN_SYMBOL);
			type = scanner.token.tokenString;
			if (type.equals("void")) {
				scanner.advance();
				if (scanner.token.tokenType == ScannerToken.TOKEN_WORD && scanner.token.tokenString.equals("star")) {
					type = "VoidStar";
				}
			} else if (type.equals("Character") || type.equals("char")) {
				scanner.advance();
				if (scanner.token.tokenType == ScannerToken.TOKEN_WORD
					&& (scanner.token.tokenString.equals("star") || scanner.token.tokenString.equals("vector"))) {
					type = "String";
				}
			}
			type = lookupType(type);
			while (scanner.token.tokenType != ScannerToken.TOKEN_TYPE_END) {
				scanner.advance();
			}
			scanner.advance();
		}
		return type;
	}

	protected String readBracketType(ChunkParser parser, String type) {
		type = parser.nextWord();
		if (type.startsWith("#")) {
			// guess for: {void star} fetchNewRawSpace: size {#size.U.t var}
			type = "int";
		} else if (type.startsWith("(")) {
			// guess for: 		myDetectors {(PrimSet NOCOPY of: FeFillRangeDetector)| NULL}'
			type = parser.nextWord();
		}
		type = lookupType(type);
		while (!parser.nextWord().equals("}"));
		return type;
	}

	protected String stringReplaceWith(String s, String find, String replaceWith) {
		StringBuffer buffer = new StringBuffer();
		int start = 0;
		int match;
		while ((match = s.indexOf(find, start)) != -1) {
			buffer.append(s.substring(start, match));
			buffer.append(replaceWith);
			start = match + find.length();
		}
		buffer.append(s.substring(start));
		return buffer.toString();
	}

	protected void includeAnyReferencedTypes(MethodBody body) {
		Vector tokens = body.tokens;
		for (int i = 0; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.elementAt(i);
			if ((token instanceof JavaIdentifier || token instanceof JavaType || token instanceof JavaCast)
				&& Character.isJavaIdentifierStart(token.value.charAt(0))) {
				includeImportForType(token.value);
			}
		}
	}

	public void write(String baseDirectory) throws Exception {
		System.out.println("Writing class: " + getPackage() + "." + className);

		File dir = new File(baseDirectory, getPackageDirectory());
		dir.mkdirs();

		File javaFile = new File(dir, className + ".java");
		FileWriter fileWriter = new FileWriter(javaFile);
		PrintWriter writer = new PrintWriter(fileWriter);
		try {
			String classDefinition = writeClassDefinition();
			includeImportForType(superclassName);

			writeFileComment(writer);
			writer.println("package " + getPackage() + ";");
			writer.println();
			writeImports(writer);
			writer.println();
			writer.print(classDefinition);
		} finally {
			fileWriter.close();
		}
	}

	private void writeImports(PrintWriter writer) {
		for (Iterator iterator = importedPackages.iterator(); iterator.hasNext();) {
			String importPackage = (String) iterator.next();
			if (!importPackage.equals(getPackage())) {
				writer.println("import " + importPackage + ";");
			}
		}
	}

	protected void writeAsJavadocComment(PrintWriter writer, String comment) {
		writer.println("/**");
		for (StringTokenizer tokenizer = new StringTokenizer(comment, "\n"); tokenizer.hasMoreTokens();) {
			String line = tokenizer.nextToken().trim();
			int start = 0;
			while (start < line.length()) {
				int end = line.length() - 1;
				if (start + JAVADOC_MARGIN < line.length()) {
					end = Math.min(start + JAVADOC_MARGIN, end);
					while (end > start && !Character.isWhitespace(line.charAt(end))) {
						end -= 1;
					}
					while (end > start && Character.isWhitespace(line.charAt(end))) {
						end -= 1;
					}
					if (end == start) {
						end = line.length() - 1;
					}
				}
				writer.println(" * " + line.substring(start, end + 1));
				start = end + 1;
				while (start < line.length() - 1 && Character.isWhitespace(line.charAt(start))) {
					start += 1;
				}
			}
		}
		writer.println(" */");
	}

	protected void writeFileComment(PrintWriter writer) {
		final String fileComment =
			"Abora-Gold\n"
				+ "Part of the Abora hypertext project: http://www.abora.org\n"
				+ "Copyright 2003 David G Jones\n"
				+ " \n"
				+ "Translated from Udanax-Gold source code: http://www.udanax.com\n"
				+ "Copyright 1979-1999 Udanax.com. All rights reserved";
		writeAsComment(writer, fileComment);
	}

	protected void writeAsComment(PrintWriter writer, String comment) {
		writer.println("/*");
		for (StringTokenizer tokenizer = new StringTokenizer(comment, "\n"); tokenizer.hasMoreTokens();) {
			String line = tokenizer.nextToken().trim();
			writer.println(" * " + line);
		}
		writer.println(" */");
	}

	protected void writeAsQuote(PrintWriter writer, String context, String comment) {
		comment = stringReplaceWith(comment, "/*", "/-");
		comment = stringReplaceWith(comment, "*/", "-/");

		writer.println("/*");
		writer.println(context);
		for (StringTokenizer tokenizer = new StringTokenizer(comment, "\n"); tokenizer.hasMoreTokens();) {
			writer.println(tokenizer.nextToken());
		}
		writer.println("*/");
	}

	public String writeClassDefinition() throws Exception {
		StringWriter stringWriter = new StringWriter();
		PrintWriter writer = new PrintWriter(stringWriter);
		try {
			writer.println();
			if (comment != null) {
				writeAsJavadocComment(writer, comment);
			}
			writer.println("public class " + className + " extends " + superclassName + " {");

			String classDefinition = ((ChunkDetails) classQuotes.firstElement()).contents;
			ChunkParser parser = new ChunkParser(classDefinition);
			for (int i = 0; i < 5; i++) {
				parser.nextWord();
			}
			writeVariables(writer, parser, "");
			parser.nextWord();
			writeVariables(writer, parser, "static ");

			for (Enumeration e = classQuotes.elements(); e.hasMoreElements();) {
				ChunkDetails comment = (ChunkDetails) e.nextElement();
				writeAsQuote(writer, comment.context, comment.contents);
			}
			writeMethods(writer, instanceMethods, "");
			writeMethods(writer, classMethods, "static ");
			writer.println("}");
		} finally {
			writer.close();
		}
		return stringWriter.toString();
	}

	public void writeMethod(PrintWriter writer, ChunkDetails methodDetails, String modifiers) {
		String smalltalkMethod = methodDetails.contents;
		if (smalltalkMethod.trim().length() == 0) {
			return;
		}

		String params = "";
		String methodName = "";
		SmalltalkScanner scanner = new SmalltalkScanner(smalltalkMethod);
		String returnType = parseReturnType(scanner);
		if (scanner.token.tokenType == ScannerToken.TOKEN_KEYWORD) {
			while (scanner.token.tokenType == ScannerToken.TOKEN_KEYWORD) {
				methodName = appendKeyword(methodName, scanner.token.tokenString);

				scanner.advance();
				String varName = parseJavaSafeVarNameDeclaration(scanner);
				String type = parseParameterType(scanner);

				if (params.length() != 0) {
					params = params + ", ";
				}
				params = params + type + " " + varName;
			}
		} else {
			methodName = scanner.token.tokenString;
			scanner.advance();
			if (methodName.equals("=")) {
				String varName = parseJavaSafeVarNameDeclaration(scanner);
				String type = parseParameterType(scanner);
				params = type + " " + varName;
			}
		}
		methodName = getJavaSafeWord(methodName);

		returnType = overrideReturnType(methodName, returnType);
		if (methodName.equals("create") && modifiers.indexOf("static") == -1) {
			modifiers = "";
			returnType = "";
			methodName = className;
		}
		if (OVERRIDE_STATIC.contains(methodName) && modifiers.indexOf("static") == -1) {
			modifiers = "static " + modifiers;
		}

		if (scanner.token.tokenType == ScannerToken.TOKEN_COMMENT) {
			writeAsJavadocComment(writer, scanner.token.tokenString);
			scanner.advance();
		}

		writer.println("public " + modifiers + returnType + " " + methodName + "(" + params + ") {");
		
		if (INCLUDE_METHOD_BODIES) {
			writeTranslatedMethod(writer, scanner);
		} else {
			writer.write("throw new UnsupportedOperationException();");
		}
		if (quoteSmalltalk) {
			writeAsQuote(writer, methodDetails.context, smalltalkMethod);
		}
		writer.println("}");
	}

	protected void writeMethods(PrintWriter writer, Vector methods, String modifiers) throws Exception {
		for (Enumeration e = methods.elements(); e.hasMoreElements();) {
			ChunkDetails methodDetails = (ChunkDetails) e.nextElement();
			writer.println();
			writeMethod(writer, methodDetails, modifiers);
			writer.flush();
		}
	}

	protected MethodBody readMethodUnit(SmalltalkScanner scanner) {
		Vector tokens = new Vector();
		Vector expression = new Vector();

		boolean atExpressionStart = true;
		boolean endOfUnit = false;
		boolean endOfExpression = false;
		JavaCallKeywordStart existingKeyword = null;
		boolean hasIf = false;
		boolean hasFor = false;

		while (!endOfUnit && scanner.token.tokenType != ScannerToken.TOKEN_END) {
			switch (scanner.token.tokenType) {
				case ScannerToken.TOKEN_TEMPS :
					{
						scanner.advance();
						if (atExpressionStart) {
							expression.addAll(parseTemps(scanner));
						} else {
							//TODO udanax seem to use this to give optional types to cast
							expression.add(new JavaKeyword("|"));
						}
						endOfExpression = true;
						break;
					}
				case ScannerToken.TOKEN_RETURN :
					{
						if (!expressionIsEmptyOrComments(expression)) {
							throw new IllegalStateException("Return must be first token in expression");
						}
						expression.add(new JavaKeyword("return"));
						scanner.advance();
						break;
					}
				case ScannerToken.TOKEN_STATEMENT_END :
					{
						expression.add(new JavaStatementTerminator());
						endOfExpression = true;
						scanner.advance();
						break;
					}
				case ScannerToken.TOKEN_ASSIGNMENT :
					{
						expression.add(new JavaAssignment());
						scanner.advance();
						atExpressionStart = true;
						break;
					}
				case ScannerToken.TOKEN_BINARY :
					{
						String binary = scanner.token.tokenString;
						if (binary.equals("=")) {
							binary = "==";
						} else if (binary.equals("~=") || binary.equals("~~")) {
							binary = "!=";
						} else if (binary.equals(",")) {
							//TODO is this ok?
							binary = "+";
						} else if (binary.equals("\\\\")) {
							//TODO this is not technically accurate as \\ truncates to negative infinity
							// while C % truncates to zero
							binary = "%";
						} else if (binary.equals("<<")) {
							//TODO not good enough
							binary = ".print()";
						}
						expression.add(new JavaKeyword(binary));
						scanner.advance();
						atExpressionStart = true;
						break;
					}
				case ScannerToken.TOKEN_BRACKET_START :
					{
						expression.add(new JavaParenthesisStart());
						scanner.advance();
						expression.addAll(readMethodUnit(scanner).tokens);
						scanner.token.checkType(ScannerToken.TOKEN_BRACKET_END);
						expression.add(new JavaParenthesisEnd());
						scanner.advance();
						atExpressionStart = false;
						break;
					}
				case ScannerToken.TOKEN_BRACKET_END :
					{
						endOfUnit = true;
						break;
					}
				case ScannerToken.TOKEN_BLOCK_START :
					{
						expression.add(new JavaBlockStart());
						scanner.advance();
						if (scanner.token.tokenType == ScannerToken.TOKEN_BLOCK_TEMP) {
							if (hasFor) {
								String tempName = scanner.token.tokenString;
								scanner.advance();
								String tempType = parseParameterType(scanner);
								expression.add(new JavaType(tempType));
								expression.add(new JavaIdentifier(tempName));
								expression.add(new JavaKeyword("="));
								expression.add(new JavaParenthesisStart());
								expression.add(new JavaType(tempType));
								expression.add(new JavaParenthesisEnd());
								expression.add(new JavaIdentifier("iterator"));
								expression.add(new JavaCallStart("next"));
								expression.add(new JavaCallEnd());
								expression.add(new JavaStatementTerminator());
								scanner.token.checkType(ScannerToken.TOKEN_TEMPS);
								scanner.advance();
							} else {
								expression.addAll(parseTemps(scanner));
							}
						}
						expression.addAll(readMethodUnit(scanner).tokens);
						scanner.token.checkType(ScannerToken.TOKEN_BLOCK_END);
						expression.add(new JavaBlockEnd());
						scanner.advance();
						atExpressionStart = true;
						break;
					}
				case ScannerToken.TOKEN_BLOCK_END :
					{
						endOfUnit = true;
						break;
					}
				case ScannerToken.TOKEN_INTEGER :
					{
						String value = "";
						if (scanner.token.tokenIntRadix == 16) {
							value = "0x";
						} else if (scanner.token.tokenIntRadix == 8) {
							value = "0";
							// TODO ignore any other radixes and show in base 10 instead
						}
						value = value + Long.toString(scanner.token.tokenInt, scanner.token.tokenIntRadix);
						if (scanner.token.tokenInt > Integer.MAX_VALUE || scanner.token.tokenInt < Integer.MIN_VALUE) {
							value = value + "L";
						}
						expression.add(new JavaLiteral(value));
						atExpressionStart = false;
						scanner.advance();
						break;
					}
				case ScannerToken.TOKEN_DOUBLE :
					{
						String value = Double.toString(scanner.token.tokenDouble);
						expression.add(new JavaLiteral(value));
						atExpressionStart = false;
						scanner.advance();
						break;
					}
				case ScannerToken.TOKEN_WORD :
					{
						String word = scanner.token.tokenString;
						if (word.equals("NULL") || word.equals("nil")) {
							word = "null";
						} else if (word.equals("self")) {
							word = "this";
						}
						if (!atExpressionStart) {
							expression.add(new JavaCallStart(word));
							expression.add(new JavaCallEnd());
						} else {
							if (word.equals("UInt32Zero") || word.equals("Int32Zero") || word.equals("Int0")) {
								expression.add(new JavaLiteral("0"));
							} else if ((word.equals("IntegerVar0")) || word.equals("IntegerVarZero")) {
								expression.add(new JavaIdentifier("IntegerVar"));
								expression.add(new JavaCallStart("zero"));
								expression.add(new JavaCallEnd());
							} else {
								expression.add(new JavaIdentifier(word));
							}
						}
						scanner.advance();
						atExpressionStart = false;
						break;
					}
				case ScannerToken.TOKEN_KEYWORD :
					{
						String word = scanner.token.tokenString;
						String wordTrimmed = word.substring(0, word.length() - 1);
						if (wordTrimmed.equals("ifTrue") || wordTrimmed.equals("ifFalse")) {
							if (hasIf) {
								expression.add(new JavaKeyword("else"));
							} else {
								int startIndex = findStartOfExpression(expression);
								expression.add(startIndex, new JavaKeyword("if"));
								if (wordTrimmed.equals("ifFalse")) {
									expression.add(startIndex + 1, new JavaParenthesisStart());
									expression.add(startIndex + 2, new JavaKeyword("!"));
									expression.add(new JavaParenthesisEnd());
								}
								hasIf = true;
							}
						} else if (wordTrimmed.equals("forEach")) {
							hasFor = true;
							int startIndex = findStartOfExpression(expression);
							expression.add(startIndex, new JavaKeyword("for"));
							expression.add(startIndex + 1, new JavaParenthesisStart());
							expression.add(startIndex + 2, new JavaType("Iterator"));
							expression.add(startIndex + 3, new JavaIdentifier("iterator"));
							expression.add(startIndex + 4, new JavaKeyword("="));
							expression.add(new JavaCallStart(wordTrimmed));
							expression.add(new JavaCallEnd());
							expression.add(new JavaKeyword(";"));
							expression.add(new JavaIdentifier("iterator"));
							expression.add(new JavaCallStart("hasNext"));
							expression.add(new JavaCallEnd());
							expression.add(new JavaKeyword(";"));
							expression.add(new JavaParenthesisEnd());

						} else {
							if (existingKeyword != null) {
								existingKeyword.value = appendKeyword(existingKeyword.value, word);
								expression.add(new JavaCallArgumentSeparator());
							} else {
								existingKeyword = new JavaCallKeywordStart(wordTrimmed);
								expression.add(existingKeyword);
							}
						}
						scanner.advance();
						atExpressionStart = true;
						break;
					}
				case ScannerToken.TOKEN_STRING :
					{
						String safeString = stringReplaceWith(scanner.token.tokenString, "\"", "\\\"");
						safeString = stringReplaceWith(safeString, "\n", "\\n\"+\n\"");
						safeString = "\"" + safeString + "\"";
						expression.add(new JavaLiteral(safeString));
						scanner.advance();
						atExpressionStart = false;
						break;
					}
				case ScannerToken.TOKEN_COMMENT :
					{
						expression.add(new JavaComment(scanner.token.tokenString));
						scanner.advance();
						break;
					}
				case ScannerToken.TOKEN_SYMBOL :
					{
						String value = scanner.token.tokenString;
						scanner.advance();
						if (value.equals("(") && scanner.token.tokenType == ScannerToken.TOKEN_BRACKET_END) {
							//TODO special case for #()
							expression.add(new JavaIdentifier("Array"));
							expression.add(new JavaCallStart("new"));
							expression.add(new JavaCallEnd());
							scanner.advance();
						} else {
							StringBuffer buffer = new StringBuffer();
							for (int i = 0; i < value.length(); i++) {
								char c = value.charAt(i);
								if (i > 0 && Character.isUpperCase(c) && Character.isLowerCase(value.charAt(i - 1))) {
									buffer.append('_');
								}
								buffer.append(Character.toUpperCase(c));
							}
							expression.add(new JavaIdentifier(buffer.toString()));
						}
						atExpressionStart = false;
						break;
					}
				case ScannerToken.TOKEN_CHARACTER :
					{
						String value = "'" + scanner.token.tokenString + "'";
						expression.add(new JavaLiteral(value));
						scanner.advance();
						atExpressionStart = false;
						break;
					}
				case ScannerToken.TOKEN_CASCADE :
					{
						//TODO not  really good enough
						scanner.advance();
						break;
					}
				case ScannerToken.TOKEN_CHUNK :
					{
						scanner.advance();
						endOfUnit = true;
						break;
					}
				default :
					{
						throw new IllegalStateException("Unexpected token type while writing method");
					}
			}
			if (endOfUnit || endOfExpression) {
				if (existingKeyword != null) {
					JavaToken closingKeyword = new JavaCallEnd();
					if (expression.get(expression.size() - 1) instanceof JavaStatementTerminator) {
						expression.add(expression.size() - 1, closingKeyword);
					} else {
						expression.add(closingKeyword);
					}
				}
				tokens.addAll(expression);

				expression = new Vector();
				endOfExpression = false;
				atExpressionStart = true;
				existingKeyword = null;
				hasIf = false;
				hasFor = false;
			}
		}
		return new MethodBody(tokens);
	}

	protected void writeTranslatedMethod(PrintWriter writer, SmalltalkScanner scanner) {
		MethodBody methodBody = readMethodUnit(scanner);

		methodTransformer.transform(methodBody);
		includeAnyReferencedTypes(methodBody);

		StringBuffer buffer = new StringBuffer();
		for (Enumeration e = methodBody.tokens.elements(); e.hasMoreElements();) {
			JavaToken token = (JavaToken) e.nextElement();
			token.write(buffer);
		}
		writer.print(buffer.toString());
	}

	protected void writeVariables(PrintWriter writer, ChunkParser parser, String modifiers) throws Exception {
		if (!parser.nextWord().equals("'")) {
			throw new Exception("Expected variables");
		}
		String w = parser.nextWord();
		while (!w.equals("'")) {
			String varName = getJavaSafeWord(w);
			String type = nextType(parser);
			writer.println("\tprotected " + modifiers + type + " " + varName + ";");
			w = parser.nextWord();
		}
	}

	protected String transformCategory(String separator) {
		StringBuffer buffer = new StringBuffer();
		for (StringTokenizer tokenizer = new StringTokenizer(classCategory, CATEGORY_SEPARATOR); tokenizer.hasMoreTokens();) {
			if (buffer.length() > 0) {
				buffer.append(separator);
			}
			String element = tokenizer.nextToken().toLowerCase();
			element = getJavaSafeWord(element);
			buffer.append(element);
		}
		String category = buffer.toString();
		if (category.startsWith("xanadu" + separator)) {
			category = category.substring(("xanadu" + separator).length());
		}
		return "org" + separator + "abora" + separator + "gold" + separator + category;
	}
}