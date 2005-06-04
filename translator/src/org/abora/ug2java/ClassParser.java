package org.abora.ug2java;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Vector;

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
import org.abora.ug2java.javatoken.JavaLoopTerminator;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.stscanner.ChunkDetails;
import org.abora.ug2java.stscanner.ChunkParser;
import org.abora.ug2java.stscanner.ScannerToken;
import org.abora.ug2java.stscanner.SmalltalkScanner;
import org.abora.ug2java.transform.method.MethodTransformation;
import org.abora.ug2java.transform.method.intra.TransformIntraMethod;



public class ClassParser {
	
	private JavaClass javaClass;
	private MethodTransformation methodTransformer = new TransformIntraMethod();
	private int stompLevel = 1;
	
	public static final String HEAPER_CLASS = "Heaper";
	public static final String STEPPER_CLASS = "Stepper";
	public static final String TABLE_STEPPER_CLASS = "TableStepper";
	
	static final Map LOOKUP_TYPES;
	static {
		Map table = new Hashtable();
		table.put("BooleanVar", "boolean");
		table.put("Boolean", "boolean");
		table.put("Integer", "int");
		table.put("IntegerVar", "int");
//TODO		table.put("IntegerVar", "IntegerVar");
		table.put("UInt32", "int");
		table.put("Int32", "int");
		table.put("UInt8", "int"/*"byte"*/);
		// mispelling in SimpleShuffler
		table.put("Uint8", "int"/*"byte"*/);
		table.put("Int8", "int"/*"byte"*/);
		//		table.put("UInt8Array", "byte[]");
		table.put("Uint3", "int"/*"byte"*/);
		table.put("UInt4", "int"/*"byte"*/);
		table.put("Int4", "int"/*"byte"*/);
		table.put("UInt1", "int");
		table.put("IEEEDoubleVar", "double");
		table.put("IEEEFloatVar", "float");
		table.put("IEEE64", "double");
		table.put("IEEE32", "float");
		table.put("IEEE8", "float"); //TODO what to do here?
		
//		table.put("Category", "Class");
				
		// total guess work		
		table.put("size_U_t", "int");
		table.put("sizeUt", "int");
//		table.put("size", "int");
		table.put("UNKNOWN", "Object");
		table.put("SnarfID", "int");
		//TODO mispelling?
		table.put("IDRegio", "IDRegion");
		table.put("Symbol", "String");
		table.put("Selector", "String");

		table.put("ostream", "PrintWriter");
		LOOKUP_TYPES = Collections.unmodifiableMap(table);
	}

	static final Set JAVA_RESERVED_WORDS;
	static {
		Set set = new HashSet();
		set.add("abstract");
		set.add("byte");
		set.add("class");
		set.add("char");
		set.add("double");
		set.add("extends");
		set.add("final");
		set.add("float");
		set.add("import");
		set.add("instanceof");
		set.add("int");
		set.add("interface");
		set.add("long");
		set.add("package");
		set.add("private");
		set.add("protected");
		set.add("public");
		set.add("return");
		set.add("static");
		set.add("synchonized");

		set.add("do");
		JAVA_RESERVED_WORDS = Collections.unmodifiableSet(set);
	}

	public static final Set NON_CONSTRUCTORS;
	static {
		Set set = new HashSet();
		set.add("createAfter");
		NON_CONSTRUCTORS = Collections.unmodifiableSet(set);
	}
	
	public static final Map OVERRIDE_RETURN_TYPE;
	static {
		Map table = new Hashtable();
		table.put("actualHashForEqual", "int");
		table.put("isEqual", "boolean");
		table.put("isUnlocked", "boolean");
		table.put("displayString", "String");
		table.put("exportName", "String");
		table.put("inspect", "Object");
		table.put("isFullOrder", "boolean");
		table.put("fetchOldRawSpace", "Array");
		table.put("fetchNewRawSpace", "Array");
		table.put("fluidSpace", "Array");	
		table.put("LPPrimeSizeProvider.make", "PrimeSizeProvider");
		table.put("ActualHashSet.arrayStats", "void");
		table.put("GrandHashTable.make(CoordinateSpace)", "MuTable");

		OVERRIDE_RETURN_TYPE = Collections.unmodifiableMap(table);
	}

	public static final Map OVERRIDE_VOID_RETURN_TYPE;
	static {
		Map table  = new Hashtable();
		table.put("stepper", STEPPER_CLASS);
		table.put("isGenerated", "boolean");
		table.put("SnarfPacker.conistentCount", "int");
		table.put("ScruTable.stepper", "TableStepper");
		table.put("SetTable.stepper", "TableStepper");
		table.put("FluidVar.emulsion", "Emulsion");
		table.put("FluidVar.fluidVar", "Heaper");
		table.put("FluidVar.fluidFetch", "Object");
		table.put("FluidVar.fluidGet", "Object");
		table.put("FluidVar.initialValue", "Heaper");
		table.put("XnRegion.simpleRegions", "Stepper");
		table.put("XnRegion.disjointSimpleRegions", "Stepper");
		table.put("crums", "Array");
		table.put("BeClub.make", "BeWork");
		table.put("Ent.makeHandleFor", "RootHandle");
		table.put("preorderNumber", "int");
		table.put("GlobalEmulsion.globalEmulsion", "Emulsion");
		table.put("GlobalEmulsion.make", "Emulsion");
		table.put("GenericCrossDsp.make", "Mapping");
		table.put("Abraham.dismantleStatistics", "IdentityDictionary");
		table.put("Category.brotherClass", "Class");
		table.put("FakePackageCategory.contentsCategory", "Category");
		table.put("FakePackageCategory.originalContentsCategory", "Category");
		table.put("FakePackageCategory.isConcretePackage", "boolean");
		table.put("FakeCategory.getSuperCategory", "Category");
		table.put("FakeCategory.isEqualOrSubclassOf", "boolean");
		table.put("FakeCategory.originalClass", "Class");
		table.put("PrimSet.createWithExecutor", "PrimSet");
		table.put("HeightChanger.make", "PropChanger");
		table.put("SnarfInfoHandler.create", "SnarfInfoHandler");
		table.put("TextyRcvr.receiveString", "String");
		table.put("asOrderedCollection", "OrderedCollection");
		table.put("SimpleTurtle.make", "Turtle");
		table.put("Category.name", "String");
		table.put("HashSet.make", "MuSet");
		table.put("HashSet.makeIntegerVar", "MuSet");
		table.put("HashSet.makeHeaper", "MuSet");
		table.put("ActualHashSet.make", "MuSet");
		table.put("ActualHashSet.makeIntegerVar", "MuSet");
		table.put("ActualHashSet.makeHeaper", "MuSet");
		table.put("CrossMapping.make(Object)", "Mapping");
		table.put("SnarfPacker.consistentCount", "int");
		table.put("CBlockTrackingPacker.consistentCount", "int");
		table.put("SnarfPacker.make(String)", "DiskManager");
		table.put("StaticFunctionPointer.staticClass", "Class");
		table.put("StaticFunctionPointer.selector", "String");
		table.put("Emulsion.imageEmulsion", "Emulsion");
		table.put("DeletedHeaper.isKindOf", "boolean");
		table.put("DeletedHeaper.getCategory", "Category");
		table.put("Category.getCategory", "Category");
		table.put("PrimSet.create", "PrimSet");
		table.put("IntegerTableStepper.create", "IntegerTableStepper");
		table.put("GrandHashSet.make()", "MuSet");
		table.put("GrandHashSet.make(int)", "MuSet");
		OVERRIDE_VOID_RETURN_TYPE = Collections.unmodifiableMap(table);
	}

	public static final Set OVERRIDE_VOID_RETURN_TYPE_WITH_CLASS;
	static {
		Set set  = new HashSet();
		set.add("make");
		//TODO support "make*"
		set.add("makeIntegerVar");
		set.add("makeCoordinateSpace");
		set.add("makeScruSet");
		set.add("makeJoint");
		set.add("makeXnRegion");
		set.add("makeHeaper");
		set.add("makeCoordinateSpaceWithRegion");
		set.add("makeBertCrum");
		set.add("makeHUpperCrum");
		OVERRIDE_VOID_RETURN_TYPE_WITH_CLASS = Collections.unmodifiableSet(set);
		
	}

	static final Set OVERRIDE_STATIC;
	static {
		Set set  = new HashSet();
		set.add("asOop");
		set.add("getCategory");
		set.add("passe");
		set.add("unimplemented");
		OVERRIDE_STATIC = Collections.unmodifiableSet(set);
	}


	static final String CATEGORY_SEPARATOR = "-";

	private static final String FOR_EACH_STEPPER_VARIABLE = "stomp";
	public static final String ABORA_RUNTIME_EXCEPTION_CLASS = "AboraRuntimeException";

	public void setJavaClass(JavaClass javaClass) {
		this.javaClass = javaClass;
	}
	
	protected String overrideTypeIfNecessary(String xanaduType) {
		String type = (String) LOOKUP_TYPES.get(xanaduType);
		if (type == null) {
			type = xanaduType;
		}
		return type;
	}
	
	protected String lookupType(String xanaduType) {

		String type = overrideTypeIfNecessary(xanaduType);

		//TODO ugly double duty
//		javaClass.includeImportForType(type);

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

	protected String parseJavaSafeVarNameDeclaration(SmalltalkScanner scanner) {
		scanner.token.checkType(ScannerToken.TOKEN_WORD);
		String varName = scanner.token.tokenString;
		scannerAdvance(scanner);
		while (scanner.token.tokenType == ScannerToken.TOKEN_STATEMENT_END) {
			// work around the . separated names in x++
			scanner.advanceAndCheckType(ScannerToken.TOKEN_WORD);
			varName = varName + scanner.token.tokenString;
			scannerAdvance(scanner);
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
			tempName = getJavaSafeWord(tempName);
			scannerAdvance(scanner);

			String tempType = parseParameterType(scanner);
			tokens.add(new JavaType(tempType));
			tokens.add(new JavaIdentifier(tempName));
			tokens.add(new JavaStatementTerminator());
		}
		scannerAdvance(scanner);
		return tokens;
	}

	protected void parseTemps(SmalltalkScanner scanner, PrintWriter writer) {
		while (scanner.token.tokenType != ScannerToken.TOKEN_TEMPS) {
			String tempName = scanner.token.tokenString;
			scannerAdvance(scanner);
			String tempType = parseParameterType(scanner);
			writer.println(tempType + " " + tempName + ";");
		}
		scannerAdvance(scanner);
	}

	protected String parseType(SmalltalkScanner scanner, String missingType) {
		String type = missingType;

		if (scanner.token.tokenType == ScannerToken.TOKEN_TYPE_START) {
			scannerAdvance(scanner);
			if (scanner.token.tokenType == ScannerToken.TOKEN_BRACKET_START) {
				scannerAdvance(scanner);
			}
			scanner.token.checkType(ScannerToken.TOKEN_WORD, ScannerToken.TOKEN_SYMBOL);
			type = scanner.token.tokenString;
			if (type.equals("void")) {
				scannerAdvance(scanner);
				if (scanner.token.tokenType == ScannerToken.TOKEN_WORD && scanner.token.tokenString.equals("star")) {
					//type = "PrimArray";
					type = "PtrArray";
				}
			} else if (type.equals("Character") || type.equals("char")) {
				scannerAdvance(scanner);
				if (scanner.token.tokenType == ScannerToken.TOKEN_WORD
					&& (scanner.token.tokenString.equals("star") || scanner.token.tokenString.equals("vector"))) {
					type = "String";
				}
			} else if (type.equals("UInt8")) {
				scannerAdvance(scanner);
				if (scanner.token.tokenType == ScannerToken.TOKEN_WORD
					&& (scanner.token.tokenString.equals("vector"))) {
					type = "UInt8Array";
				}
			}
			type = lookupType(type);
			while (scanner.token.tokenType != ScannerToken.TOKEN_TYPE_END) {
				scannerAdvance(scanner);
			}
			scannerAdvance(scanner);
		}
		return type;
	}

	protected String readBracketType(ChunkParser parser, String missingType) {
		List typeWords = new ArrayList();
		String word;
		while (!(word = parser.nextWord()).equals("}")) {
			typeWords.add(word);
		}

		String type = missingType;
		if (typeWords.size() > 0) { 
			type = (String)typeWords.get(0);
		}
		if (type.startsWith("#")) {
			// guess for: {void star} fetchNewRawSpace: size {#size.U.t var}
			type = "int";
		} else if (type.startsWith("(")) {
			// guess for: 		myDetectors {(PrimSet NOCOPY of: FeFillRangeDetector)| NULL}'
			type = (String)typeWords.get(1);
		} else if (type.equals("char") || type.equals("Character")) {
			if (typeWords.size() > 1 && typeWords.get(1).equals("star")) {
				type = "String";
			}
		}
		type = lookupType(type);
		return type;
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
				|| ((test instanceof JavaIdentifier) && test.value.equals("return"))
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

	public static String getJavaSafeWord(String element) {
		if (JAVA_RESERVED_WORDS.contains(element)) {
			element = element + "x";
		}
		if (element.equals("=")) {
			element = "equalsX";
		}
		return element;
	}

	protected void parseMethods(Vector methods, String modifiers) throws Exception {
		for (Enumeration e = methods.elements(); e.hasMoreElements();) {
			ChunkDetails methodDetails = (ChunkDetails) e.nextElement();
			JavaMethod javaMethod = parseMethod(methodDetails, modifiers);
			if (javaMethod != null) {
				javaClass.methods.add(javaMethod);
			} else {
				System.out.println("-- Warning: Missing method");
			}
		}
	}

	protected String appendKeyword(String existingKeywords, String newKeyword) {
		String w = newKeyword.substring(0, newKeyword.length() - 1);
		if (existingKeywords.length() != 0) {
			w = Character.toUpperCase(w.charAt(0)) + w.substring(1, w.length());
		}
		if (existingKeywords.length() > 0 && w.startsWith("With")) {
			// ignore
		} else {
			existingKeywords = existingKeywords + w;
		}
		return existingKeywords;
	}

	public JavaMethod parseMethod(ChunkDetails methodDetails, String modifiers) {
		String smalltalkMethod = methodDetails.contents;
		if (smalltalkMethod.trim().length() == 0) {
			return null;
		}
	
		String methodName = "";
		List parameterList = new ArrayList();
		SmalltalkScanner scanner = new SmalltalkScanner(smalltalkMethod);
		String returnType = parseReturnType(scanner);
		if (scanner.token.tokenType == ScannerToken.TOKEN_KEYWORD) {
			while (scanner.token.tokenType == ScannerToken.TOKEN_KEYWORD) {
				methodName = appendKeyword(methodName, scanner.token.tokenString);
	
				scannerAdvance(scanner);
				String varName = parseJavaSafeVarNameDeclaration(scanner);
				String type = parseParameterType(scanner);
	
				JavaField javaField = new JavaField("", type, varName);
				parameterList.add(javaField);
			}
		} else {
			methodName = scanner.token.tokenString;
			scannerAdvance(scanner);
			if (methodName.equals("=")) {
				String varName = parseJavaSafeVarNameDeclaration(scanner);
				String type = parseParameterType(scanner);
				JavaField javaField = new JavaField("", type, varName);
				parameterList.add(javaField);
			}
		}
		methodName = getJavaSafeWord(methodName);
	
		if (methodName.startsWith("create")  && modifiers.indexOf("static") == -1 && (!NON_CONSTRUCTORS.contains(methodName))) {
			modifiers = "";
			returnType = "";
			methodName = javaClass.className;
		}
		if (OVERRIDE_STATIC.contains(methodName) && modifiers.indexOf("static") == -1) {
			modifiers = "static " + modifiers;
		}
	
		JavaMethod javaMethod = new JavaMethod();
		javaMethod.modifiers = modifiers;
		javaMethod.returnType = returnType;
		javaMethod.name = methodName;
		javaMethod.javaClass = javaClass;
		javaMethod.parameters = parameterList;

		if (scanner.token.tokenType == ScannerToken.TOKEN_COMMENT) {
			javaMethod.comment = scanner.token.tokenString;
			scannerAdvance(scanner);
		}
		stompLevel = 1;/*hack*/
		javaMethod.methodBody = readMethodUnit(scanner);
		methodTransformer.transform(javaMethod);
//		javaClass.includeAnyReferencedTypes(javaMethod.methodBody);
		lookupType(javaMethod.returnType);

			SmalltalkSource smalltalkSource = new SmalltalkSource();
			smalltalkSource.context = methodDetails.context;
			smalltalkSource.text = smalltalkMethod;
			javaMethod.smalltalkSource = smalltalkSource;
		
		return javaMethod;
	}
	
	public void parseClassDefinition() throws Exception {
		
				String classDefinition = ((ChunkDetails) javaClass.classQuotes.firstElement()).contents;
				ChunkParser parser = new ChunkParser(classDefinition);
				for (int i = 0; i < 5; i++) {
					parser.nextWord();
				}
				parseVariables(parser, "");
				parser.nextWord();
				parseVariables(parser, "static ");

				parseMethods(javaClass.instanceMethodChunks, "");
				parseMethods(javaClass.classMethodChunks, "static ");
	}
	
	public void parse() throws Exception {
		parseClassDefinition();
//		javaClass.includeImportForType(javaClass.superclassName);
	}

	protected MethodBody readMethodUnit(SmalltalkScanner scanner) {
		Vector tokens = new Vector();
		Vector expression = new Vector();

		boolean atExpressionStart = true;
		boolean endOfUnit = false;
		boolean endOfExpression = false;
		boolean cascadeBreak = false;
		JavaCallKeywordStart existingKeyword = null;
		boolean hasIf = false;
		
		//TODO move this to a transform
		boolean hasForEach = false;
		boolean hasForPositions = false;
		boolean hasForIndices = false;

		while (!endOfUnit && scanner.token.tokenType != ScannerToken.TOKEN_END) {
			switch (scanner.token.tokenType) {
				case ScannerToken.TOKEN_TEMPS :
					{
						scannerAdvance(scanner);
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
						scannerAdvance(scanner);
						break;
					}
				case ScannerToken.TOKEN_STATEMENT_END :
					{
						expression.add(new JavaStatementTerminator());
						endOfExpression = true;
						scannerAdvance(scanner);
						break;
					}
				case ScannerToken.TOKEN_ASSIGNMENT :
					{
						expression.add(new JavaAssignment());
						scannerAdvance(scanner);
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
						} else if (binary.equals("//")) {
							//TODO what about truncation?
							binary = "/";
//						} else if (binary.equals("<<")) {
//							//TODO not good enough
//							binary = ".print()";
						}
						expression.add(new JavaKeyword(binary));
						scannerAdvance(scanner);
						atExpressionStart = true;
						break;
					}
				case ScannerToken.TOKEN_BRACKET_START :
					{
						expression.add(new JavaParenthesisStart());
						scannerAdvance(scanner);
						expression.addAll(readMethodUnit(scanner).tokens);
						scanner.token.checkType(ScannerToken.TOKEN_BRACKET_END);
						expression.add(new JavaParenthesisEnd());
						scannerAdvance(scanner);
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
						scannerAdvance(scanner);
						boolean needsForEnd = false;
						if (scanner.token.tokenType == ScannerToken.TOKEN_BLOCK_TEMP) {
							if (hasForEach) {
								needsForEnd = true;
								String tempName = getJavaSafeWord(scanner.token.tokenString);
								scannerAdvance(scanner);
								String tempType = parseType(scanner, HEAPER_CLASS);
								expression.add(new JavaType(tempType));
								expression.add(new JavaIdentifier(tempName));
								expression.add(new JavaKeyword("="));
								if (!tempType.equals(HEAPER_CLASS)) {
									expression.add(new JavaCast(tempType));
								}
								expression.add(new JavaIdentifier(FOR_EACH_STEPPER_VARIABLE+stompLevel));
								expression.add(new JavaCallStart("fetch"));
								expression.add(new JavaCallEnd());
								expression.add(new JavaStatementTerminator());
								stompLevel++;
								scanner.token.checkType(ScannerToken.TOKEN_TEMPS);
								scannerAdvance(scanner);
							} else if (hasForPositions) { 
								needsForEnd = true;
								String tempName = getJavaSafeWord(scanner.token.tokenString);
								scannerAdvance(scanner);
								String tempType = parseType(scanner, HEAPER_CLASS);
								expression.add(new JavaType(tempType));
								expression.add(new JavaIdentifier(tempName));
								expression.add(new JavaKeyword("="));
								if (!tempType.equals(HEAPER_CLASS)) {
									expression.add(new JavaCast(tempType));
								}
								expression.add(new JavaIdentifier(FOR_EACH_STEPPER_VARIABLE+stompLevel));
								expression.add(new JavaCallStart("position"));
								expression.add(new JavaCallEnd());
								expression.add(new JavaStatementTerminator());
								
								tempName = getJavaSafeWord(scanner.token.tokenString);
								scannerAdvance(scanner);
								tempType = parseType(scanner, HEAPER_CLASS);
								expression.add(new JavaType(tempType));
								expression.add(new JavaIdentifier(tempName));
								expression.add(new JavaKeyword("="));
								if (!tempType.equals(HEAPER_CLASS)) {
									expression.add(new JavaCast(tempType));
								}
								expression.add(new JavaIdentifier(FOR_EACH_STEPPER_VARIABLE+stompLevel));
								expression.add(new JavaCallStart("fetch"));
								expression.add(new JavaCallEnd());
								expression.add(new JavaStatementTerminator());
								stompLevel++;
								scanner.token.checkType(ScannerToken.TOKEN_TEMPS);
								scannerAdvance(scanner);

							} else if (hasForIndices) { 
								needsForEnd = true;
								String tempName = getJavaSafeWord(scanner.token.tokenString);
								scannerAdvance(scanner);
								String tempType = parseType(scanner, HEAPER_CLASS);
								expression.add(new JavaType(tempType));
								expression.add(new JavaIdentifier(tempName));
								expression.add(new JavaKeyword("="));
								if (!tempType.equals(HEAPER_CLASS)) {
									expression.add(new JavaCast(tempType));
								}
								expression.add(new JavaIdentifier(FOR_EACH_STEPPER_VARIABLE+stompLevel));
								expression.add(new JavaCallStart("index"));
								expression.add(new JavaCallEnd());
								expression.add(new JavaStatementTerminator());
								
								tempName = getJavaSafeWord(scanner.token.tokenString);
								scannerAdvance(scanner);
								tempType = parseType(scanner, HEAPER_CLASS);
								expression.add(new JavaType(tempType));
								expression.add(new JavaIdentifier(tempName));
								expression.add(new JavaKeyword("="));
								if (!tempType.equals(HEAPER_CLASS)) {
									expression.add(new JavaCast(tempType));
								}
								expression.add(new JavaIdentifier(FOR_EACH_STEPPER_VARIABLE+stompLevel));
								expression.add(new JavaCallStart("fetch"));
								expression.add(new JavaCallEnd());
								expression.add(new JavaStatementTerminator());
								stompLevel++;
								scanner.token.checkType(ScannerToken.TOKEN_TEMPS);
								scannerAdvance(scanner);
								
							} else {
								expression.addAll(parseTemps(scanner));
							}
						}
						expression.addAll(readMethodUnit(scanner).tokens);
						scanner.token.checkType(ScannerToken.TOKEN_BLOCK_END);
						expression.add(new JavaBlockEnd());
scannerAdvance(scanner);
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
						scannerAdvance(scanner);
						break;
					}
				case ScannerToken.TOKEN_DOUBLE :
					{
						String value = Double.toString(scanner.token.tokenDouble);
						expression.add(new JavaLiteral(value));
						atExpressionStart = false;
						scannerAdvance(scanner);
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
						word = getJavaSafeWord(word);
						word = overrideTypeIfNecessary(word);
						if (!atExpressionStart) {
							expression.add(new JavaCallStart(word));
							expression.add(new JavaCallEnd());
						} else {
							if (word.equals("UInt32Zero") || word.equals("Int32Zero") || word.equals("Int0")) {
								expression.add(new JavaLiteral("0"));
							} else if ((word.equals("IntegerVar0")) || word.equals("IntegerVarZero")) {
								//TODO IntegerVar choice!!
								expression.add(new JavaLiteral("0"));
//								expression.add(new JavaIdentifier("IntegerVar"));
//								expression.add(new JavaCallStart("zero"));
//								expression.add(new JavaCallEnd());
							} else if (word.equals("Int32Min") || word.equals("UInt32Min")) {
								//TODO use Integer.MIN_VALUE
								expression.add(new JavaLiteral("0x80000000"));
							} else if (word.equals("Int32Max") || word.equals("UInt32Max")) {
								//TODO use Integer.MAX_VALUE
								expression.add(new JavaLiteral("0x7fffffff"));
							} else if (word.equals("UInt8Max")) {
								expression.add(new JavaLiteral("0xff"));
							} else {
								expression.add(new JavaIdentifier(word));
							}
						}
						scannerAdvance(scanner);
						atExpressionStart = false;
						break;
					}
				case ScannerToken.TOKEN_KEYWORD :
					{
						String word = scanner.token.tokenString;
						String wordTrimmed = word.substring(0, word.length() - 1);
						word = getJavaSafeWord(word);
						wordTrimmed = getJavaSafeWord(wordTrimmed);

						if (wordTrimmed.equals("ifTrue") || wordTrimmed.equals("ifFalse")) {
							if (hasIf) {
								expression.add(new JavaKeyword("else"));
							} else {
								int startIndex = findStartOfExpression(expression);
								if (!(expression.lastElement() instanceof JavaParenthesisEnd) || !(expression.get(startIndex) instanceof JavaParenthesisStart)) {
									expression.add(startIndex, new JavaParenthesisStart());
									expression.add(new JavaParenthesisEnd());
								}
								expression.add(startIndex, new JavaKeyword("if"));
								if (wordTrimmed.equals("ifFalse")) {
									expression.add(startIndex + 1, new JavaParenthesisStart());
									expression.add(startIndex + 2, new JavaKeyword("!"));
									expression.add(new JavaParenthesisEnd());
								}
								hasIf = true;
							}
						} else if (wordTrimmed.equals("forEach")) {
							hasForEach = true;
							int startIndex = findStartOfExpression(expression);
							expression.add(startIndex, new JavaKeyword("for"));
							expression.add(startIndex + 1, new JavaParenthesisStart());
							expression.add(startIndex + 2, new JavaType(STEPPER_CLASS));
							expression.add(startIndex + 3, new JavaIdentifier(FOR_EACH_STEPPER_VARIABLE+stompLevel));
							expression.add(startIndex + 4, new JavaKeyword("="));
							expression.add(new JavaLoopTerminator());
							expression.add(new JavaIdentifier(FOR_EACH_STEPPER_VARIABLE+stompLevel));
							expression.add(new JavaCallStart("hasValue"));
							expression.add(new JavaCallEnd());
							expression.add(new JavaLoopTerminator());
							expression.add(new JavaIdentifier(FOR_EACH_STEPPER_VARIABLE+stompLevel));
							expression.add(new JavaCallStart("step"));
							expression.add(new JavaCallEnd());
							expression.add(new JavaParenthesisEnd());

						} else if (wordTrimmed.equals("forPositions")) {
							hasForPositions = true;
							int startIndex = findStartOfExpression(expression);
							expression.add(startIndex, new JavaKeyword("for"));
							expression.add(startIndex + 1, new JavaParenthesisStart());
							expression.add(startIndex + 2, new JavaType(TABLE_STEPPER_CLASS));
							expression.add(startIndex + 3, new JavaIdentifier(FOR_EACH_STEPPER_VARIABLE+stompLevel));
							expression.add(startIndex + 4, new JavaKeyword("="));
							expression.add(new JavaLoopTerminator());
							expression.add(new JavaIdentifier(FOR_EACH_STEPPER_VARIABLE+stompLevel));
							expression.add(new JavaCallStart("hasValue"));
							expression.add(new JavaCallEnd());
							expression.add(new JavaLoopTerminator());
							expression.add(new JavaIdentifier(FOR_EACH_STEPPER_VARIABLE+stompLevel));
							expression.add(new JavaCallStart("step"));
							expression.add(new JavaCallEnd());
							expression.add(new JavaParenthesisEnd());
							
						} else if (wordTrimmed.equals("forIndices")) {
							hasForIndices = true;
							int startIndex = findStartOfExpression(expression);
							expression.add(startIndex, new JavaKeyword("for"));
							expression.add(startIndex + 1, new JavaParenthesisStart());
							expression.add(startIndex + 2, new JavaType(TABLE_STEPPER_CLASS));
							expression.add(startIndex + 3, new JavaIdentifier(FOR_EACH_STEPPER_VARIABLE+stompLevel));
							expression.add(startIndex + 4, new JavaKeyword("="));
							expression.add(new JavaLoopTerminator());
							expression.add(new JavaIdentifier(FOR_EACH_STEPPER_VARIABLE+stompLevel));
							expression.add(new JavaCallStart("hasValue"));
							expression.add(new JavaCallEnd());
							expression.add(new JavaLoopTerminator());
							expression.add(new JavaIdentifier(FOR_EACH_STEPPER_VARIABLE+stompLevel));
							expression.add(new JavaCallStart("step"));
							expression.add(new JavaCallEnd());
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
						scannerAdvance(scanner);
						atExpressionStart = true;
						break;
					}
				case ScannerToken.TOKEN_STRING :
					{
						String safeString = stringReplaceWith(scanner.token.tokenString, "\\", "\\\\");
						safeString = stringReplaceWith(safeString, "\"", "\\\"");
						safeString = stringReplaceWith(safeString, "\n", "\\n\"+\n\"");
						safeString = "\"" + safeString + "\"";
						expression.add(new JavaLiteral(safeString));
						scannerAdvance(scanner);
						atExpressionStart = false;
						break;
					}
				case ScannerToken.TOKEN_COMMENT :
					{
						expression.add(new JavaComment(scanner.token.tokenString));
						scannerAdvance(scanner);
						break;
					}
				case ScannerToken.TOKEN_SYMBOL :
					{
						String value = scanner.token.tokenString;
						scannerAdvance(scanner);
						if (value.equals("(") && scanner.token.tokenType == ScannerToken.TOKEN_BRACKET_END) {
							//TODO special case for #()
							expression.add(new JavaIdentifier("Array"));
							expression.add(new JavaCallStart("new"));
							expression.add(new JavaCallEnd());
							scannerAdvance(scanner);
						} else {
							String symbol = transformSmalltalkSymbolToJava(value);
							expression.add(new JavaIdentifier(symbol));
						}
						atExpressionStart = false;
						break;
					}
				case ScannerToken.TOKEN_CHARACTER :
					{
						String value = "'" + scanner.token.tokenString + "'";
						expression.add(new JavaLiteral(value));
						scannerAdvance(scanner);
						atExpressionStart = false;
						break;
					}
				case ScannerToken.TOKEN_CASCADE :
					{
						cascadeBreak = true;
						scannerAdvance(scanner);
						break;
					}
				case ScannerToken.TOKEN_CHUNK :
					{
						scannerAdvance(scanner);
						endOfUnit = true;
						break;
					}
				default :
					{
						throw new IllegalStateException("Unexpected token type while writing method");
					}
			}
			if (endOfUnit || endOfExpression || cascadeBreak) {
				if (existingKeyword != null) {
					JavaToken closingKeyword = new JavaCallEnd();
					if (expression.get(expression.size() - 1) instanceof JavaStatementTerminator) {
						expression.add(expression.size() - 1, closingKeyword);
					} else {
						expression.add(closingKeyword);
					}
				}
				if (cascadeBreak) {
					expression.add(new JavaKeyword(";"));
				}
				tokens.addAll(expression);

				expression = new Vector();
				endOfExpression = false;
				if (cascadeBreak) {
					atExpressionStart = false;
				} else {
					atExpressionStart = true;
				}
				cascadeBreak = false;
				existingKeyword = null;
				hasIf = false;
				hasForEach = false;
				hasForPositions = false;
				hasForIndices = false;
			}
		}
		return new MethodBody(tokens);
	}
	
	//TODO do something about this. Sideffect of incorrect #(Blah Again) handling
	public static String transformSmalltalkSymbolToJava(String value) {
		StringBuffer buffer = new StringBuffer();
		for (int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			if (i > 0 && Character.isUpperCase(c) && Character.isLowerCase(value.charAt(i - 1))) {
				buffer.append('_');
			}
			if (c == ':') {
				buffer.append("_");
			} else {
				buffer.append(Character.toUpperCase(c));
			}
		}
		String symbol = buffer.toString();
		return symbol;
	}

	private void scannerAdvance(SmalltalkScanner scanner) {
		scanner.advance();
	}

	protected void parseVariables(ChunkParser parser, String modifiers) throws Exception {
		if (!parser.nextWord().equals("'")) {
			throw new Exception("Expected variables");
		}
		String w = parser.nextWord();
		while (!w.equals("'")) {
			String varName = getJavaSafeWord(w);
			String type = nextType(parser);
			JavaField field = new JavaField(modifiers, type, varName);
			javaClass.fields.add(field);
			w = parser.nextWord();
		}
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
	
	public static String transformCategory(String smalltalkCategory) {
		StringBuffer buffer = new StringBuffer();
		for (StringTokenizer tokenizer = new StringTokenizer(smalltalkCategory, ClassParser.CATEGORY_SEPARATOR); tokenizer.hasMoreTokens();) {
			if (buffer.length() > 0) {
				buffer.append(JavaClass.PACKAGE_SEPARATOR);
			}
			String element = tokenizer.nextToken().toLowerCase();
			element = getJavaSafeWord(element);
			buffer.append(element);
		}
		String category = buffer.toString();
		if (category.startsWith("xanadu" + JavaClass.PACKAGE_SEPARATOR)) {
			category = category.substring(("xanadu" + JavaClass.PACKAGE_SEPARATOR).length());
		}
		return "org" + JavaClass.PACKAGE_SEPARATOR + "abora" + JavaClass.PACKAGE_SEPARATOR + "gold" + JavaClass.PACKAGE_SEPARATOR + category;
	}

}
