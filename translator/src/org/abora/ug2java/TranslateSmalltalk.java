/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */

package org.abora.ug2java;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.abora.ug2java.stscanner.ChunkDetails;
import org.abora.ug2java.stscanner.ChunkParser;
import org.abora.ug2java.transform.type.ClassTransformer;
import org.abora.ug2java.transform.type.ClassTransformers;
import org.abora.ug2java.writer.ClassWriter;

public class TranslateSmalltalk {
	private static final char CHUNK_SEPARATOR = '!';

	private static final Set SKIP_METHOD_CATEGORIES;
	static {
		Set set = new HashSet();
		set.add("Heaper class methodsFor: 'stubble PROXY'!");
		set.add("Heaper class methodsFor: 'locking'!");
		SKIP_METHOD_CATEGORIES = Collections.unmodifiableSet(set);
	}
	
	private static final Set SKIP_CLASSES;
	static {
		Set set = new HashSet();
		set.add("ExtractMethodConstant");
		SKIP_CLASSES = Collections.unmodifiableSet(set);
	}

	public TranslateSmalltalk() {
		super();
	}
	/**
	 * Populate locations of classes which are not included in the supplied
	 * source files. These files should be hand-created.
	 */
	protected void initializePackageLookup(Hashtable packageLookup) {
		packageLookup.put("IEEE32Array", "org.abora.gold.collection.basic");
		packageLookup.put("IEEE64Array", "org.abora.gold.collection.basic");
		packageLookup.put("Int32Array", "org.abora.gold.collection.basic");
		packageLookup.put("IntegerVarArray", "org.abora.gold.collection.basic");
		packageLookup.put("PrimArray", "org.abora.gold.collection.basic");
		packageLookup.put("PrimDataArray", "org.abora.gold.collection.basic");
		packageLookup.put("PrimFloatArray", "org.abora.gold.collection.basic");
		packageLookup.put("PrimIntArray", "org.abora.gold.collection.basic");
		packageLookup.put("PrimIntegerArray", "org.abora.gold.collection.basic");
		packageLookup.put("PtrArray", "org.abora.gold.collection.basic");
		packageLookup.put("SharedPtrArray", "org.abora.gold.collection.basic");
		packageLookup.put("UInt8Array", "org.abora.gold.collection.basic");
		packageLookup.put("UInt32Array", "org.abora.gold.collection.basic");
		packageLookup.put("WeakPtrArray", "org.abora.gold.collection.basic");

		packageLookup.put("ActualCalc", "org.abora.gold.java.missing");
		packageLookup.put("BooleanVar", "org.abora.gold.java.missing");
		packageLookup.put("BertPropJoint", "org.abora.gold.java.missing");
		packageLookup.put("Calc", "org.abora.gold.java.missing");
		//	packageLookup.put("Category", "org.abora.gold.java.missing");
		packageLookup.put("CxxClassDescription", "org.abora.gold.java.missing");
		packageLookup.put("Developer", "org.abora.gold.java.missing");
		//	packageLookup.put("Emulsion", "org.abora.gold.java.missing");	
		packageLookup.put("EncrypterConstructor", "org.abora.gold.java.missing");
		packageLookup.put("FeCompletionDetector", "org.abora.gold.java.missing");
		packageLookup.put("FeDirectWrapperChecker", "org.abora.gold.java.missing");
		packageLookup.put("FeDirectWrapperMaker", "org.abora.gold.java.missing");
		packageLookup.put("FeFillInDetector", "org.abora.gold.java.missing");
		packageLookup.put("FeIndirectWrapperChecker", "org.abora.gold.java.missing");
		packageLookup.put("FeIndirectWrapperMaker", "org.abora.gold.java.missing");
		packageLookup.put("FeWrapperSpecHolder", "org.abora.gold.java.missing");
		packageLookup.put("FHash", "org.abora.gold.java.missing");
		packageLookup.put("GrandHashSetStepper", "org.abora.gold.java.missing");
		//	packageLookup.put("Heaper", "org.abora.gold.java.missing");
		packageLookup.put("Heaplet", "org.abora.gold.java.missing");
		packageLookup.put("HRoot", "org.abora.gold.java.missing");
		//BAD	packageLookup.put("IDRegio", "org.abora.gold.java.missing");
		packageLookup.put("IEEE128", "org.abora.gold.java.missing");
		packageLookup.put("IEEE8", "org.abora.gold.java.missing");
		packageLookup.put("Initializer", "org.abora.gold.java.missing");
		packageLookup.put("IntegerVar", "org.abora.gold.java.missing");
		packageLookup.put("IObject", "org.abora.gold.java.missing");
		packageLookup.put("MuWordArray", "org.abora.gold.java.missing");
		packageLookup.put("Logger", "org.abora.gold.java.missing");
		packageLookup.put("OccludingCategoryTable", "org.abora.gold.java.missing");
		packageLookup.put("OrderEnum", "org.abora.gold.java.missing");
		packageLookup.put("PackOBits", "org.abora.gold.java.missing");
		packageLookup.put("PackageCategory", "org.abora.gold.java.missing");
		packageLookup.put("Problem", "org.abora.gold.java.missing");
		packageLookup.put("PropJoint", "org.abora.gold.java.missing");
		packageLookup.put("RandomStepper", "org.abora.gold.java.missing");
		packageLookup.put("Sema4", "org.abora.gold.java.missing");
		packageLookup.put("SensorPropJoint", "org.abora.gold.java.missing");
		packageLookup.put("SequenceDsp", "org.abora.gold.java.missing");
		packageLookup.put("ShepFlag", "org.abora.gold.java.missing");
		packageLookup.put("ShepherdStub", "org.abora.gold.java.missing");
		packageLookup.put("Signal", "org.abora.gold.java.missing");
		packageLookup.put("SnarfID", "org.abora.gold.java.missing");
		packageLookup.put("SnarfHandle", "org.abora.gold.java.missing");
		packageLookup.put("SocketPortal", "org.abora.gold.java.missing");
		packageLookup.put("SplayEntLoaf", "org.abora.gold.java.missing");
		packageLookup.put("Stamp", "org.abora.gold.java.missing");
		packageLookup.put("TransclusionRecorder", "org.abora.gold.java.missing");
		packageLookup.put("TwoStepper", "org.abora.gold.java.missing");
		packageLookup.put("UnixSocketAccessor", "org.abora.gold.java.missing");
		packageLookup.put("Urdi", "org.abora.gold.java.missing");
		packageLookup.put("UrdiView", "org.abora.gold.java.missing");
		packageLookup.put("VoidStar", "org.abora.gold.java.missing");
		//	packageLookup.put("XnExecutor", "org.abora.gold.java.missing");
		//	packageLookup.put("XuRegion", "org.abora.gold.java.missing");
		packageLookup.put("XnReadFile", "org.abora.gold.java.missing");
		packageLookup.put("XnSensor", "org.abora.gold.java.missing");
		packageLookup.put("XnWriteFile", "org.abora.gold.java.missing");
		packageLookup.put("XuRegion", "org.abora.gold.java.missing");

		// Standin for Smalltalk Class that doesn't have a name collision with Java Class
		packageLookup.put("AboraClass", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Array", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Association", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Behavior", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("BlockClosure", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Collection", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("CompiledMethod", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Context", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Delay", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Dictionary", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("EntView", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Filename", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("HashTableInspector", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("IdentityDictionary", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("IdentitySet", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("InspectorView", "org.abora.gold.java.missing.smalltalk");
		//TODO not really a smalltalk class - just an int version of Array
		packageLookup.put("IntArray", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("IntegerTableInspector", "org.abora.gold.java.missing.smalltalk");		
		packageLookup.put("MethodDictionary", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("OrderedCollection", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("ParseNode", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Processor", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Random", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("RootHandle", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Sensor", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Selector", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Set", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Smalltalk", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Stream", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("Symbol", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("TypeDescription", "org.abora.gold.java.missing.smalltalk");
		packageLookup.put("WriteStream", "org.abora.gold.java.missing.smalltalk");
		
		packageLookup.put("AboraBlockSupport", "org.abora.gold.java");
		packageLookup.put("AboraCharacterSupport", "org.abora.gold.java");
		packageLookup.put("AboraHeaper", "org.abora.gold.java");
		packageLookup.put("AboraSocketSupport", "org.abora.gold.java");
		packageLookup.put("AboraSupport", "org.abora.gold.java");
		packageLookup.put("Abstract", "org.abora.gold.java");
		packageLookup.put("Fn", "org.abora.gold.java");
		packageLookup.put("HashHelper", "org.abora.gold.java");

		packageLookup.put("AboraAssertionException", "org.abora.gold.java.exception");
		packageLookup.put("AboraRuntimeException", "org.abora.gold.java.exception");
		packageLookup.put("PasseException", "org.abora.gold.java.exception");
		packageLookup.put("ShouldImplementException", "org.abora.gold.java.exception");
		packageLookup.put("SubclassResponsibilityException", "org.abora.gold.java.exception");
		packageLookup.put("UnimplementedException", "org.abora.gold.java.exception");

		packageLookup.put("PrintWriter", "java.io");
		packageLookup.put("StringWriter", "java.io");
		packageLookup.put("Iterator", "java.util");

		packageLookup.put("HFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("HHBFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("HHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("HHHBFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("HHHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("HHHHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("HHHHHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("HHHHHHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("HHHHHHHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("BHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("BHHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("BHHHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("HIHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("VHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("VHHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("VHHHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("VHHHHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("VHHHHHFn", "org.abora.gold.java.missing.handle");
		packageLookup.put("VHBFn", "org.abora.gold.java.missing.handle");
	}
	/**
	 * Starts the application.
	 * @param args an array of command-line arguments
	 */
	public static void main(java.lang.String[] args) throws Exception {
		if (args.length < 2) {
			System.err.println("Usage: java TranslateSmalltalk javaDirectory smalltalkSource+ ");
			System.exit(1);
		}

		String outputDirectory = args[0];
		String[] sources = new String[args.length - 1];
		System.arraycopy(args, 1, sources, 0, sources.length);

		System.out.println("Translating Udanax-Gold source  --> " + outputDirectory);

		TranslateSmalltalk translateSmalltalk = new TranslateSmalltalk();
		translateSmalltalk.translate(sources, outputDirectory);

		System.out.println("Finished!!!");
	}
	private String readChunk(LineNumberReader reader) throws IOException {
		boolean inComment = false;
		boolean inString = false;
		boolean isChar = false;

		StringBuffer buffer = new StringBuffer();
		while (true) {
			int i = reader.read();
			if (i == -1) {
				if (buffer.length() == 0) {
					return null;
				} else {
					break;
				}
			}
			char c = (char) i;
			buffer.append(c);
			if (c == '"' && !inString && !isChar) {
				inComment = !inComment;
			} else if (c == '\'' && !inComment && !isChar) {
				inString = !inString;
			} else if (c == CHUNK_SEPARATOR && !(inComment || inString || isChar)) {
				break;
			}
			isChar = c == '$';
		}
		return buffer.toString().trim();
	}
	public void translate(String[] sources, String outputDirectoryName) throws Exception {

		JavaCodebase javaCodebase = new JavaCodebase();
		initializePackageLookup(javaCodebase.packageLookup);
		initializeNonTranslatedClasses(javaCodebase);

		List classToWrite = readAllSourcesFiles(sources, javaCodebase);
		writeClasses(outputDirectoryName, classToWrite);
	}

	private void initializeNonTranslatedClasses(JavaCodebase javaCodebase) {
		new JavaClass("Object", javaCodebase);
		//TODO should read all of this info in by parsing our own java classes...
		new JavaClass("AboraHeaper", "Object", javaCodebase);
		JavaClass primArray = new JavaClass("PrimArray", "Heaper", javaCodebase);
		JavaMethod m = new JavaMethod("PrimArray", "copyGrow");
		m.addParameter(new JavaField("int", "i"));
		primArray.addMethod(m);
		JavaClass ptrArray = new JavaClass("PtrArray", "PrimArray", javaCodebase);
		m = new JavaMethod("Heaper", "at");
		m.addParameter(new JavaField("int", "i"));
		ptrArray.addMethod(m);
		JavaClass weakPtrArray = new JavaClass("WeakPtrArray", "PtrArray", javaCodebase);
		m = new JavaMethod("PtrArray", "make");
		m.addParameter(new JavaField("XnExecutor", "executor"));
		m.addParameter(new JavaField("int", "i"));
		m.modifiers = "static ";
		weakPtrArray.addMethod(m);
		JavaClass character = new JavaClass("Character", "Object", javaCodebase);
		m = new JavaMethod("boolean", "isDigit");
		m.addParameter(new JavaField("char", "c"));
		m.modifiers = "static ";
		character.addMethod(m);
	}
	
	private List readAllSourcesFiles(String[] sources, JavaCodebase javaCodebase) throws FileNotFoundException, IOException, Exception {

		System.out.println();
		System.out.println("Reading Source Files");
		System.out.println("-------------------------------------------------------");

		List classesToWrite = new ArrayList();

		for (int i = 0; i < sources.length; i++) {
			String filename = sources[i];
			System.out.println("Source: " + filename);
			readSourceFile(filename, javaCodebase, classesToWrite);
		}

		return classesToWrite;
	}

	private void readSourceFile(String source, JavaCodebase javaCodebase, List classesToWrite) throws FileNotFoundException, IOException, Exception {

		File smalltalkFile = new File(source);

		FileReader fileReader = new FileReader(smalltalkFile);
		LineNumberReader reader = new LineNumberReader(fileReader);
		try {
			JavaClass javaClass = null;
			boolean methodsFor = false;
			boolean methodsForClass = false;
			String methodsForDescription = "";
			String methodCategory = null;
			boolean skipMethodCategory = false;
			while (true) {
				int chunkLineNumber = reader.getLineNumber();
				String chunk = readChunk(reader);
				if (chunk == null) {
					break;
				}
				if (chunk.startsWith("!")) {
					if (methodsFor) {
						if (!chunk.equals("!")) {
							System.out.println("Error: parsing end of embedded chunk");
						}
						methodsFor = false;
						methodsForClass = false;
					} else {
						methodsFor = true;
					}
					continue;
				}
				int subclassIndex = chunk.indexOf("subclass: #");
				int commentIndex = chunk.indexOf("comment:");
				int methodsForIndex = chunk.indexOf("methodsFor:");
				int instanceVariableNamesIndex = chunk.indexOf("instanceVariableNames:");

				if (subclassIndex != -1) {

					ChunkParser parser = new ChunkParser(chunk);
					String superClassName = parser.nextWord();
					parser.nextWord();
					parser.nextWord();
					String className = parser.nextWord();
					if (className.indexOf(":") != -1) {
						throw new Exception("Corrupt classname: " + javaClass.className + " line:" + chunkLineNumber);
					}
					javaClass = new JavaClass(className, javaCodebase);
					classesToWrite.add(javaClass);
					if (superClassName.equals("Object") /*&& classWriter.className.equals("Heaper")*/
						) {
						//TODO make up mind about superclass, or list overrides in a data structure
						if (className.equals("DeletedHeaper")) {
							superClassName = "Heaper";
						} else {
							superClassName = "AboraHeaper";
						}
					}
					javaClass.superclassName = superClassName;
					parser.moveToWord("category:");
					parser.nextWord();
					javaClass.classCategory = ClassParser.transformCategory(parser.nextWord());
					ChunkDetails chunkDetails = new ChunkDetails(smalltalkFile.getName(), chunkLineNumber, "", chunk);
					javaClass.classQuotes.add(chunkDetails);
					javaCodebase.packageLookup.put(javaClass.className, javaClass.getPackage());
				} else if (methodsForIndex != -1) {
					skipMethodCategory = false;
					methodsForClass = chunk.indexOf(" class ") != -1;
					methodsForDescription = chunk;
					//TODO would like to remove this, but it uncovers a parsing problem
					if (SKIP_METHOD_CATEGORIES.contains(chunk)) {
						skipMethodCategory = true;
					}
				} else if (instanceVariableNamesIndex != -1) {
					ChunkDetails chunkDetails = new ChunkDetails(smalltalkFile.getName(), chunkLineNumber, "", chunk);
					javaClass.classQuotes.add(chunkDetails);
				} else if (chunk.startsWith("\"--") && chunk.endsWith("\"!")) {
					//ignore
				} else if (chunk.startsWith("(") || chunk.startsWith("CxxSystemOrganization")) {
					if (javaClass != null) {
						ChunkDetails chunkDetails = new ChunkDetails(smalltalkFile.getName(), chunkLineNumber, "", chunk);
						javaClass.classQuotes.add(chunkDetails);
					}
					// skip
				} else if (commentIndex != -1) {
					int first = chunk.indexOf("'");
					int last = chunk.lastIndexOf("'");
					if (first == -1 || last == -1) {
						throw new Exception("Couldn't find class comment: " + chunkLineNumber);
					}
					javaClass.comment = chunk.substring(first + 1, last);
					ChunkDetails chunkDetails = new ChunkDetails(smalltalkFile.getName(), chunkLineNumber, "", chunk);
					javaClass.classQuotes.add(chunkDetails);
				} else {
					if (javaClass != null) {
						ChunkDetails chunkDetails = new ChunkDetails(smalltalkFile.getName(), chunkLineNumber, methodsForDescription, chunk);
						if (skipMethodCategory) {
							javaClass.classQuotes.add(chunkDetails);
						} else {
							if (methodsForClass) {
								javaClass.classMethodChunks.add(chunkDetails);
							} else {
								javaClass.instanceMethodChunks.add(chunkDetails);
							}
						}
					}
				}

			}

		} finally {
			reader.close();
		}
	}

	private void writeClasses(String outputDirectoryName, List javaClasses) throws Exception {
	
		parseClasses(javaClasses);
		transformClasses(javaClasses);
		generateImports(javaClasses);
		
		writeJavaClasses(outputDirectoryName, javaClasses);
	}
	private void logSectionHeader(String title) {
		System.out.println();
		System.out.println(title);
		System.out.println("-------------------------------------------------------");
	}
	private void writeJavaClasses(String outputDirectoryName, List javaClasses) throws Exception {
		logSectionHeader("Writing Classes");
		for (Iterator iter = javaClasses.iterator(); iter.hasNext();) {
			JavaClass javaClass = (JavaClass) iter.next();
			//TODO consider skipping classes while being read in?
			if (!SKIP_CLASSES.contains(javaClass.className)) {
				ClassWriter classWriter = new ClassWriter(javaClass);
				classWriter.write(outputDirectoryName);
			}
		}
	}
	private void generateImports(List javaClasses) {
		for (Iterator iter = javaClasses.iterator(); iter.hasNext();) {
			JavaClass javaClass = (JavaClass) iter.next();
			javaClass.generateImports();
		}
	}
	private void transformClasses(List javaClasses) {
		logSectionHeader("Transforming Classes");

		ClassTransformer classTransformer = new ClassTransformers();
		for (Iterator iter = javaClasses.iterator(); iter.hasNext();) {
			JavaClass javaClass = (JavaClass) iter.next();
			System.out.println("Transform: "+javaClass.className);
			classTransformer.transform(javaClass);
		}
	}
	private void parseClasses(List javaClasses) throws Exception {
		logSectionHeader("Parsing Java");

		ClassParser classParser = new ClassParser();

		for (Iterator iter = javaClasses.iterator(); iter.hasNext();) {
			JavaClass javaClass = (JavaClass) iter.next();
			classParser.setJavaClass(javaClass);
			System.out.println("Parse: "+javaClass.className);
			classParser.parse();
		}
	}

}
