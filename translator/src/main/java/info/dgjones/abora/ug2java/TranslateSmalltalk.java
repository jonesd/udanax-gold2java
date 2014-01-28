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
package info.dgjones.abora.ug2java;

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

import info.dgjones.abora.ug2java.stscanner.ChunkDetails;
import info.dgjones.abora.ug2java.stscanner.ChunkParser;
import info.dgjones.abora.ug2java.transform.type.ClassTransformer;
import info.dgjones.abora.ug2java.transform.type.ClassTransformers;
import info.dgjones.abora.ug2java.writer.ClassWriter;

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
		packageLookup.put("IEEE32Array", "info.dgjones.abora.gold.collection.basic");
		packageLookup.put("IEEE64Array", "info.dgjones.abora.gold.collection.basic");
		packageLookup.put("Int32Array", "info.dgjones.abora.gold.collection.basic");
		packageLookup.put("IntegerVarArray", "info.dgjones.abora.gold.collection.basic");
		packageLookup.put("PrimArray", "info.dgjones.abora.gold.collection.basic");
		packageLookup.put("PrimDataArray", "info.dgjones.abora.gold.collection.basic");
		packageLookup.put("PrimFloatArray", "info.dgjones.abora.gold.collection.basic");
		packageLookup.put("PrimIntArray", "info.dgjones.abora.gold.collection.basic");
		packageLookup.put("PrimIntegerArray", "info.dgjones.abora.gold.collection.basic");
		packageLookup.put("PtrArray", "info.dgjones.abora.gold.collection.basic");
		packageLookup.put("SharedPtrArray", "info.dgjones.abora.gold.collection.basic");
		packageLookup.put("UInt8Array", "info.dgjones.abora.gold.collection.basic");
		packageLookup.put("UInt32Array", "info.dgjones.abora.gold.collection.basic");
		packageLookup.put("WeakPtrArray", "info.dgjones.abora.gold.collection.basic");

		packageLookup.put("ActualCalc", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("ActualCopyRecipe", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("BooleanVar", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("BertPropJoint", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("Calc", "info.dgjones.abora.gold.java.missing");
		//	packageLookup.put("Category", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("CxxClassDescription", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("CxxSystemOrganization", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("CxxTreeAssociation", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("Developer", "info.dgjones.abora.gold.java.missing");
		//	packageLookup.put("Emulsion", "info.dgjones.abora.gold.java.missing");	
		packageLookup.put("EncrypterConstructor", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("FeCompletionDetector", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("FeDirectWrapperChecker", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("FeDirectWrapperMaker", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("FeFillInDetector", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("FeIndirectWrapperChecker", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("FeIndirectWrapperMaker", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("FeWrapperSpecHolder", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("FHash", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("GrandHashSetStepper", "info.dgjones.abora.gold.java.missing");
		//	packageLookup.put("Heaper", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("Heaplet", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("HRoot", "info.dgjones.abora.gold.java.missing");
		//BAD	packageLookup.put("IDRegio", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("IEEE128", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("IEEE8", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("Initializer", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("IntegerVar", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("IObject", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("MuWordArray", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("Logger", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("OccludingCategoryTable", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("OrderEnum", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("PackOBits", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("PackageCategory", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("Problem", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("PropJoint", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("PseudoCopyRecipe", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("RandomStepper", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("Sema4", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("SensorPropJoint", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("SequenceDsp", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("ShepFlag", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("ShepherdStub", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("Signal", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("SocketPortal", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("SplayEntLoaf", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("Stamp", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("TransclusionRecorder", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("TwoStepper", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("UnixSocketAccessor", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("VoidStar", "info.dgjones.abora.gold.java.missing");
		//	packageLookup.put("XnExecutor", "info.dgjones.abora.gold.java.missing");
		//	packageLookup.put("XuRegion", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("XnReadFile", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("XnSensor", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("XnWriteFile", "info.dgjones.abora.gold.java.missing");
		packageLookup.put("XuRegion", "info.dgjones.abora.gold.java.missing");

		// Standin for Smalltalk Class that doesn't have a name collision with Java Class
		packageLookup.put("AboraClass", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Array", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Association", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Behavior", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("BlockClosure", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Collection", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("CompiledMethod", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Context", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Delay", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Dictionary", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("EntView", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Filename", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("HashTableInspector", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("IdentityDictionary", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("IdentitySet", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("InspectorView", "info.dgjones.abora.gold.java.missing.smalltalk");
		//TODO not really a smalltalk class - just an int version of Array
		packageLookup.put("IntArray", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("IntegerTableInspector", "info.dgjones.abora.gold.java.missing.smalltalk");		
		packageLookup.put("MethodDictionary", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("OrderedCollection", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("ParseNode", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Processor", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Random", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("RootHandle", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Sensor", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Selector", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Set", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Smalltalk", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Stream", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("Symbol", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("TypeDescription", "info.dgjones.abora.gold.java.missing.smalltalk");
		packageLookup.put("WriteStream", "info.dgjones.abora.gold.java.missing.smalltalk");

		// Missing classes; just group together
		packageLookup.put("SnarfID", "info.dgjones.abora.gold.java.urdi");
		packageLookup.put("SnarfHandle", "info.dgjones.abora.gold.java.urdi");
		packageLookup.put("Urdi", "info.dgjones.abora.gold.java.urdi");
		packageLookup.put("UrdiView", "info.dgjones.abora.gold.java.urdi");

		packageLookup.put("AboraBlockSupport", "info.dgjones.abora.gold.java");
		packageLookup.put("AboraCharacterSupport", "info.dgjones.abora.gold.java");
		packageLookup.put("AboraHeaper", "info.dgjones.abora.gold.java");
		packageLookup.put("AboraSocketSupport", "info.dgjones.abora.gold.java");
		packageLookup.put("AboraSupport", "info.dgjones.abora.gold.java");
		packageLookup.put("Abstract", "info.dgjones.abora.gold.java");
		packageLookup.put("Fn", "info.dgjones.abora.gold.java");
		packageLookup.put("HashHelper", "info.dgjones.abora.gold.java");

		packageLookup.put("AboraAssertionException", "info.dgjones.abora.gold.java.exception");
		packageLookup.put("AboraRuntimeException", "info.dgjones.abora.gold.java.exception");
		packageLookup.put("PasseException", "info.dgjones.abora.gold.java.exception");
		packageLookup.put("ShouldImplementException", "info.dgjones.abora.gold.java.exception");
		packageLookup.put("SubclassResponsibilityException", "info.dgjones.abora.gold.java.exception");
		packageLookup.put("UnimplementedException", "info.dgjones.abora.gold.java.exception");

		packageLookup.put("PrintWriter", "java.io");
		packageLookup.put("StringWriter", "java.io");
		packageLookup.put("Iterator", "java.util");

		packageLookup.put("HFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("HHBFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("HHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("HHHBFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("HHHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("HHHHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("HHHHHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("HHHHHHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("HHHHHHHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("BHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("BHHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("BHHHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("HIHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("VHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("VHHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("VHHHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("VHHHHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("VHHHHHFn", "info.dgjones.abora.gold.java.missing.handle");
		packageLookup.put("VHBFn", "info.dgjones.abora.gold.java.missing.handle");
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
		//TODO relying on custom code...
//		JavaClass sharedPtrArray = new JavaClass("SharedPtrArray", "PtrArray", javaCodebase);
//		m = new JavaMethod("PtrArray", "make");
//		m.addParameter(new JavaField("int", "count"));
//		m.modifiers = "static ";
//		sharedPtrArray.addMethod(m);
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
					if (superClassName.equals("Object") /*&& classWriter.className.equals("Heaper")*/
						) {
						//TODO make up mind about superclass, or list overrides in a data structure
						if (className.equals("DeletedHeaper")) {
							superClassName = "Heaper";
						} else {
							superClassName = "AboraHeaper";
						}
					}
					javaClass = new JavaClass(className, superClassName, javaCodebase);
					classesToWrite.add(javaClass);
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
					javaClass.setComment(chunk.substring(first + 1, last));
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
