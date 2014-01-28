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
package info.dgjones.abora.ug2java.transform.type;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import info.dgjones.abora.ug2java.JavaClass;
import info.dgjones.abora.ug2java.JavaField;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.MethodBody;
import info.dgjones.abora.ug2java.SmalltalkSource;
import info.dgjones.abora.ug2java.javatoken.JavaBlockEnd;
import info.dgjones.abora.ug2java.javatoken.JavaBlockStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaCast;
import info.dgjones.abora.ug2java.javatoken.JavaComment;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaLiteral;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisEnd;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisStart;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.javatoken.StringLiteral;



public class AddMethod implements ClassTransformer {

	public void transform(JavaClass javaClass) {
		if (javaClass.className.equals("IntegerTable")) {
			addIntegerTableMakeInt(javaClass);
			
		} else if (javaClass.className.equals("ImmuSet")) {
			addImmuSetMakeMuSet(javaClass);
			
		} else if (javaClass.className.equals("MuSet")) {
			addMuSetMakeIntegerVar(javaClass);
			
		} else if (javaClass.className.equals("FeWorkSet")) {
			addFeWorkSet(javaClass);
			
		} else if (javaClass.className.equals("RequestHandler")) {
			addUnsupportedMethod(javaClass, "", "Fn", "instVarAt", new String[] {"int", "i"});
			
		} else if (javaClass.className.equals("Recipe") || javaClass.className.equals("ServerChunk")) {
			addDefineGlobalRecipe(javaClass);
			
		} else if (javaClass.className.equals("DiskManager")) {
			addUnsupportedMethod(javaClass, "", "void", "destroyAbandoned", new String[] {});
			
		} else if (javaClass.className.equals("Abraham")) {
			addUnsupportedMethod(javaClass, "", "Category", "getCategoryFromStub", new String[] {});
			
		} else if (javaClass.className.equals("SnarfRecord")) {
			addUnsupportedMethod(javaClass, "", "SnarfHandler", "getReadHandler", new String[] {});
			addUnsupportedMethod(javaClass, "", "void", "releaseReadHandler", new String[] {});
			
		} else if (javaClass.className.equals("Heaper")) {
			addHeaperIsDestructed(javaClass);
			addHeaperIsConstructed(javaClass);
			addHeaperEquals(javaClass);
			
		} else if (javaClass.className.equals("Category")) {
			addUnsupportedMethod(javaClass, "", "Category", "makeHooked", new String[] {});
			addUnsupportedMethod(javaClass, "", "Category", "fetchSuperCategory", new String[] {});
			addUnsupportedMethod(javaClass, "", "Category", "registerPackageCategory", new String[] {"Object", "packageCategory"});
			addUnsupportedMethod(javaClass, "", "AboraClass", "originalClass", new String[] {});
			addCategoryIsEqual(javaClass);
			addCategoryPrintOn(javaClass);
			
		} else if (javaClass.className.equals("Tester")) {
			addUnsupportedMethod(javaClass, "", "void", "perform", new String[] {"String", "test", "PrintWriter", "out"});			
			addUnsupportedMethod(javaClass, "static ", "String", "spyTest", new String[] {"String", "test"});			
			addUnsupportedMethod(javaClass, "static ", "String", "runTest", new String[] {"String", "test"});
			
		} else if (javaClass.className.equals("Package")) {
			addUnsupportedMethod(javaClass, "static ", "String", "fetchAttribute", new String[] {"String", "attributeName"});
			addUnsupportedMethod(javaClass, "static ", "boolean", "hasAttribute", new String[] {"String", "attributeName"});
			
		} else if (javaClass.className.equals("DeleteExecutor")) {
			addUnsupportedMethod(javaClass, "static ", "void", "registerHolder", new String[] {"Heaper", "holder", "String", "storage"});
			
		} else if (javaClass.className.equals("HashSetTester")) {
			addHashSetTesterIntroduceTestsOn(javaClass);
			addHashSetTesterStoreTestsOn(javaClass);
			
		} else if (javaClass.className.equals("ByteShuffler")) {
			addByteShufflerShuffle(javaClass);
			
		} else if (javaClass.className.equals("Binary2Rcvr")) {
			addUnsupportedMethod(javaClass, "", "void", "getCharToken", new String[] {"char", "c"});
			
		} else if (javaClass.className.equals("XnReadStream")) {
			addUnsupportedMethod(javaClass, "", "boolean", "end", new String[] {});
		}
	}
	
	private JavaMethod addCategoryIsEqual(JavaClass javaClass) {
		JavaMethod method = new JavaMethod("boolean", "isEqual");
		method.addParameter(new JavaField("Heaper", "other"));
		List tokens = new ArrayList();
		tokens.add(new JavaKeyword("return"));
		tokens.add(new JavaIdentifier("this"));
		tokens.add(new JavaKeyword("=="));
		tokens.add(new JavaIdentifier("other"));
		tokens.add(new JavaStatementTerminator());
		method.modifiers = "";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddMethod";
		javaClass.addMethod(method);
		return method;
	}

	private JavaMethod addByteShufflerShuffle(JavaClass javaClass) {
		JavaMethod method = new JavaMethod("void", "shuffle");
		method.addParameter(new JavaField("int", "precision"));
		method.addParameter(new JavaField("PrimArray", "buffer"));
		method.addParameter(new JavaField("int", "size"));
		List tokens = new ArrayList();
		tokens.add(new JavaCallKeywordStart("shuffle"));
		tokens.add(new JavaIdentifier("precision"));
		tokens.add(new JavaCallArgumentSeparator());
		tokens.add(new JavaCast("UInt8Array"));
		tokens.add(new JavaIdentifier("buffer"));
		tokens.add(new JavaCallArgumentSeparator());
		tokens.add(new JavaIdentifier("size"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = "";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddMethod";
		javaClass.addMethod(method);
		return method;
	}

	public JavaMethod addIntegerTableMakeInt(JavaClass javaClass) {
		JavaMethod method = new JavaMethod("IntegerTable", "make");
		method.addParameter(new JavaField("int", "i"));
		List tokens = new ArrayList();
		tokens.add(new JavaKeyword("return"));
		tokens.add(new JavaCallKeywordStart("makeIntegerVar"));
		tokens.add(new JavaIdentifier("i"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = "static ";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddMethod";
		javaClass.addMethod(method);
		return method;
	}

	public JavaMethod addImmuSetMakeMuSet(JavaClass javaClass) {
		JavaMethod method = new JavaMethod("ImmuSet", "make");
		method.addParameter(new JavaField("MuSet", "i"));
		List tokens = new ArrayList();
		tokens.add(new JavaKeyword("return"));
		tokens.add(new JavaIdentifier("ImmuSet"));
		tokens.add(new JavaCallKeywordStart("makeMuSet"));
		tokens.add(new JavaIdentifier("i"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = "static ";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddMethod";
		javaClass.addMethod(method);
		return method;
	}

	public JavaMethod addMuSetMakeIntegerVar(JavaClass javaClass) {
		JavaMethod method = new JavaMethod("MuSet", "make");
		method.addParameter(new JavaField("int", "i"));
		List tokens = new ArrayList();
		tokens.add(new JavaKeyword("return"));
		tokens.add(new JavaCallKeywordStart("makeIntegerVar"));
		tokens.add(new JavaIdentifier("i"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = "static ";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddMethod";
		javaClass.addMethod(method);
		return method;
	}

	public JavaMethod addUnsupportedMethod(JavaClass javaClass, String modifiers, String returnType, String name, String[] args) {
		JavaMethod method = new JavaMethod(returnType, name);
		for (int i = 0; i < args.length; i+=2) {
			method.addParameter(new JavaField(args[i], args[i+1]));
		}
		List tokens = new ArrayList();
		tokens.add(new JavaKeyword("throw"));
		tokens.add(new JavaKeyword("new"));
		tokens.add(new JavaCallStart("UnsupportedOperationException"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = modifiers;
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddMethod";
		javaClass.addMethod(method);
		return method;
	}
	
	public JavaMethod addFeWorkSet(JavaClass javaClass) {
		JavaMethod method = new JavaMethod("", "FeWorkSet");
		method.addParameter(new JavaField("FeEdition", "edition"));
		method.addParameter(new JavaField("FeWrapperSpec", "spec"));
		List tokens = new ArrayList();
		tokens.add(new JavaCallKeywordStart("super"));
		tokens.add(new JavaIdentifier("edition"));
		tokens.add(new JavaCallArgumentSeparator());
		tokens.add(new JavaIdentifier("spec"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = "";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddMethod";
		javaClass.addMethod(method);
		return method;
	}

	public JavaMethod addHashSetTesterIntroduceTestsOn(JavaClass javaClass) {
		return this.addHashSetTesterTestsOn(javaClass, "introduceTestsOn");
	}

	public JavaMethod addHashSetTesterStoreTestsOn(JavaClass javaClass) {
		return this.addHashSetTesterTestsOn(javaClass, "storeTestsOn");
	}

	private JavaMethod addHashSetTesterTestsOn(JavaClass javaClass, String methodName) {
		JavaMethod method = new JavaMethod("void", methodName);
		method.addParameter(new JavaField("PrintWriter", "oo"));
		method.addParameter(new JavaField("MuSet", "set"));
		method.addParameter(new JavaField("SHTO", "shto"));
		List tokens = new ArrayList();
		tokens.add(new JavaCallKeywordStart(methodName));
		tokens.add(new JavaIdentifier("oo"));
		tokens.add(new JavaCallArgumentSeparator());
		tokens.add(new JavaCast("HashSet"));
		tokens.add(new JavaIdentifier("set"));
		tokens.add(new JavaCallArgumentSeparator());
		tokens.add(new JavaIdentifier("shto"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = "";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddMethod";
		javaClass.addMethod(method);
		return method;
	}

	public JavaMethod addDefineGlobal(JavaClass javaClass) {
		JavaMethod method = new JavaMethod("void", "defineGlobal");
		method.addParameter(new JavaField("String", "globalName"));
		method.addParameter(new JavaField("Heaper", "initialValue"));
		List tokens = new ArrayList();
		tokens.add(new JavaIdentifier("AboraSupport"));
		tokens.add(new JavaCallKeywordStart("defineGlobal"));
		tokens.add(new JavaIdentifier("globalName"));
		tokens.add(new JavaCallArgumentSeparator());
		tokens.add(new JavaIdentifier("initialValue"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = "static ";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddMethod";
		javaClass.addMethod(method);
		return method;
	}

	public JavaMethod addDefineGlobalRecipe(JavaClass javaClass) {
		JavaMethod method = new JavaMethod("void", "defineGlobal");
		method.addParameter(new JavaField("String", "globalName"));
		method.addParameter(new JavaField("Heaper", "initialValue"));
		List tokens = new ArrayList();
		tokens.add(new JavaIdentifier("AboraSupport"));
		tokens.add(new JavaCallKeywordStart("defineGlobalRecipe"));
		tokens.add(new JavaIdentifier("globalName"));
		tokens.add(new JavaCallArgumentSeparator());
		tokens.add(new JavaIdentifier("initialValue"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = "static ";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddMethod";
		javaClass.addMethod(method);
		return method;
	}

	public JavaMethod addHeaperEquals(JavaClass javaClass) {
		JavaMethod method = new JavaMethod("boolean", "equals");
		method.addParameter(new JavaField("Object", "o"));
		List tokens = new ArrayList();
		tokens.add(new JavaKeyword("if"));
		tokens.add(new JavaParenthesisStart());
		tokens.add(new JavaIdentifier("o"));
		tokens.add(new JavaKeyword("instanceof"));
		tokens.add(new JavaIdentifier("Heaper"));
		tokens.add(new JavaParenthesisEnd());
		tokens.add(new JavaBlockStart());
		tokens.add(new JavaKeyword("return"));
		tokens.add(new JavaCallKeywordStart("equals"));
		tokens.add(new JavaCast("Heaper"));
		tokens.add(new JavaIdentifier("o"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		tokens.add(new JavaBlockEnd());
		tokens.add(new JavaKeyword("else"));
		tokens.add(new JavaBlockStart());
		tokens.add(new JavaKeyword("return"));
		tokens.add(new JavaIdentifier("false"));
		tokens.add(new JavaStatementTerminator());
		tokens.add(new JavaBlockEnd());
		method.modifiers = "";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddMethod";
		javaClass.addMethod(method);
		return method;
	}
	
	protected JavaMethod addCategoryPrintOn(JavaClass javaClass) {
		JavaMethod method = new JavaMethod("void", "printOn");
		method.addParameter(new JavaField("PrintWriter", "oo"));
		List tokens = new ArrayList();
		tokens.add(new JavaIdentifier("oo"));
		tokens.add(new JavaCallKeywordStart("print"));
		tokens.add(new JavaCallStart("getAboraClass"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaCallStart("name"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		
		tokens.add(new JavaIdentifier("oo"));
		tokens.add(new JavaCallKeywordStart("print"));
		tokens.add(new StringLiteral("("));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		
		tokens.add(new JavaIdentifier("oo"));
		tokens.add(new JavaCallKeywordStart("print"));
		tokens.add(new JavaCallStart("name"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		
		tokens.add(new JavaIdentifier("oo"));
		tokens.add(new JavaCallKeywordStart("print"));
		tokens.add(new StringLiteral(")"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		
		method.modifiers = "";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddMethod";
		javaClass.addMethod(method);
		return method;
	}

	protected JavaMethod addHeaperIsDestructed(JavaClass javaClass) {
//		{BooleanVar} isDestructed: obj {void star}
//		^obj == NULL or: [obj class == DeletedHeaper]!
		JavaMethod method = new JavaMethod("boolean", "isDestructed");
		method.addParameter(new JavaField("Object", "obj"));

		List tokens = new ArrayList();
		tokens.add(new JavaKeyword("return"));
		tokens.add(new JavaIdentifier("obj"));
		tokens.add(new JavaKeyword("=="));
		tokens.add(new JavaIdentifier("null"));
		tokens.add(new JavaKeyword("||"));
		tokens.add(new JavaIdentifier("obj"));
		tokens.add(new JavaKeyword("instanceof"));
		tokens.add(new JavaIdentifier("DeletedHeaper"));
		tokens.add(new JavaStatementTerminator());
		method.modifiers = "static ";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddMethod";
		javaClass.addMethod(method);
		return method;
	}

	protected JavaMethod addHeaperIsConstructed(JavaClass javaClass) {
//		{BooleanVar} isConstructed: obj {void star}
//		^obj ~~ NULL and: [obj class ~~ DeletedHeaper]!
		JavaMethod method = new JavaMethod("boolean", "isConstructed");
		method.addParameter(new JavaField("Object", "obj"));

		List tokens = new ArrayList();
		tokens.add(new JavaKeyword("return"));
		tokens.add(new JavaIdentifier("obj"));
		tokens.add(new JavaKeyword("!="));
		tokens.add(new JavaIdentifier("null"));
		tokens.add(new JavaKeyword("&&"));
		tokens.add(new JavaKeyword("!"));
		tokens.add(new JavaParenthesisStart());
		tokens.add(new JavaIdentifier("obj"));
		tokens.add(new JavaKeyword("instanceof"));
		tokens.add(new JavaIdentifier("DeletedHeaper"));
		tokens.add(new JavaParenthesisEnd());
		tokens.add(new JavaStatementTerminator());

		method.modifiers = "static ";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddMethod";
		javaClass.addMethod(method);
		return method;
	}
}
