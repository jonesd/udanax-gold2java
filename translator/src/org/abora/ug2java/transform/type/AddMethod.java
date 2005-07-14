package org.abora.ug2java.transform.type;

import java.util.ArrayList;
import java.util.List;

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaField;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.SmalltalkSource;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;



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
			addDefineGlobal(javaClass);
		} else if (javaClass.className.equals("DiskManager")) {
			addUnsupportedMethod(javaClass, "", "void", "destroyAbandoned", new String[] {});
		} else if (javaClass.className.equals("Abraham")) {
			addUnsupportedMethod(javaClass, "", "Category", "getCategoryFromStub", new String[] {});
		} else if (javaClass.className.equals("SnarfRecord")) {
			addUnsupportedMethod(javaClass, "", "SnarfHandler", "getReadHandler", new String[] {});
			addUnsupportedMethod(javaClass, "", "void", "releaseReadHandler", new String[] {});
		} else if (javaClass.className.equals("Heaper")) {
			addUnsupportedMethod(javaClass, "static ", "boolean", "isConstructed", new String[] {"Heaper", "h"});			
			addUnsupportedMethod(javaClass, "static ", "boolean", "isDestructed", new String[] {"Heaper", "h"});
		} else if (javaClass.className.equals("Category")) {
			addUnsupportedMethod(javaClass, "", "Category", "makeHooked", new String[] {});
			addUnsupportedMethod(javaClass, "", "Category", "fetchSuperCategory", new String[] {});
			addUnsupportedMethod(javaClass, "", "Category", "registerPackageCategory", new String[] {"Object", "packageCategory"});
			addUnsupportedMethod(javaClass, "", "AboraClass", "originalClass", new String[] {});
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
			addUnsupportedMethod(javaClass, "", "void", "shuffle", new String[] {"int", "precision", "PrimArray", "buffer", "int", "size"});
		} else if (javaClass.className.equals("Binary2Rcvr")) {
			addUnsupportedMethod(javaClass, "", "void", "getCharToken", new String[] {"char", "c"});
		} else if (javaClass.className.equals("XnReadStream")) {
			addUnsupportedMethod(javaClass, "", "boolean", "end", new String[] {});
		}
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

}
