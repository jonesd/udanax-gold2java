package org.abora.ug2java.transform.type;

import java.util.ArrayList;
import java.util.List;

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaField;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.SmalltalkSource;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
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
		} else if (javaClass.className.equals("RequestHandler")) {
			addRequestHandlerInstVarAt(javaClass);
		} else if (javaClass.className.equals("Recipe")) {
			addRecipeDefineGlobal(javaClass);
		} else if (javaClass.className.equals("DiskManager")) {
			addDiskManagerDestroyAbandoned(javaClass);			
		} else if (javaClass.className.equals("Abraham")) {
			addAbrahamGetCategoryFromStub(javaClass);			
		}
	}
	
	public void addIntegerTableMakeInt(JavaClass javaClass) {
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
		method.smalltalkSource.text = "Generated during transformation: "+this;
		javaClass.addMethod(method);
	}

	public void addImmuSetMakeMuSet(JavaClass javaClass) {
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
		method.smalltalkSource.text = "Generated during transformation: "+this;
		javaClass.addMethod(method);
	}

	public void addMuSetMakeIntegerVar(JavaClass javaClass) {
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
		method.smalltalkSource.text = "Generated during transformation: "+this;
		javaClass.addMethod(method);
	}

	public void addRequestHandlerInstVarAt(JavaClass javaClass) {
		JavaMethod method = new JavaMethod("Fn", "instVarAt");
		method.addParameter(new JavaField("int", "i"));
		List tokens = new ArrayList();
		tokens.add(new JavaKeyword("throw"));
		tokens.add(new JavaKeyword("new"));
		tokens.add(new JavaCallStart("UnsupportedOperationException"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = "";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: "+this;
		javaClass.addMethod(method);
	}

	public void addRecipeDefineGlobal(JavaClass javaClass) {
		JavaMethod method = new JavaMethod("void", "defineGlobal");
		method.addParameter(new JavaField("String", "s"));
		method.addParameter(new JavaField("Heaper", "h"));
		List tokens = new ArrayList();
		tokens.add(new JavaKeyword("throw"));
		tokens.add(new JavaKeyword("new"));
		tokens.add(new JavaCallStart("UnsupportedOperationException"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = "static ";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: "+this;
		javaClass.addMethod(method);
	}

	public void addDiskManagerDestroyAbandoned(JavaClass javaClass) {
		JavaMethod method = new JavaMethod("void", "destroyAbandoned");
		List tokens = new ArrayList();
		tokens.add(new JavaKeyword("throw"));
		tokens.add(new JavaKeyword("new"));
		tokens.add(new JavaCallStart("UnsupportedOperationException"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = "";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: "+this;
		javaClass.addMethod(method);
	}

	public void addAbrahamGetCategoryFromStub(JavaClass javaClass) {
		JavaMethod method = new JavaMethod("Category", "getCategoryFromStub");
		List tokens = new ArrayList();
		tokens.add(new JavaKeyword("throw"));
		tokens.add(new JavaKeyword("new"));
		tokens.add(new JavaCallStart("UnsupportedOperationException"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = "";
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: "+this;
		javaClass.addMethod(method);
	}
}
