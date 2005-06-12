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
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.javatoken.JavaStatementTerminator;



public class AddDefaultParameter implements ClassTransformer {

	public void transform(JavaClass javaClass) {
		if (javaClass.className.equals("SHTO")) {
			addStatic(javaClass, "SHTO", "make", new String[] {"String", "aString"}, "0");
		} else if (javaClass.className.equals("BeEdition")) {
				addInstance(javaClass, "Stepper", "retrieve", new String[] {"XnRegion", "region", "OrderSpec", "order"}, "0");
				addInstance(javaClass, "Stepper", "retrieve", new String[] {"XnRegion", "region"}, "null");
				addInstance(javaClass, "Stepper", "retrieve", new String[] {}, "null");
		} else if (javaClass.className.equals("Encrypter")) {
			addStatic(javaClass, "Encrypter", "make", new String[] {"Sequence", "identifier", "UInt8Array", "publicKey"}, "null");
		} else if (javaClass.className.equals("DiskManager")) {
			addInstance(javaClass, "void", "purgeClean", new String[] {}, "false");
		}
	}
	
	public void addStatic(JavaClass javaClass, String returnType, String name, String[] params, String additionalParam) {
		addStatic(javaClass, returnType, name, params, additionalParam, name);
	}
	
	public void addStatic(JavaClass javaClass, String returnType, String name, String[] params, String additionalParam, String call) {
		addMethod(javaClass, "static ", returnType, name, params, additionalParam, call);
	}

	public void addInstance(JavaClass javaClass, String returnType, String name, String[] params, String additionalParam) {
		addInstance(javaClass, returnType, name, params, additionalParam, name);
	}
	
	public void addInstance(JavaClass javaClass, String returnType, String name, String[] params, String additionalParam, String call) {
		addMethod(javaClass, "", returnType, name, params, additionalParam, call);
	}

	public void addMethod(JavaClass javaClass, String modifiers, String returnType, String name, String[] params, String additionalParam, String call) {
		JavaMethod method = new JavaMethod(returnType, name);
		List tokens = new ArrayList();
		if (!returnType.equals("void")) {
			tokens.add(new JavaKeyword("return"));
		}
		tokens.add(new JavaCallKeywordStart(call));
		for (int i = 0; i < params.length; i += 2) {
			String paramType = params[i];
			String paramName = params[i+1];
			method.addParameter(new JavaField(paramType, paramName));
			tokens.add(new JavaIdentifier(paramName));
			tokens.add(new JavaCallArgumentSeparator());
		}
		tokens.add(new JavaLiteral(additionalParam));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		method.modifiers = modifiers;
		method.methodBody = new MethodBody(tokens);
		//TODO add a generated source
		method.smalltalkSource = new SmalltalkSource();
		method.smalltalkSource.context = "";
		method.smalltalkSource.text = "Generated during transformation: AddDefaultParameter";
		javaClass.addMethod(method);
	}
}
