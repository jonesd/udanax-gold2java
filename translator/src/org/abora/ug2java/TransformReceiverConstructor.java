package org.abora.ug2java;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaStatementTerminator;



public class TransformReceiverConstructor implements ClassTransformer {

	public void transform(JavaClass javaClass) {
		if (javaClass.className.equals("Heaper")) {
			return;
		}
		boolean foundEmptyConstructor = false;
		for (Iterator iter = javaClass.methodBodies.iterator(); iter.hasNext();) {
			JavaMethod method = (JavaMethod) iter.next();
			if (method.name.equals(javaClass.className) && method.params.equals("")) {
				foundEmptyConstructor = true;
			}
			if (method.name.equals(javaClass.className) && method.params.startsWith("Rcvr ")) {
				return;
			}
		}
		if (!foundEmptyConstructor) {
			javaClass.methodBodies.add(createEmptyConstructor(javaClass));
		}
		javaClass.methodBodies.add(createReceiverConstructor(javaClass));
	}

	private JavaMethod createReceiverConstructor(JavaClass javaClass) {
		JavaMethod method = new JavaMethod();
		method.javaClass = javaClass;
		method.modifiers = "";
		method.name = javaClass.className;
		method.params = "Rcvr receiver";
		method.returnType = "";
		method.comment = null;
		
		SmalltalkSource source = new SmalltalkSource();
		source.context = "";
		source.text = "Generated during transformation";
		method.smalltalkSource = source;
		
		List tokens = new ArrayList();
		tokens.add(new JavaCallKeywordStart("super"));
		tokens.add(new JavaIdentifier("receiver"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());
		MethodBody body = new MethodBody(tokens);		
		method.methodBody = body;
		javaClass.includeImportForType("Rcvr");
		
		return method;
	}

	private JavaMethod createEmptyConstructor(JavaClass javaClass) {
		JavaMethod method = new JavaMethod();
		method.javaClass = javaClass;
		method.modifiers = "";
		method.name = javaClass.className;
		method.params = "";
		method.returnType = "";
		method.comment = null;
		
		SmalltalkSource source = new SmalltalkSource();
		source.context = "";
		source.text = "Generated during transformation";
		method.smalltalkSource = source;
		
		List tokens = new ArrayList();
		MethodBody body = new MethodBody(tokens);		
		method.methodBody = body;
		
		return method;
	}
}
