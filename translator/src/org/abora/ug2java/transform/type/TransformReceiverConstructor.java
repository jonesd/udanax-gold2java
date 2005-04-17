package org.abora.ug2java.transform.type;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaField;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.SmalltalkSource;
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
		boolean foundRcvrConstructor = false;
		for (Iterator iter = javaClass.methods.iterator(); iter.hasNext();) {
			JavaMethod method = (JavaMethod) iter.next();
			if (method.name.equals(javaClass.className) && method.parameters.isEmpty()) {
				foundEmptyConstructor = true;
			}
			if (method.name.equals(javaClass.className) && method.parameters.size() == 1 && ((JavaField)method.parameters.get(0)).type.equals("Rcvr")) {
				foundRcvrConstructor = true;
			}
		}
		if (!foundEmptyConstructor) {
			javaClass.methods.add(createEmptyConstructor(javaClass));
		}
		if (!foundRcvrConstructor) {
			javaClass.methods.add(createReceiverConstructor(javaClass));
		}
	}

	private JavaMethod createReceiverConstructor(JavaClass javaClass) {
		JavaMethod method = new JavaMethod();
		method.javaClass = javaClass;
		method.modifiers = "";
		method.name = javaClass.className;
		method.parameters.add(new JavaField("", "Rcvr", "receiver"));
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
