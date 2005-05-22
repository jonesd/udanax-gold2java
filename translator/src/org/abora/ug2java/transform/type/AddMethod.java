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
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;



public class AddMethod implements ClassTransformer {

	public void transform(JavaClass javaClass) {
		if (javaClass.className.equals("IntegerTable")) {
			addIntegerTableMakeInt(javaClass);
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
}
