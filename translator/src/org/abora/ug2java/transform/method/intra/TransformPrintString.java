/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformPrintString extends AbstractMethodBodyTransformation {

	
public TransformPrintString() {
		super();
	}
	public TransformPrintString(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallStart.class, "printString"),
				factory.token(JavaCallEnd.class)
				);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier var = (JavaIdentifier)tokens.get(i);
		JavaCallStart call = (JavaCallStart)tokens.get(i+1);
		
		String varName = var.value;
		String varType = javaMethod.findTypeOfVariable(varName);
		String classType = null;
		if ("double".equals(varType)) {
			classType = "Double";
		} else if ("int".equals(varType)) {
			classType = "Integer";
		}
		if (classType == null) {
			return i;
		}
		
		var.value = classType;
		call.value = "toString";
		tokens.add(i + 2, new JavaIdentifier(varName));
		return i;
	}
}
