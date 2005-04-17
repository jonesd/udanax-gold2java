/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformReceiverReceiveIntegerVar extends AbstractMethodBodyTransformation {

	public TransformReceiverReceiveIntegerVar() {
		super();
	}
	public TransformReceiverReceiveIntegerVar(TokenMatcherFactory factory) {
		super(factory);
	}

	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaAssignment.class),
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallStart.class, "receiveIntegerVar"),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier variable = (JavaIdentifier)tokens.get(i);
		String type = javaMethod.findTypeOfVariable(variable.value);
		if (type != null && type.equals("boolean")) {
			JavaCallStart call = (JavaCallStart)tokens.get(i+3);
			call.value="receiveBooleanVar";
		}
		
		return i;
	}
	
}
