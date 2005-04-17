/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformSendIntegerVar extends AbstractMethodBodyTransformation {

	public TransformSendIntegerVar() {
		super();
	}
	public TransformSendIntegerVar(TokenMatcherFactory factory) {
		super(factory);
	}

	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallKeywordStart.class, "sendIntegerVar"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int callEnd = javaMethod.methodBody.findClosingCallEnd(i+1);
		JavaToken token = (JavaToken)tokens.get(i+2);
		boolean transformToBoolean = false;
		if (callEnd == i+3) {
			String varName = token.value;
			String varType = javaMethod.findTypeOfVariable(varName);
			transformToBoolean =  varType != null && varType.equals("boolean");
		} else if ("!".equals(token.value)) {
			//TODO limited boolean expression matcher
			transformToBoolean = true;
		}
		
		if (transformToBoolean) {
			JavaCallStart call = (JavaCallStart)tokens.get(i+1);
			call.value="sendBooleanVar";
		}
		
		return i;
	}
	
}
