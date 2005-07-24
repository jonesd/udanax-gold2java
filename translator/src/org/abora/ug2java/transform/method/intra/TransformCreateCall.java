/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.ClassParser;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformCreateCall extends AbstractMethodBodyTransformation {

	public TransformCreateCall() {
		super();
	}
	public TransformCreateCall(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallStart.class, "create.*");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart)tokens.get(i);
		if (ClassParser.NON_CONSTRUCTORS.contains(call.value)) {
			return i;
		}
		if (i > 0 && (tokens.get(i - 1) instanceof JavaIdentifier)) {
			JavaToken token = (JavaToken) tokens.get(i - 1);
			if (token.value.equals("super")) {
				return i;
			} else if (token.value.equals(javaMethod.javaClass.className) && javaMethod.isConstructor()) {
				call.value = "this";
				tokens.remove(i-1);
				return i-1;
			}
			call.value = token.value;
			tokens.remove(i - 1);
			tokens.add(i - 1, new JavaKeyword("new"));
		} else if (javaMethod.isConstructor()) {
			call.value = "this";
		} else {
			call.value = javaMethod.javaClass.className;
			tokens.add(i, new JavaKeyword("new"));
		}
		return i;
	}
}
