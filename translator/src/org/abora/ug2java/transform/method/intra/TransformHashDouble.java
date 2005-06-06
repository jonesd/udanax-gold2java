/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformHashDouble extends AbstractMethodBodyTransformation {

	public TransformHashDouble() {
		super();
	}
	public TransformHashDouble(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
			factory.token(JavaIdentifier.class),
			factory.token(JavaCallStart.class, "hash"),
			factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier var = (JavaIdentifier)tokens.get(i);
		String varType = javaMethod.findTypeOfVariable(var.value);
		if ("double".equals(varType) || "float".equals(varType)) {
			tokens.remove(i+1);
			tokens.add(i, new JavaIdentifier("HashHelper"));
			tokens.add(i + 1, new JavaCallKeywordStart("hashForEqual"));
		}
		return i;
	}
}
