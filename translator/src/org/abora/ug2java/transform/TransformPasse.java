/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformPasse extends AbstractMethodBodyTransformation {

	
public TransformPasse() {
		super();
	}
	public TransformPasse(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallStart.class, "passe"),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart)tokens.get(i);
		tokens.add(i, new JavaKeyword("throw"));
		tokens.add(i + 1, new JavaKeyword("new"));
		call.value = "PasseException";
		javaMethod.javaClass.includeImportForType("PasseException");
		
		return i;
	}
}
