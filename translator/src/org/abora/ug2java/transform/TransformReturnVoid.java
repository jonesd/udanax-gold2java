/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformReturnVoid extends AbstractMethodBodyTransformation {

	public TransformReturnVoid() {
		super();
	}
	public TransformReturnVoid(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaKeyword.class, "return"), 
				factory.token(JavaIdentifier.class, "VOID"));
	}

	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		tokens.remove(i + 1);
	}
}
