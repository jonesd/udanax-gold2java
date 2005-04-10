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



public class TransformNewCreate extends AbstractMethodBodyTransformation {


	public TransformNewCreate() {
		super();
	}
	public TransformNewCreate(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallStart.class, "new"),
				factory.token(JavaCallEnd.class),
				factory.token(JavaKeyword.class, "new"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		tokens.remove(i+1);
		tokens.remove(i);
		return i;
	}
}
