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
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformStaticCall extends AbstractMethodBodyTransformation {

	public TransformStaticCall() {
		super();
	}
	public TransformStaticCall(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallStart.class, "classx"), 
				factory.token(JavaCallEnd.class),
				factory.token(JavaCallStart.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		tokens.add(i, new JavaIdentifier(javaMethod.javaClass.className));
		tokens.remove(i + 1);
		tokens.remove(i + 1);
		
		return i;
	}
}
