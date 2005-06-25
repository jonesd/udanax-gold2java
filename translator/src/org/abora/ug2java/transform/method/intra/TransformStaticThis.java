/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformStaticThis extends AbstractMethodBodyTransformation {

	public TransformStaticThis() {
		super();
	}
	public TransformStaticThis(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaIdentifier.class, "this");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.isStatic()) {
			return i;
		}
		tokens.remove(i);
		tokens.add(i, new JavaIdentifier(javaMethod.javaClass.className));
		tokens.add(i+1, new JavaIdentifier("class"));
		
		return i;
	}
}
