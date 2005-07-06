/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformSharedPtrArrayMake extends AbstractMethodBodyTransformation {
	

	public TransformSharedPtrArrayMake() {
		super();
	}
	public TransformSharedPtrArrayMake(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "SharedPtrArray"),
				factory.token(JavaCallStart.class, "make"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (i == 0 || !(tokens.get(i-1) instanceof JavaCast)) {
			tokens.add(i, new JavaCast("SharedPtrArray"));
		}
		
		return i;
	}
}
