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
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformCategoryName extends AbstractMethodBodyTransformation {


	public TransformCategoryName() {
		super();
	}
	public TransformCategoryName(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallStart.class, "getCategory"),
				factory.token(JavaCallEnd.class),
				factory.token(JavaCallStart.class, "name"),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart category = (JavaCallStart)tokens.get(i);
		category.value = "getClass";
		JavaCallStart name = (JavaCallStart)tokens.get(i+2);
		name.value = "getName";
		
		return i;
	}
}
