/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformIntegerZero extends AbstractMethodBodyTransformation {

	
public TransformIntegerZero() {
		super();
	}
	public TransformIntegerZero(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaIdentifier.class, "IntegerZero|Integer0"); 
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		tokens.remove(i);
		tokens.add(i, new JavaIdentifier("IntegerPos"));
		tokens.add(i+1, new JavaCallStart("zero"));
		tokens.add(i+2, new JavaCallEnd());
		
		return i;
	}
}
