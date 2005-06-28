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



public class TransformUInt8Vector extends AbstractMethodBodyTransformation {

	public TransformUInt8Vector() {
		super();
	}
	public TransformUInt8Vector(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq( 
				factory.token(JavaIdentifier.class, "int"),
				factory.token(JavaCallStart.class, "vector"),
				factory.token(JavaCallEnd.class)
				);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier identifier = (JavaIdentifier)tokens.get(i);
		identifier.value = "UInt8Array";
		tokens.remove(i + 2);
		tokens.remove(i + 1);
		
		return i-1;
	}
}
