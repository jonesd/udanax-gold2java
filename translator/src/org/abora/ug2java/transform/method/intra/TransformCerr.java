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



public class TransformCerr extends AbstractMethodBodyTransformation {

	
public TransformCerr() {
		super();
	}
	public TransformCerr(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.any(
				factory.token(JavaIdentifier.class, "cerr"),
				factory.token(JavaIdentifier.class, "Transcript"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		tokens.remove(i);
		tokens.add(i, new JavaIdentifier("AboraSupport"));
		tokens.add(i+1, new JavaIdentifier("logger"));
		return i;
	}
}
