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
import org.abora.ug2java.transform.tokenmatcher.MatchAny;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformDOTCalls extends AbstractMethodBodyTransformation {

	public TransformDOTCalls() {
		super();
	}
	public TransformDOTCalls(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		MatchAny matchAny = new MatchAny();
		matchAny.add(factory.token(JavaCallStart.class, "DOTasLong"));
		matchAny.add(factory.token(JavaCallStart.class, "DOTasInt"));
		matchAny.add(factory.token(JavaCallStart.class, "DOTasInt32"));
		matchAny.add(factory.token(JavaCallStart.class, "DOTasUInt32"));
		return factory.seq(matchAny, factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		tokens.remove(i + 1);
		tokens.remove(i);
		return i;
	}
}
