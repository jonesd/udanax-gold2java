/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformInitializeSystemOrganization extends AbstractMethodBodyTransformation {


	public TransformInitializeSystemOrganization() {
		super();
	}
	public TransformInitializeSystemOrganization(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallStart.class, "getOrMakeCxxClassDescription"),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.getName().equals("initializeSystemOrganization")) {
			return i;
		}
		
		tokens.add(i, new JavaIdentifier("AboraSupport"));
		tokens.add(i+1, new JavaCallKeywordStart("findAboraClass"));
		// i+2
		tokens.add(i+3, new JavaIdentifier("class"));
		tokens.add(i+4, new JavaCallEnd());
		
		return i+2;
	}
}
