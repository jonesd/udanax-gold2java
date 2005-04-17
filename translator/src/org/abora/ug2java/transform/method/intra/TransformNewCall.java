/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformNewCall extends AbstractMethodBodyTransformation {

	public TransformNewCall() {
		super();
	}
	public TransformNewCall(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallStart.class, "new"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier type = (JavaIdentifier)tokens.get(i);
		JavaCallStart call = (JavaCallStart)tokens.get(i+1);
		
		call.value = type.value;
			javaMethod.javaClass.includeImportForType(type.value);
			tokens.remove(i);
			tokens.add(i, new JavaKeyword("new"));
		return i;
	}
}
