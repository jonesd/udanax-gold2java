/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformCAThashForEqual extends AbstractMethodBodyTransformation {

	public TransformCAThashForEqual() {
		super();
	}
	public TransformCAThashForEqual(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(factory.token(JavaIdentifier.class, "CAT"), factory.token(JavaCallStart.class, "hashForEqual"), factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier javaIdentifier = (JavaIdentifier)tokens.get(i);
		javaIdentifier.value = "HashHelper";
		javaMethod.javaClass.includeImportForType("HashHelper");
		tokens.remove(i + 1);
		tokens.add(i + 1, new JavaCallKeywordStart("hashForEqual"));
		tokens.add(i + 2, new JavaIdentifier("this"));
		tokens.add(i + 3, new JavaCallStart("getClass"));
		tokens.add(i + 4, new JavaCallEnd());
		
		return i;
	}
}
