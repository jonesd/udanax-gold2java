/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformMinMax extends AbstractMethodBodyTransformation {

	public TransformMinMax() {
		super();
	}
	public TransformMinMax(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.any(
				factory.token(JavaCallKeywordStart.class, "min"),
				factory.token(JavaCallKeywordStart.class, "max"));
	}

	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		int start = javaMethod.methodBody.findStartOfExpression(i);
		JavaCallKeywordStart call = (JavaCallKeywordStart)tokens.get(i);
		tokens.remove(i);
		tokens.add(i, new JavaCallArgumentSeparator());
		tokens.add(start, new JavaIdentifier("Math"));
		tokens.add(start+1, new JavaCallKeywordStart(call.value));
		javaMethod.javaClass.includeImportForType("Math");
	}
}
