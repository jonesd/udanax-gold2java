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
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformCast extends MethodBodyTransformation {

	public TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.any(
						factory.token(JavaCallKeywordStart.class, "cast"),
						factory.token(JavaCallKeywordStart.class, "quickCast")), 
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallEnd.class));
	}

	public void transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier type = (JavaIdentifier)tokens.get(i + 1);
		int start = javaMethod.methodBody.findStartOfExpression(i - 1);
		tokens.remove(i + 2);
		tokens.remove(i + 1);
		tokens.remove(i);
		tokens.add(start, new JavaCast(type.value));
	}
}
