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
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformIntegerCall extends AbstractMethodBodyTransformation {

	public TransformIntegerCall() {
		super();
	}
	public TransformIntegerCall(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.any(
						factory.token(JavaIdentifier.class),
						factory.token(JavaLiteral.class)),
				factory.token(JavaCallStart.class, "integer"), 
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaToken variable = (JavaToken)tokens.get(i);
		if (variable instanceof JavaIdentifier) {
			String type = javaMethod.findTypeOfVariable(variable.value);
			if (!"int".equals(type)) {
				return i;
			}
		}
		//TODO assume literal is a number...
		tokens.add(i, new JavaIdentifier("IntegerPos"));
		tokens.add(i + 1, new JavaCallKeywordStart("make"));
		tokens.remove(i + 3);
		
		return i;
	}
}
