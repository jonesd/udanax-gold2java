/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.IntegerLiteral;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformOperatorIntNull extends AbstractMethodBodyTransformation {

	public TransformOperatorIntNull() {
		super();
	}
	public TransformOperatorIntNull(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class), 
				factory.token(JavaKeyword.class, "==|!="),		
				factory.token(JavaIdentifier.class, "null")); 
	}
	
	protected int transform(JavaMethod javaMethod, List methodBodyTokens, int i) {
		JavaIdentifier var = (JavaIdentifier)methodBodyTokens.get(i);
		String type = javaMethod.findTypeOfVariable(var.value);
		if ("int".equals(type)) {
			methodBodyTokens.remove(i + 2);
			methodBodyTokens.add(i+2, new IntegerLiteral(0));
		}
		return i;
	}
}
