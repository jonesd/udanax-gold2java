/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformSuperCreate extends AbstractMethodBodyTransformation {

	public TransformSuperCreate() {
		super();
	}
	public TransformSuperCreate(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "super"), 
				factory.token(JavaCallStart.class, "create"));
	}

	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart) tokens.get(i + 1);
		call.value = "super";
		tokens.remove(i);
		int end = javaMethod.methodBody.findClosingCallEnd(i)+/*assumed ); next*/2;
		// Also must ensure that super() is the first thing in the method.
		for (int j = i - 1; j >= 0; j--) {
			JavaToken preToken = (JavaToken)tokens.get(j);
			tokens.add(end, preToken);
			tokens.remove(j);
			end--;
		}
	}
}
