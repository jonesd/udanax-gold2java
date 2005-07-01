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
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



/**
 * Transformation due to incorrect original source code within HashSetTester.test2On 
 */
public class TransformGrandHashSetRemove extends AbstractMethodBodyTransformation {

	public TransformGrandHashSetRemove() {
		super();
	}
	public TransformGrandHashSetRemove(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaParenthesisStart.class),
				factory.token(JavaParenthesisStart.class),
				factory.token(JavaCallStart.class, "hasMember"),
				factory.token(JavaIdentifier.class, "aHeaper"),
				factory.token(JavaCallEnd.class),
				factory.token(JavaParenthesisEnd.class),
				factory.token(JavaKeyword.class, "=="),
				factory.token(JavaIdentifier.class, "null"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.getQualifiedName().equals("GrandHashSet.remove")) {
			return i;
		}
		tokens.remove(i+7);
		tokens.remove(i+6);
		tokens.add(i+1, new JavaKeyword("!"));
		return i;
	}
}
