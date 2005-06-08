/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformCharacterCharx extends AbstractMethodBodyTransformation {

	public TransformCharacterCharx() {
		super();
	}
	public TransformCharacterCharx(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "Character"),
				factory.token(JavaCallKeywordStart.class, "charx"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int callEnd = javaMethod.methodBody.findClosingCallEnd(i+1);
		
		tokens.remove(callEnd);
		tokens.add(callEnd, new JavaParenthesisEnd());
		tokens.remove(i+1);
		tokens.remove(i);
		tokens.add(i, new JavaCast("char"));
		tokens.add(i+1, new JavaParenthesisStart());
		return i;
	}
}
