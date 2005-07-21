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
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformFeWrapperSpecAboraPointerToStaticMemberCall extends AbstractMethodBodyTransformation {

	public TransformFeWrapperSpecAboraPointerToStaticMemberCall() {
		super();
	}
	public TransformFeWrapperSpecAboraPointerToStaticMemberCall(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCast.class),
				factory.token(JavaIdentifier.class, "AboraSupport"),
				factory.token(JavaCallKeywordStart.class, "pointerToStaticMember"));
	}
	

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCast cast = (JavaCast)tokens.get(i);
		
		tokens.remove(i+2);
		tokens.remove(i+1);
		tokens.remove(i);
		
		tokens.add(i, new JavaKeyword("new"));
		tokens.add(i+1, new JavaCallKeywordStart(cast.value));

		return i;
	}
}
