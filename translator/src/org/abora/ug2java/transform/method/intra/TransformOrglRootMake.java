/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformOrglRootMake extends AbstractMethodBodyTransformation {

	public TransformOrglRootMake() {
		super();
	}
	public TransformOrglRootMake(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		// return makeScruTable(((ScruTable) it));
		return factory.seq(
				factory.token(JavaKeyword.class, "return"), 
				factory.token(JavaCallKeywordStart.class, "makeScruTable"),
				factory.token(JavaParenthesisStart.class),
				factory.token(JavaCast.class, "ScruTable"), 
				factory.token(JavaIdentifier.class, "it"),
				factory.token(JavaParenthesisEnd.class),
				factory.token(JavaCallEnd.class));
	}
	
	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (! (javaMethod.name.equals("make") || javaMethod.javaClass.className.equals("OrglRoot"))) {
			return i;
		}
		javaMethod.methodBody.remove(i, i+6+1);
		tokens.add(i, new JavaKeyword("throw"));
		tokens.add(i+1, new JavaKeyword("new"));
		tokens.add(i+2, new JavaCallKeywordStart("UnsupportedOperationException"));
		tokens.add(i+3, new JavaCallEnd());
		return i;
	}
}
