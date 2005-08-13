/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaArrayInitializerStart;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.javatoken.StringLiteral;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformInitializeClassAttributes extends AbstractMethodBodyTransformation {

	public TransformInitializeClassAttributes() {
		super();
	}
	public TransformInitializeClassAttributes(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaParenthesisStart.class),
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallStart.class, "getOrMakeCxxClassDescription"),
				factory.token(JavaCallEnd.class),
				factory.token(JavaParenthesisEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.getName().equals("initializeClassAttributes")) {
			return i;
		}
		
		javaMethod.methodBody.remove(i, i+5/*12*/);
		
		if (((JavaToken)tokens.get(i)).value.equals("friends")) {
			int end = javaMethod.methodBody.findClosingCallEnd(i);
			javaMethod.methodBody.remove(i, end+1);
		}
		javaMethod.methodBody.remove(i, i+7);
		
		
		int j = i;
		tokens.add(j++, new JavaIdentifier("AboraSupport"));
		tokens.add(j++, new JavaCallKeywordStart("findAboraClass"));
		tokens.add(j++, new JavaIdentifier(javaMethod.javaClass.className));
		tokens.add(j++, new JavaIdentifier("class"));
		tokens.add(j++, new JavaCallEnd());
		tokens.add(j++, new JavaCallKeywordStart("setAttributes"));
		tokens.add(j++, new JavaKeyword("new"));
		tokens.add(j++, new JavaCallStart("Set"));
		tokens.add(j++, new JavaCallEnd());
		
		while (j < tokens.size()) {
			JavaToken token = (JavaToken)tokens.get(j);
			if (token instanceof JavaIdentifier) {
				tokens.remove(j);
				tokens.add(j, new StringLiteral(token.value));
			} else if (token instanceof JavaKeyword && token.value.equals(";")) {
				tokens.remove(j);
				j -= 1;
			} else if (token.value.equals("yourself")) {
				tokens.remove(j+1);
				tokens.remove(j);
				j -= 1;
			} else if (token instanceof JavaParenthesisEnd) {
				tokens.remove(j);
				j -= 1;
			} else if (token instanceof JavaArrayInitializerStart) {
				//TODO shouldn't need to do this, as other transforms should set up string arrays properly...
				tokens.add(j, new JavaKeyword("new"));
				tokens.add(j+1, new JavaIdentifier("String[]"));
				j += 2;
			}
			j += 1;
		}
		
		return i;
	}
}
