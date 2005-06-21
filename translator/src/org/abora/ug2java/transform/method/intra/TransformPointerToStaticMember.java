/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.javatoken.StringLiteral;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformPointerToStaticMember extends AbstractMethodBodyTransformation {

	
public TransformPointerToStaticMember() {
		super();
	}
	public TransformPointerToStaticMember(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallKeywordStart.class, "pointerToStaticMember"),
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallArgumentSeparator.class),
				factory.token(JavaLiteral.class),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier member = (JavaIdentifier)tokens.get(i+2);
		JavaLiteral memberType = (JavaLiteral)tokens.get(i+4);
		//TODO
		if (!member.value.startsWith("\"")) {
			tokens.remove(i+2);
			tokens.add(i+2, new StringLiteral(member.value));
		}
		String castType = memberType.value;
		if (castType.startsWith("\"")) {
			castType = castType.substring(1, castType.length()-1);
		}
		tokens.add(i, new JavaCast(castType));
		return i;
	}
}
