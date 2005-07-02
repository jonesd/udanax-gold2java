/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformName extends AbstractMethodBodyTransformation {


	public TransformName() {
		super();
	}
	public TransformName(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallStart.class, "name"),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart)tokens.get(i);
		if (i > 0) {
			if (tokens.get(i-1) instanceof JavaIdentifier) {
				JavaIdentifier var = (JavaIdentifier)tokens.get(i-1);
				if (javaMethod.getJavaCodebase().getJavaClass(var.value) != null) {
					call.value = "getName";
				}
			} else if (!(tokens.get(i-1) instanceof JavaCallEnd) && javaMethod.isStatic()) {
					call.value = "getName";
					tokens.add(i, new JavaIdentifier(javaMethod.javaClass.className));
					tokens.add(i+1, new JavaIdentifier("class"));
			}
		}
		
		return i;
	}
}
