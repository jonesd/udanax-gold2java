/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformCastCheck extends AbstractMethodBodyTransformation {

	public TransformCastCheck() {
		super();
	}
	public TransformCastCheck(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCast.class),
				factory.token(JavaIdentifier.class),
				factory.token(JavaStatementTerminator.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		// If this is a statement composed of just a variable cast, then it
		// is probably meant as dynamic type check. To fit into the Java syntax
		// requirement we need to dress it up as a full statement.
		JavaCast type = (JavaCast)tokens.get(i);
		JavaIdentifier var = (JavaIdentifier)tokens.get(i+1);
		
		if (i == 0 || tokens.get(i) instanceof JavaBlockEnd || tokens.get(i) instanceof JavaStatementTerminator) {
			tokens.add(i, new JavaType(type.value));
			tokens.add(i+1, new JavaIdentifier(var.value+"Cast"));
			tokens.add(i+2, new JavaAssignment());
		}
		return i+3;
	}
}
