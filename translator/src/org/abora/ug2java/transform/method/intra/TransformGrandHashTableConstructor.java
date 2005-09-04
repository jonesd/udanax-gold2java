/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.IntegerLiteral;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



/**
 * TODO this class exists only to a possible bug in the GrandHashtable constructer where
 * the calculated nodIndexShift could be overestimated, so leading later code to generate
 * a collection index too high. The concern is that this code could be a single fix
 * for a more endemic problem of an incorrect // implementation...
 */
public class TransformGrandHashTableConstructor extends AbstractMethodBodyTransformation {

	public TransformGrandHashTableConstructor() {
		super();
	}
	public TransformGrandHashTableConstructor(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "ExponentialHashMap"),
				factory.token(JavaCallStart.class, "hashBits"),
				factory.token(JavaCallEnd.class),
				factory.token(JavaKeyword.class, "/"),
				factory.token(JavaIdentifier.class, "numNodes"),
				factory.token(JavaStatementTerminator.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.getQualifiedName().equals("GrandHashTable.GrandHashTable")) {
			return i;
		}
		tokens.add(i+4, new JavaParenthesisStart());
		tokens.add(i+6, new JavaKeyword("-"));
		tokens.add(i+7, new IntegerLiteral(1));
		tokens.add(i+8, new JavaParenthesisEnd());
		
		return i;
	}
}
