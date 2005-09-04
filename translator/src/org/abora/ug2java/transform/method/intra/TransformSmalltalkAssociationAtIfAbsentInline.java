/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformSmalltalkAssociationAtIfAbsentInline extends AbstractMethodBodyTransformation {

	private static final Set ONLY_METHODS;
	static {
		Set set = new HashSet();
		set.add("Recipe.staticTimeNonInherited");

		//TODO test case only
		set.add("Test.testSmalltalkAssociationAtIfAbsentInline");

		ONLY_METHODS = Collections.unmodifiableSet(set);
	}

	public TransformSmalltalkAssociationAtIfAbsentInline() {
		super();
	}
	public TransformSmalltalkAssociationAtIfAbsentInline(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "Smalltalk"),
				factory.token(JavaCallKeywordStart.class, "associationAtIfAbsent")
				);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!(ONLY_METHODS.contains(javaMethod.getName()) || ONLY_METHODS.contains(javaMethod.getQualifiedName()))) {
			return i;
		}
		
		List argStarts = javaMethod.methodBody.extractCallArgStarts(i+1);
		int arg1Start = ((Integer)argStarts.get(1)).intValue();
		javaMethod.methodBody.shouldMatch(arg1Start, JavaBlockStart.class);
		int blockEnd = javaMethod.methodBody.findEndOfBlock(arg1Start);
		
		tokens.remove(blockEnd);
		if (tokens.get(blockEnd-1) instanceof JavaStatementTerminator) {
			tokens.remove(blockEnd - 1);
		}
		tokens.remove(arg1Start);
		
		return i;

	}
}
