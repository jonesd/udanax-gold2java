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
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformCompilerFodder extends AbstractMethodBodyTransformation {
	/**
	 * Methods that are too complicated for the current simple code to analyse properly.
	 * Any methods matched against this should be left as is.
	 */
	private static final Set ignoreMethods;
	static {
		Set set = new HashSet();
		set.add("ResultRecorderPFinder.shouldTrigger");
		
		//TODO for tests only
		set.add("Test.testUnreachableCodeIgnore");
		ignoreMethods = Collections.unmodifiableSet(set);
	}
	


	public TransformCompilerFodder() {
		super();
	}
	public TransformCompilerFodder(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaBlockEnd.class),
				factory.token(JavaKeyword.class, "return"),
				factory.token(JavaToken.class),
				factory.token(JavaStatementTerminator.class),
				factory.any(
						factory.token(JavaComment.class, "compiler fodder"),
						factory.token(JavaComment.class, "fodder")
				));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		String shortName = javaMethod.name;
		String fullName = javaMethod.javaClass.className+"."+shortName;
		if (ignoreMethods.contains(fullName) || ignoreMethods.contains(shortName)) {
			return i;
		}

		int blockStart = javaMethod.methodBody.findStartOfBlock(i);
		JavaToken preBlock = (JavaToken)tokens.get(blockStart - 1);
		if (preBlock instanceof JavaKeyword && "else".equals(preBlock.value)) {
			// Can only trim out the return if this return wont be called.
			// This is just a quick approximate check
			tokens.remove(i+4);
			tokens.remove(i+3);
			tokens.remove(i+2);
			tokens.remove(i+1);
		}
		return i;
	}
}
