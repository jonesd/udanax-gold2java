/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaType;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformFluidBindDuring extends AbstractMethodBodyTransformation {
	

	public TransformFluidBindDuring() {
		super();
	}
	public TransformFluidBindDuring(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallStart.class, "fluidBindDuring"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier identifier = (JavaIdentifier)tokens.get(i);
		String oldValueVariable = identifier.value+"OldValue";
		int argumentStart = javaMethod.methodBody.findNextTokenOfType(i+1, JavaCallArgumentSeparator.class);
		if (!(tokens.get(argumentStart+1) instanceof JavaBlockStart)) {
			System.out.println("--Failed JavaBlockStart match for fluidBindDuring");
			return i;
		}
		int endOfBlock = javaMethod.methodBody.findEndOfBlock(argumentStart+1);
		if (!(tokens.get(endOfBlock+1) instanceof JavaCallEnd)) {
			System.out.println("--Failed JavaBlockEnd match for fluidBindDuring");
			return i;
		}
		int endOfCall = endOfBlock+1;// javaMethod.methodBody.findClosingCallEnd(i+1);

		tokens.remove(endOfCall+1);
		tokens.remove(endOfCall);
		tokens.add(endOfCall, new JavaKeyword("finally"));
		tokens.add(endOfCall+1, new JavaBlockStart());
		tokens.add(endOfCall+2, new JavaIdentifier("AboraBlockSupport"));
		tokens.add(endOfCall+3, new JavaCallKeywordStart("exitFluidBindDuring"));
		tokens.add(endOfCall+4, new JavaIdentifier(identifier.value));
		tokens.add(endOfCall+5, new JavaCallArgumentSeparator());
		tokens.add(endOfCall+6, new JavaIdentifier(oldValueVariable));
		tokens.add(endOfCall+7, new JavaCallEnd());
		tokens.add(endOfCall+8, new JavaStatementTerminator());
		tokens.add(endOfCall+9, new JavaBlockEnd());
		
		tokens.remove(argumentStart);
		tokens.add(argumentStart, new JavaCallEnd());
		tokens.add(argumentStart+1, new JavaStatementTerminator());
		tokens.add(argumentStart+2, new JavaKeyword("try"));
		
		tokens.remove(i+1);
		tokens.remove(i);
		
		tokens.add(i, new JavaType("Object"));
		tokens.add(i+1, new JavaIdentifier(oldValueVariable));
		tokens.add(i+2, new JavaAssignment());
		tokens.add(i+3, new JavaIdentifier("AboraBlockSupport"));
		tokens.add(i+4, new JavaCallKeywordStart("enterFluidBindDuring"));
		tokens.add(i+5, new JavaIdentifier(identifier.value));
		tokens.add(i+6, new JavaCallArgumentSeparator());
		
		return i;
	}
}
