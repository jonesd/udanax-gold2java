/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformInitializer extends AbstractMethodBodyTransformation {
	

	public TransformInitializer() {
		super();
	}
	public TransformInitializer(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "Initializer"),
				factory.token(JavaCallStart.class, "doMain|withDoMain"));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int argumentStart = javaMethod.methodBody.findNextTokenOfType(i+1, JavaBlockStart.class) - 1;
//		int startOfBlack = argumentStart+1;
		int startOfBlock = argumentStart+1;
		if (argumentStart == i + 1) {
			argumentStart += 1;
		}
		if (tokens.get(startOfBlock) instanceof JavaComment) {
			//TODO should be filtering out comments implicitly
			++startOfBlock;
		}
		if (!(tokens.get(startOfBlock) instanceof JavaBlockStart)) {
			System.out.println("--Failed JavaBlockStart match for fluidBindDuring");
			return i;
		}
		int endOfBlock = javaMethod.methodBody.findEndOfBlock(startOfBlock);
		if (!(tokens.get(endOfBlock+1) instanceof JavaCallEnd)) {
			System.out.println("--Failed JavaBlockEnd match for fluidBindDuring");
			return i;
		}
		int endOfCall = endOfBlock+1;// javaMethod.methodBody.findClosingCallEnd(i+1);

		tokens.remove(endOfCall+1);
		tokens.remove(endOfCall);
		tokens.add(endOfCall, new JavaKeyword("finally"));
		tokens.add(endOfCall+1, new JavaBlockStart());
		tokens.add(endOfCall+2, new JavaIdentifier("Initializer"));
		tokens.add(endOfCall+3, new JavaCallKeywordStart("exitDoMain"));
		tokens.add(endOfCall+4, new JavaCallEnd());
		tokens.add(endOfCall+5, new JavaStatementTerminator());
		tokens.add(endOfCall+6, new JavaBlockEnd());
		
		tokens.add(argumentStart, new JavaCallEnd());
		tokens.add(argumentStart+1, new JavaStatementTerminator());
		tokens.add(argumentStart+2, new JavaKeyword("try"));
		if (tokens.get(argumentStart+3) instanceof JavaCallArgumentSeparator) {
			tokens.remove(argumentStart+3);
		}
		
		tokens.remove(i+1);
		tokens.remove(i);
		
		tokens.add(i, new JavaIdentifier("Initializer"));
		tokens.add(i+1, new JavaCallKeywordStart("enterDoMain"));
		
		return i;
	}
}
