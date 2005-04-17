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
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformPrint extends AbstractMethodBodyTransformation {


	public TransformPrint() {
		super();
	}
	public TransformPrint(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaKeyword.class, "<<");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int endOfBlock = javaMethod.methodBody.findEndOfExpression(i+1);
		for (int j = i + 2; j < endOfBlock; j ++) {
			JavaToken token = (JavaToken)tokens.get(j);
			if ((token instanceof JavaKeyword) && token.value.equals("<<")) {
				endOfBlock = j - 1;
			}
		}
		tokens.add(endOfBlock+1, new JavaCallEnd());
		tokens.remove(i);
		tokens.add(i, new JavaCallKeywordStart("print"));
		int startOfBlock = javaMethod.methodBody.findStartOfExpression(i-1);
		//TODO watchc out for "AboraSupport.getPrintWriter()." case
		if (startOfBlock + 1 != i && tokens.get(startOfBlock) instanceof JavaIdentifier && !((JavaIdentifier)tokens.get(startOfBlock)).value.equals("AboraSupport")) {
			JavaIdentifier identifier = (JavaIdentifier)tokens.get(startOfBlock);
			tokens.add(i, new JavaStatementTerminator());
			tokens.add(i + 1, new JavaIdentifier(identifier.value));
		}
		
		return i;
	}
}
