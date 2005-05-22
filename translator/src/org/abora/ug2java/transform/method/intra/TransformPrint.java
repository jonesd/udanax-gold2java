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
import org.abora.ug2java.javatoken.JavaComment;
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
		//Watch out for immediately proceeding comment TODO would prefer to keep it
		if (tokens.get(i-1) instanceof JavaComment) {
			tokens.remove(i-1);
			i -= 1;
		}
		int startOfBlock = javaMethod.methodBody.findStartOfExpressionMinimal(i-1);
		//TODO special comment handling...
		if (startOfBlock + 2 < i && tokens.get(startOfBlock) instanceof JavaComment) {
				startOfBlock += 1;
		}
		if (startOfBlock + 2 < i && tokens.get(startOfBlock) instanceof JavaIdentifier) {
		
			JavaIdentifier identifier = (JavaIdentifier)tokens.get(startOfBlock);
			tokens.add(i, new JavaStatementTerminator());
			tokens.add(i + 1, new JavaIdentifier(identifier.value));
			// watch out for "AboraSupport.logger." case
			if (tokens.get(startOfBlock+1) instanceof JavaIdentifier) {
				JavaIdentifier identifier2 = (JavaIdentifier)tokens.get(startOfBlock+1);
				tokens.add(i+2, new JavaIdentifier(identifier2.value));
			}
		}
		
		return i;
	}
}
