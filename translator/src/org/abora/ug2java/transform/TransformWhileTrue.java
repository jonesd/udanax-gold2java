/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformWhileTrue extends AbstractMethodBodyTransformation {

	public TransformWhileTrue() {
		super();
	}
	public TransformWhileTrue(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaStatementTerminator.class), 
				factory.token(JavaBlockEnd.class), 
				factory.token(JavaCallKeywordStart.class, "whileTrue"));
	}
	
	protected void transform(JavaMethod javaMethod, List tokens, int i) {
		int preBlockStart = javaMethod.methodBody.findStartOfBlock(i + 1);
		tokens.add(preBlockStart, new JavaKeyword("while"));
		tokens.remove(preBlockStart + 1);
		tokens.add(preBlockStart + 1, new JavaParenthesisStart());
		tokens.remove(i + 1); // ;
		tokens.remove(i + 1); // }					
		tokens.add(i + 1, new JavaParenthesisEnd());
		int postCallEnd = javaMethod.methodBody.findClosingCallEnd(i + 2);
		if (postCallEnd + 1 < tokens.size() && (tokens.get(postCallEnd + 1) instanceof JavaStatementTerminator)) {
			tokens.remove(postCallEnd + 1);
		}
		tokens.remove(postCallEnd);
		tokens.remove(i+2);
	}
}
