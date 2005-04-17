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
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformConditionalOperator extends AbstractMethodBodyTransformation {

	public TransformConditionalOperator() {
		super();
	}
	public TransformConditionalOperator(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
						factory.token(JavaKeyword.class, "if"),
						factory.token(JavaParenthesisStart.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (i == 0) {
			return i;
		}
		for (int j = i -1; j > 0; j--) {
			JavaToken preToken = (JavaToken)tokens.get(j);
			if (preToken instanceof JavaStatementTerminator || preToken instanceof JavaBlockEnd || preToken instanceof JavaBlockStart) {
				// leave as normal if statement
				return i;
			} else if (!(preToken instanceof JavaComment)) {
				break;
			}
		}			
		int ifBlockStart = javaMethod.methodBody.findNextTokenOfTypeQuietFail(i, JavaBlockStart.class);
		if (ifBlockStart == -1) {
			return i;
		}
		int ifBlockEnd = javaMethod.methodBody.findEndOfBlockQuietFail(ifBlockStart);
		if (ifBlockEnd != -1 && ifBlockEnd < tokens.size() - 1) {
			JavaToken elseKeyword = (JavaToken)tokens.get(ifBlockEnd+1);
			if (elseKeyword instanceof JavaKeyword && elseKeyword.value.equals("else")) {
				int elseBlockStart = javaMethod.methodBody.findNextTokenOfTypeQuietFail(ifBlockEnd, JavaBlockStart.class);
				if (elseBlockStart == -1) {
					return i;
				}
				int elseBlockEnd = javaMethod.methodBody.findEndOfBlockQuietFail(elseBlockStart);
				if (elseBlockEnd == -1) {
					return i;
				}
				javaMethod.methodBody.removeShouldMatch(elseBlockEnd, JavaBlockEnd.class);
				javaMethod.methodBody.removeShouldMatch(elseBlockStart, JavaBlockStart.class);
				javaMethod.methodBody.removeShouldMatch(elseBlockStart-1, JavaKeyword.class, "else");
				javaMethod.methodBody.removeShouldMatch(elseBlockStart-2, JavaBlockEnd.class);
				javaMethod.methodBody.remove(elseBlockStart-3);//TODOelseShouldMatch(elseBlockStart-3, JavaStatementTerminator.class|JavaBlockEnd.class);
				tokens.add(elseBlockStart-3, new JavaKeyword(":"));
				javaMethod.methodBody.removeShouldMatch(ifBlockStart, JavaBlockStart.class);
				tokens.add(ifBlockStart, new JavaKeyword("?"));
				javaMethod.methodBody.removeShouldMatch(i, JavaKeyword.class, "if");
			}
		}
		return i;
	}
}
