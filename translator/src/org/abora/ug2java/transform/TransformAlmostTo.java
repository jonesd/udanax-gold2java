/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaAssignment;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformAlmostTo extends MethodBodyTransformation {

	public TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallKeywordStart.class, "almostToDo");
	}

	public void transform(JavaMethod javaMethod, List tokens, int i) {
		int expressionStart = javaMethod.methodBody.findStartOfExpression(i - 1);
		tokens.add(expressionStart, new JavaKeyword("for"));
		tokens.add(expressionStart + 1, new JavaParenthesisStart());

		int blockStart = javaMethod.methodBody.findNextTokenOfType(i + 2, JavaBlockStart.class);
		JavaToken variableType = (JavaToken) tokens.get(blockStart + 1);
		if (!variableType.value.equals("int")) {
			System.out.println("-- Warning: Non-int almostToDo variable");
			//	throw new IllegalStateException("Non-int almostToDo variable");
		}
		JavaToken variable = (JavaToken) tokens.get(blockStart + 2);
		tokens.add(expressionStart + 2, variableType);
		tokens.add(expressionStart + 3, variable);
		tokens.add(expressionStart + 4, new JavaAssignment());
		tokens.add(i + 5, new JavaKeyword(";"));
		tokens.add(i + 6, variable);
		tokens.add(i + 7, new JavaKeyword("<"));
		tokens.remove(i + 8);
		tokens.add(blockStart + 4, new JavaKeyword(";"));
		tokens.add(blockStart + 5, variable);
		tokens.add(blockStart + 6, new JavaKeyword("++"));
		tokens.add(blockStart + 7, new JavaParenthesisEnd());
		tokens.remove(blockStart + 8);
		tokens.remove(blockStart + 9);
		tokens.remove(blockStart + 9);
		tokens.remove(blockStart + 9);
		int blockEnd = javaMethod.methodBody.findEndOfBlock(blockStart + 8);
		tokens.remove(blockEnd + 1);
		tokens.remove(blockEnd + 1);
	}
}
