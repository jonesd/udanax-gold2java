/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.MethodTransformation;

public class EnsureIfTestInParentheses implements MethodTransformation {

	public void transform(JavaMethod javaMethod) {
		List tokens = javaMethod.methodBody.tokens;
		for (int i = 0; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.get(i);
			if ((token instanceof JavaKeyword) && token.value.equals("if")) {
				if (!(tokens.get(i + 1) instanceof JavaParenthesisStart)) {			
					int blockStart = javaMethod.methodBody.findNextTokenOfType(i+1, JavaBlockStart.class);
					tokens.add(blockStart, new JavaParenthesisEnd());
					tokens.add(i+1, new JavaParenthesisStart());
				}
				else if (tokens.get(i+1) instanceof JavaParenthesisStart) {
					int parenEnd = javaMethod.methodBody.findClosingTokenOfType(i+1, JavaParenthesisEnd.class);
					int blockStart = javaMethod.methodBody.findNextTokenOfType(i+1, JavaBlockStart.class);
					if (parenEnd < blockStart - 1) {
						tokens.add(blockStart, new JavaParenthesisEnd());
						tokens.add(i+1, new JavaParenthesisStart());
					}
				}
			}
		}
	}

}