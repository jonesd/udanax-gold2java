/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaToken;

public class EnsureIfTestInParentheses implements MethodTransformation {

	public void transform(JavaMethod javaMethod) {
		List tokens = javaMethod.methodBody.tokens;
		for (int i = 0; i < tokens.size(); i++) {
			JavaToken token = (JavaToken) tokens.get(i);
			if ((token instanceof JavaKeyword) && token.value.equals("if") && !(tokens.get(i + 1) instanceof JavaParenthesisStart)) {
				int earlyParentheses = 0;
				int earlyBlocks = 0;
				boolean found = false;
				for (int j = i; j < tokens.size(); j++) {
					JavaToken test = (JavaToken) tokens.get(j);
					if (test instanceof JavaParenthesisStart) {
						earlyParentheses++;
					} else if (test instanceof JavaParenthesisEnd) {
						earlyParentheses--;
					} else if (test instanceof JavaBlockEnd) {
						earlyBlocks--;
					} else if (test instanceof JavaBlockStart) {
						if (earlyParentheses > 0 || earlyBlocks > 0) {
							earlyBlocks++;
						} else {
							found = true;
							tokens.add(j, new JavaParenthesisEnd());
							break;
						}
					}
				}
				if (!found) {
					throw new IllegalStateException("Couldn't find suitable location to insert missing ) for if");
				}
				tokens.add(i + 1, new JavaParenthesisStart());
			}
		}
	}

}