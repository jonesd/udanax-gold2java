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
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;



public class EnsureReasonableStatementTermination implements MethodTransformation {

	public void transform(JavaMethod javaMethod) {
		List tokens = javaMethod.methodBody.tokens;
		//TODO we are ignoring comments?
		if (tokens.isEmpty()) {
			return;
		}
		if (!(tokens.get(tokens.size() - 1) instanceof JavaStatementTerminator) || !(tokens.get(tokens.size() - 1) instanceof JavaBlockEnd)) {
			tokens.add(new JavaStatementTerminator());
		}
		for (int i = tokens.size() - 1; i >= 1; i--) {
			JavaToken token = (JavaToken) tokens.get(i);
			if (token instanceof JavaBlockEnd) {
				JavaToken previousToken = (JavaToken) tokens.get(i - 1);
				if (!(previousToken instanceof JavaStatementTerminator) || !(previousToken instanceof JavaBlockStart)) {
					tokens.add(i, new JavaStatementTerminator());
				}
			}
		}
		for (int i = tokens.size() - 1; i >= 1; i--) {
			JavaToken token = (JavaToken) tokens.get(i);
			if (token instanceof JavaStatementTerminator) {
				JavaToken previousToken = (JavaToken) tokens.get(i - 1);
				if ((previousToken instanceof JavaStatementTerminator)
					|| (previousToken instanceof JavaBlockStart)
					|| (previousToken instanceof JavaBlockEnd)) {
					tokens.remove(i);
				}
			}
		}
	}

}
