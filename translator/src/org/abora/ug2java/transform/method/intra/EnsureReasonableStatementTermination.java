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
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.MethodTransformation;



public class EnsureReasonableStatementTermination implements MethodTransformation {

	public void transform(JavaMethod javaMethod) {
		List tokens = javaMethod.methodBody.tokens;
		//TODO we are ignoring comments?
		if (tokens.isEmpty()) {
			return;
		}
		for (int i = tokens.size() - 1; i >= 0; i-- ) {
			JavaToken token = (JavaToken)tokens.get(i);
			if (!(token instanceof JavaComment)) {
				if (!(token instanceof JavaStatementTerminator) || !(token instanceof JavaBlockEnd)) {
					tokens.add(i+1, new JavaStatementTerminator());
				}
				break;
			}
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
