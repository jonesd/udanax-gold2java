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
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformUses extends MethodBodyTransformation {

	public TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaBlockEnd.class), 
				factory.token(JavaIdentifier.class, "USES"));
	}

	public void transform(JavaMethod javaMethod, List tokens, int i) {
		if (i + 2 < tokens.size() && (tokens.get(i + 2) instanceof JavaStatementTerminator)) {
			tokens.remove(i + 2);
		}
		tokens.remove(i + 1);
		tokens.remove(i);
		int j = i - 1;
		//TODO add a first element to the matching sequence to ensure that there is at least
		// a token before i?
		while (!(tokens.get(j) instanceof JavaBlockStart)) {
			tokens.remove(j);
			j--;
		}
		tokens.remove(j);
	}
}
