/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.tokenmatcher;

import java.util.List;

import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.util.ToStringGenerator;



public class MatchToken implements TokenMatcher {

	private final Class tokenClass;
	private final String tokenValue;
	
	
	public MatchToken(Class tokenClass) {
		this(tokenClass, null);
	}
	
	public MatchToken(Class tokenClass, String tokenValue) {
		this.tokenClass = tokenClass;
		this.tokenValue = tokenValue;
	}
	
	public boolean doesMatch(List tokens, int i) {
		JavaToken javaToken = (JavaToken)tokens.get(i);
		return (tokenClass.isAssignableFrom(javaToken.getClass())) && (tokenValue == null || tokenValue.equals(javaToken.value));
	}

	public String toString() {
		ToStringGenerator generator = new ToStringGenerator(this);
		generator.add("tokenClass", tokenClass);
		generator.add("tokenValue", tokenValue);
		return generator.end();
	}
}
