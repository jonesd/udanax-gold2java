/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.tokenmatcher;

import java.util.List;



public class MatchNot implements TokenMatcher {

	private final TokenMatcher tokenMatcher;
	
	public MatchNot(TokenMatcher tokenMatcher) {
		this.tokenMatcher = tokenMatcher;
	}
	
	public boolean doesMatch(List tokens, int i) {
		return !tokenMatcher.doesMatch(tokens, i);
	}

}
