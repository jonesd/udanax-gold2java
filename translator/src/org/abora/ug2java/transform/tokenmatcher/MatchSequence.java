/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.tokenmatcher;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.abora.ug2java.util.ToStringGenerator;



public class MatchSequence implements TokenMatcher {

	private final List matchers = new ArrayList();
	
	public MatchSequence() {
		super();
	}
	
	public void add(TokenMatcher matcher) {
		matchers.add(matcher);
	}
	
	public boolean doesMatch(List tokens, int i) {
		for (Iterator iter = matchers.iterator(); iter.hasNext();) {
			TokenMatcher tokenMatcher = (TokenMatcher) iter.next();
			if (i >= tokens.size() || !tokenMatcher.doesMatch(tokens, i)) {
				return false;
			}
			i++;
		}
		return true;
	}
	
	public String toString() {
		ToStringGenerator generator = new ToStringGenerator(this);
		generator.add("matchers", matchers);
		return generator.end();
	}

}
