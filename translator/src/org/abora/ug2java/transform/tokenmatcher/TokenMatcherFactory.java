/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.tokenmatcher;



public class TokenMatcherFactory {

	public TokenMatcherFactory() {
		super();
	}

	public TokenMatcher not(TokenMatcher matcher) {
		return new MatchNot(matcher);
	}
	
	public TokenMatcher any(TokenMatcher matcher1, TokenMatcher matcher2) {
		MatchAny matchAny = new MatchAny();
		matchAny.add(matcher1);
		matchAny.add(matcher2);
		return matchAny;
	}

	public TokenMatcher any(TokenMatcher matcher1, TokenMatcher matcher2, TokenMatcher matcher3) {
		MatchAny matchAny = new MatchAny();
		matchAny.add(matcher1);
		matchAny.add(matcher2);
		matchAny.add(matcher3);
		return matchAny;
	}

	public TokenMatcher any(TokenMatcher matcher1, TokenMatcher matcher2, TokenMatcher matcher3, TokenMatcher matcher4) {
		MatchAny matchAny = new MatchAny();
		matchAny.add(matcher1);
		matchAny.add(matcher2);
		matchAny.add(matcher3);
		matchAny.add(matcher4);
		return matchAny;
	}

	public TokenMatcher seq(TokenMatcher matcher1, TokenMatcher matcher2) {
		MatchSequence matchAll = new MatchSequence();
		matchAll.add(matcher1);
		matchAll.add(matcher2);
		return matchAll;
	}

	public TokenMatcher seq(TokenMatcher matcher1, TokenMatcher matcher2, TokenMatcher matcher3) {
		MatchSequence matchAll = new MatchSequence();
		matchAll.add(matcher1);
		matchAll.add(matcher2);
		matchAll.add(matcher3);
		return matchAll;
	}

	public TokenMatcher seq(TokenMatcher matcher1, TokenMatcher matcher2, TokenMatcher matcher3, TokenMatcher matcher4) {
		MatchSequence matchAll = new MatchSequence();
		matchAll.add(matcher1);
		matchAll.add(matcher2);
		matchAll.add(matcher3);
		matchAll.add(matcher4);
		return matchAll;
	}

	public TokenMatcher seq(TokenMatcher matcher1, TokenMatcher matcher2, TokenMatcher matcher3, TokenMatcher matcher4, TokenMatcher matcher5) {
		MatchSequence matchAll = new MatchSequence();
		matchAll.add(matcher1);
		matchAll.add(matcher2);
		matchAll.add(matcher3);
		matchAll.add(matcher4);
		matchAll.add(matcher5);
		return matchAll;
	}

	public TokenMatcher token(Class clazz) {
		return new MatchToken(clazz);
	}

	public TokenMatcher token(Class clazz, String value) {
		return new MatchToken(clazz, value);
	}
}
