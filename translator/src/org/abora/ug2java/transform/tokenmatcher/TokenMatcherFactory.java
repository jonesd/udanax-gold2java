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

	public TokenMatcher seq(TokenMatcher matcher1, TokenMatcher matcher2) {
		MatchAll matchAll = new MatchAll();
		matchAll.add(matcher1);
		matchAll.add(matcher2);
		return matchAll;
	}

	public TokenMatcher seq(TokenMatcher matcher1, TokenMatcher matcher2, TokenMatcher matcher3) {
		MatchAll matchAll = new MatchAll();
		matchAll.add(matcher1);
		matchAll.add(matcher2);
		matchAll.add(matcher3);
		return matchAll;
	}

	public TokenMatcher seq(TokenMatcher matcher1, TokenMatcher matcher2, TokenMatcher matcher3, TokenMatcher matcher4) {
		MatchAll matchAll = new MatchAll();
		matchAll.add(matcher1);
		matchAll.add(matcher2);
		matchAll.add(matcher3);
		matchAll.add(matcher4);
		return matchAll;
	}

	public TokenMatcher token(Class clazz) {
		return new MatchToken(clazz);
	}

	public TokenMatcher token(Class clazz, String value) {
		return new MatchToken(clazz, value);
	}
}
