package org.abora.ug2java.transform.tokenmatcher;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.abora.ug2java.util.ToStringGenerator;



public class MatchAny implements TokenMatcher {

	private final List matchers = new ArrayList();
	
	public MatchAny() {
		super();
	}
	
	public void add(TokenMatcher matcher) {
		matchers.add(matcher);
	}
	
	public boolean doesMatch(List tokens, int i) {
		for (Iterator iter = matchers.iterator(); iter.hasNext();) {
			TokenMatcher tokenMatcher = (TokenMatcher) iter.next();
			if (tokenMatcher.doesMatch(tokens, i)) {
				return true;
			}
		}
		return false;
	}
	
	public String toString() {
		ToStringGenerator generator = new ToStringGenerator(this);
		generator.add("matchers", matchers);
		return generator.end();
	}

}
