package org.abora.ug2java.transform.tokenmatcher;

import java.util.List;



public interface TokenMatcher {
	public boolean doesMatch(List tokens, int i);
}
