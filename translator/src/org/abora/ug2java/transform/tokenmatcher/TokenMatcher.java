/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.tokenmatcher;

import java.util.List;



public interface TokenMatcher {
	public boolean doesMatch(List tokens, int i);
}
