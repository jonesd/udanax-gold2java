/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import org.abora.gold.snarf.FlockInfo;
import org.abora.gold.xpp.basic.Category;
import org.abora.gold.xpp.basic.Heaper;

public class ShepherdStub extends Heaper {

	public ShepherdStub() {
		super();
	}

	public ShepherdStub(int theHash, FlockInfo info, Category theCategory) {
		throw new UnsupportedOperationException();
	}

	public ShepherdStub(int hash, Category newCat) {
		throw new UnsupportedOperationException();
	}
}
