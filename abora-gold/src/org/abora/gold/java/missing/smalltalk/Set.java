/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing.smalltalk;

import java.util.HashSet;
import java.util.Iterator;

public class Set {

	private HashSet set = new HashSet();
	
	public Set() {
		super();
	}

	public Set add(String string) {
		set.add(string);
		return this;
	}

	public Set add(String[] string) {
		set.add(string);
		return this;
	}

	public Iterator iterator() {
		return set.iterator();
	}

}
