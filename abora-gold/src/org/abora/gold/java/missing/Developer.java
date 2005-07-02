/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import org.abora.gold.java.AboraHeaper;


public class Developer extends AboraHeaper {
	private final String name;

	public Developer(String name) {
		super();
		this.name = name;
	}

	public void thingToDo() {
		throw new UnsupportedOperationException();
	}
	public void shouldImplement() {
		throw new UnsupportedOperationException();
	}
	public void knownBug() {
		throw new UnsupportedOperationException();
	}
	public void hack() {
		throw new UnsupportedOperationException();
	}

}
