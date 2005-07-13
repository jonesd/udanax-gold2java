/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import org.abora.gold.xpp.basic.Heaper;

public class Sema4 extends Heaper {

	protected int a;
	
	public Sema4(int a) {
		super();
		this.a = a;
	}
	public static Sema4 make(int a) {
		return new Sema4(a);
	}

}
