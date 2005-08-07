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

/**
 * Mickey mouse implementation to keep code references happy, styled after XPP sema4x.cxx.
 * 
 * The current translated Java code is actually using the Sema4 instances as monitor
 * locks.
 * 
 * TODO review the use of these instances!
 */
public class Sema4 extends Heaper {

	protected int count;
	
	public Sema4(int initialCount) {
		super();
		this.count = initialCount;
	}
	public static Sema4 make(int initialCount) {
		return new Sema4(initialCount);
	}
	
	public void v() {
		count += 1;
	}
	
	public void p() {
		count -= 1;
	}
	
	public int t() {
		int result = count;
		if (count > 0) {
			count -= 1;
		}
		return result;
	}

}
