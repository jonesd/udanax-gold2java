/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

public class BertPropJoint extends PropJoint {

	public BertPropJoint() {
		super();
	}
	public PropJoint permissionsJoint() {
		throw new UnsupportedOperationException();
	}
	public PropJoint endorsementsJoint() {
		throw new UnsupportedOperationException();
	}
	public boolean isNotPartializable() {
		throw new UnsupportedOperationException();
	}
	public boolean isSensorWaiting() {
		throw new UnsupportedOperationException();
	}
}
