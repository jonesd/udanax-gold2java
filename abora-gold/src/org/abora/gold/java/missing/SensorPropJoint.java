/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import org.abora.gold.id.IDRegion;
import org.abora.gold.spaces.basic.XnRegion;


public class SensorPropJoint extends PropJoint /*GUESS*/ {

	public SensorPropJoint(XnRegion unioned, XnRegion intersected) {
		super(unioned, intersected);
	}
	public boolean isPartial() {
		throw new UnsupportedOperationException();
	}
	public IDRegion relevantPermissions() {
		throw new UnsupportedOperationException();
	}
	public IDRegion relevantEndorsements() {
		throw new UnsupportedOperationException();
	}
}
