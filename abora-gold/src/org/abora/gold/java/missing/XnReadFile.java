/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import org.abora.gold.xcvr.XnReadStream;

public class XnReadFile extends XnReadStream {

	public XnReadFile() {
		super();
	}

	public static XnReadStream make(String myReadName) {
		throw new UnsupportedOperationException();
	}

	public static XnReadStream make(Object object) {
		throw new UnsupportedOperationException();
	}

}
