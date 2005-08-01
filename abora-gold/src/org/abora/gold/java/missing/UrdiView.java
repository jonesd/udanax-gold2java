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

public class UrdiView extends Heaper {

	private Urdi urdi;
	
	public UrdiView() {
		super();
	}
	
	public UrdiView(Urdi urdi) {
		super();
		this.urdi = urdi;
	}

	public int getDataSizeOfSnarf(int/*SnarfID*/ mySnarfID) {
		throw new UnsupportedOperationException();
	}

	public void commitWrite() {
		throw new UnsupportedOperationException();
	}

	public void becomeRead() {
		throw new UnsupportedOperationException();
	}

	public SnarfHandle makeReadHandle(int/*SnarfID*/ snarfID) {
		throw new UnsupportedOperationException();
	}

	public SnarfHandle makeErasingHandle(int/*SnarfID*/ snarfID) {
		//TODO placeholder
		return new SnarfHandle();
	}

//	public int getDataSizeOfSnarf(int i) {
//		throw new UnsupportedOperationException();
//	}

}
