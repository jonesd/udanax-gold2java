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
	private boolean isWritable = false;
	
	public UrdiView(Urdi urdi, boolean isWritable) {
		super();
		this.urdi = urdi;
		this.isWritable = isWritable;
	}

	public int getDataSizeOfSnarf(int/*SnarfID*/ mySnarfID) {
		//TODO implement properly
		return urdi.getDataSizeOfSnarf(mySnarfID);
	}

	public void commitWrite() {
		//TODO implement
		System.out.println("Ignoring UrdiView commitWrite: "+urdi);
	}

	public void becomeRead() {
		isWritable = false;
	}

	public SnarfHandle makeReadHandle(int/*SnarfID*/ snarfID) {
		//TODO placeholder
		return new SnarfHandle(snarfID, urdi.getSpace(snarfID));
	}

	public SnarfHandle makeErasingHandle(int/*SnarfID*/ snarfID) {
		//TODO placeholder
		SnarfHandle handle =  new SnarfHandle(snarfID, urdi.getSpace(snarfID));
		handle.makeWritable();
		return handle;
	}
}
