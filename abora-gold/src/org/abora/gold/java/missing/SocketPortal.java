/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import org.abora.gold.proman.PacketPortal;
import org.abora.gold.proman.Portal;

public class SocketPortal extends Portal {

	public SocketPortal() {
		super();
	}

	public static PacketPortal make(int socket) {
		throw new UnsupportedOperationException();
	}
}
