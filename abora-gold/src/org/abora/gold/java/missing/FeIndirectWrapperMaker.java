/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import org.abora.gold.nkernel.FeEdition;
import org.abora.gold.wrapper.FeWrapper;
import org.abora.gold.xpp.basic.Heaper;

public class FeIndirectWrapperMaker extends Heaper {

	public FeIndirectWrapperMaker() {
		super();
	}

	public FeWrapper invokeFunction(FeEdition edition, FeWrapper inner) {
		throw new UnsupportedOperationException();
	}

}
