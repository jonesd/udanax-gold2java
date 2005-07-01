/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import org.abora.gold.java.Fn;
import org.abora.gold.nkernel.FeEdition;
import org.abora.gold.wrapper.FeWrapper;

public class FeDirectWrapperMaker extends Fn {

	public FeDirectWrapperMaker() {
		super();
	}

	public FeWrapper invokeFunction(FeEdition edition) {
		throw new UnsupportedOperationException();
	}

}
