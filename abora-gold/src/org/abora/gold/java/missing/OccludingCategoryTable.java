/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import org.abora.gold.xpp.converters.CategoryTable;

public class OccludingCategoryTable extends CategoryTable {

	public OccludingCategoryTable() {
		super();
	}

	public OccludingCategoryTable(CategoryTable front, CategoryTable back) {
		throw new UnsupportedOperationException();
	}
}
