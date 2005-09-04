/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing.smalltalk;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.abora.gold.xcvr.Recipe;
import org.abora.gold.xpp.basic.Category;
import org.abora.gold.xpp.fluid.FluidVar;


public class Smalltalk {
	private static final List associations = new ArrayList();

	
	public Smalltalk() {
		super();
	}

	public static void atPut(String recipeName, Recipe value) {
		for (Iterator iter = associations.iterator(); iter.hasNext();) {
			Association element = (Association) iter.next();
			if (element.key().equals(recipeName)) {
				element.refAssign(value);
				return;
			}
		}
		associations.add(new Association(recipeName, null));
	}
	
	public static Category at(Symbol className) {
		throw new UnsupportedOperationException();
	}

	public static Category at(String className) {
		for (Iterator iter = associations.iterator(); iter.hasNext();) {
			Association element = (Association) iter.next();
			if (element.key().equals(className)) {
				return (Category)element.value();
			}
		}
		throw new IllegalArgumentException("Could not find: "+className);
	}

	public static void garbageCollect() {
		throw new UnsupportedOperationException();
	}

	public static void safeAtPut(String varName, FluidVar var) {
		throw new UnsupportedOperationException();
	}

	public static Category ifAbsent(String clName) {
		throw new UnsupportedOperationException();
	}

	public static Category ifAbsent(String varName, Object object) {
		throw new UnsupportedOperationException();
	}

	public static Association associationAt(String xpp_cuisine) {
		//TODO this must be wrong!
		for (Iterator iter = associations.iterator(); iter.hasNext();) {
			Association element = (Association) iter.next();
			if (element.key().equals(xpp_cuisine)) {
				return element;
			}
		}
		throw new IllegalArgumentException("Could not find: "+xpp_cuisine);
	}

	public static Association associationAtIfAbsent(String string, Association association) {
		for (Iterator iter = associations.iterator(); iter.hasNext();) {
			Association element = (Association) iter.next();
			if (element.key().equals(string)) {
				return element;
			}
		}
		//TODO should we really write this...
		association.setKey(string);
		associations.add(association);
		return association;
	}

}
