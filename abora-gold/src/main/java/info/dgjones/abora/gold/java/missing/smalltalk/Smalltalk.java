/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.gold.java.missing.smalltalk;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.fluid.FluidVar;


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
