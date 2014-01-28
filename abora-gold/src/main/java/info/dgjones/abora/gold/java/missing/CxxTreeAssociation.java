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
package info.dgjones.abora.gold.java.missing;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import info.dgjones.abora.gold.java.missing.smalltalk.Association;


public class CxxTreeAssociation extends Association {

	private List children = new ArrayList();
	private CxxTreeAssociation parent = null;
	
	public CxxTreeAssociation(String key, Object value) {
		super(key, value);
	}

	public static CxxTreeAssociation keyValue(String string, Object object) {
		return new CxxTreeAssociation(string, object);
	}

	public CxxTreeAssociation yourself() {
		//TODO remove this method
		return this;
	}

	public CxxTreeAssociation addChild(CxxTreeAssociation association) {
		children.add(association);
		association.setParent(this);
		return this;
	}
	
	public CxxTreeAssociation getParent() {
		return parent;
	}
	
	protected void setParent(CxxTreeAssociation association) {
		if (parent != null) {
			throw new IllegalStateException("CxxTreeAssociation already has parent: "+parent+", "+association);
		}
		parent = association;
	}

	public CxxTreeAssociation findMatchingAssocation(String fileName) {
		if (fileName.equals(key())) {
			return this;
		}
		for (Iterator iter = children.iterator(); iter.hasNext();) {
			CxxTreeAssociation association = (CxxTreeAssociation) iter.next();
			CxxTreeAssociation match = association.findMatchingAssocation(fileName);
			if (match != null) {
				return match;
			}
		}
		return null;
	}

}
