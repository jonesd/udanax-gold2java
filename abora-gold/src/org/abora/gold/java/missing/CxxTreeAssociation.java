package org.abora.gold.java.missing;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.abora.gold.java.missing.smalltalk.Association;


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
