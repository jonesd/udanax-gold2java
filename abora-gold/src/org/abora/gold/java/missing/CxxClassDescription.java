/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.abora.gold.java.exception.AboraRuntimeException;
import org.abora.gold.java.missing.smalltalk.AboraClass;
import org.abora.gold.java.missing.smalltalk.Association;
import org.abora.gold.java.missing.smalltalk.Set;
import org.abora.gold.xpp.basic.Heaper;

public class CxxClassDescription extends Heaper {

	private final Map map = new HashMap();
	private final AboraClass aboraClass;
	private CxxSystemOrganization systemOrganization = null;
	
	public CxxClassDescription(AboraClass aboraClass) {
		super();
		
		this.aboraClass = aboraClass;
	}

	public Association fetchDirectory() {
		return CxxSystemOrganization.fetchDirectory(systemOrganization);
	}

	public String fetchAttribute(String name) {
		return (String)map.get(name);
	}
	
	public boolean hasAttribute(String name) {
		return map.containsKey(name);
	}
	
	public boolean includesAttribute(String name) {
		return hasAttribute(name);
	}

	public void setAttributes(Set set) {
		//TODO should this be in the constructor?
		for (Iterator iter = set.iterator(); iter.hasNext();) {
			Object element = iter.next();
			if (element instanceof String) {
				map.put(element, null);
			} else if (element instanceof String[]) {
				String[] pair = (String[])element;
				if (pair.length != 2) {
					throw new AboraRuntimeException("Atribute pair should only have 2 elements: "+pair);
				}
				map.put(pair[0], pair[1]);
			} else {
				throw new AboraRuntimeException("Unknown attribute: "+element);
			}
		}
		
	}

	public void setSystemOrga1nization(CxxSystemOrganization organization) {
		systemOrganization = organization;
	}

	public void printOn(PrintWriter oo) {
		oo.print(getAboraClass().name());
		oo.print("(");
		oo.print(aboraClass.name());
		oo.print(",");
		oo.print(map);
		oo.print(")");
	}
}
