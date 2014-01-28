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

import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.AboraClass;
import info.dgjones.abora.gold.java.missing.smalltalk.Association;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xpp.basic.Heaper;

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
