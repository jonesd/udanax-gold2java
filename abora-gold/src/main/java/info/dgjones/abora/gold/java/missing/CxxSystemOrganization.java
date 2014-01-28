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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import info.dgjones.abora.gold.java.AboraHeaper;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Association;


public class CxxSystemOrganization {

	private final String fileName;
	private String comment = "";
	private final Map classDescriptionsByAccess = new HashMap();
	
	private static CxxTreeAssociation root = new CxxTreeAssociation("ROOT", "ROOT");
	
	private static final Map ORGANIZATIONS = new HashMap();
	
	public CxxSystemOrganization(String fileName) {
		super();
	
		this.fileName = fileName;
	}
	
	public void comment(String comment) {
		this.comment = comment;
	}
	
	public CxxSystemOrganization addClassIn(CxxClassDescription classDescription, String access) {
		Set set = getClassDescriptionsWithAccess(access);
		set.add(classDescription);
		classDescription.setSystemOrga1nization(this);
		//TODO just to make it fit in with XnBufferedWriteStream>>initializeSystemOrganization method...
		return this;
	}
	
	private Set getClassDescriptionsWithAccess(String access2) {
		Set classDescriptions = (Set) classDescriptionsByAccess.get(access2);
		if (classDescriptions == null) {
			classDescriptions = new HashSet();
			classDescriptionsByAccess.put(access2, classDescriptions);
		}
		return classDescriptions;
	}

	public static CxxSystemOrganization fileNamed(String fileName) {
		if (ORGANIZATIONS.containsKey(fileName)) {
			throw new AboraRuntimeException("Organization entry already exists for file named: "+fileName);
		}
		CxxSystemOrganization organization = new CxxSystemOrganization(fileName);
		ORGANIZATIONS.put(fileName, organization);
		return organization;
	}
	
	public static CxxSystemOrganization getOrMakeFileNamed(String fileName) {
		return (CxxSystemOrganization) ORGANIZATIONS.get(fileName);
	}

	public void cxxHeaderIn(String includeDirective, String access) {
		//TODO implement...
		
	}

	public void hxxHeaderIn(String headerDirectives, String access) {
		//TODO implement...
		
	}

	public CxxSystemOrganization commentIn(String string, String public1) {
		//TODO implement
		return this;
	}

	public static CxxTreeAssociation tree() {
		return root;
	}

	public static CxxTreeAssociation tree(CxxTreeAssociation association) {
		// TODO Auto-generated method stub
		root = association;
		return association;
	}

	public static Association fetchDirectory(CxxSystemOrganization systemOrganization) {
		String fileName = systemOrganization.getFileName();
		CxxTreeAssociation association = tree().findMatchingAssocation(fileName);
		while (association != null) {
			if (AboraHeaper.DIR.equals(association.value())) {
				return association;
			}
			association = association.getParent();
		}
		return null;
	}
	
	public String getFileName() {
		return fileName;
	}

}
