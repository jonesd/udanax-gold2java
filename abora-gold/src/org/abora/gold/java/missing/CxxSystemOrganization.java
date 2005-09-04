package org.abora.gold.java.missing;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.abora.gold.java.AboraHeaper;
import org.abora.gold.java.exception.AboraRuntimeException;
import org.abora.gold.java.missing.smalltalk.Association;


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
