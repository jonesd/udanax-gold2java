package org.abora.gold.java.missing;


public class CxxTreeAssociation {

	public CxxTreeAssociation(String key, Object value) {
		super();
	}

	public static CxxTreeAssociation keyValue(String string, Object object) {
		return new CxxTreeAssociation(string, object);
	}

	public CxxTreeAssociation yourself() {
		//TODO remove this method
		return this;
	}

	public CxxTreeAssociation addChild(CxxTreeAssociation association) {
		// TODO Auto-generated method stub
		return this;
	}

}
