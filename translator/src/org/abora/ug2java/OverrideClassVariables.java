package org.abora.ug2java;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;



public class OverrideClassVariables implements ClassTransformer {

	private static final Map METHODS;
	static {
		Map map = new HashMap();
		map.put("DiskManager.myFluidSpace", "Array");

		METHODS = Collections.unmodifiableMap(map);
	}

	
	private String overrideWith(JavaClass javaClass, JavaField field) {
		String shortName = field.name;
		String fullName = javaClass.className+"."+shortName;
		String type = (String)METHODS.get(fullName);
		if (type == null) {
			type = (String)METHODS.get(shortName);
		}
		return type;
	}
	
	public void transform(JavaClass javaClass) {
		for (Iterator iter = javaClass.fields.iterator(); iter.hasNext();) {
			JavaField field = (JavaField) iter.next();
			String replacementType = overrideWith(javaClass, field);
			if (replacementType != null) {
				field.type = replacementType;
			}
		}
	}
}
