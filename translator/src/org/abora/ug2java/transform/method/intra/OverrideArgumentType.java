/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.abora.ug2java.JavaField;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.transform.method.MethodTransformation;

public class OverrideArgumentType implements MethodTransformation {

	private static final Map METHODS;
	static {
		Map map = new HashMap();
		map.put("printOn", "PrintWriter");
		map.put("DiskManager.fluidSpace", "Array");

		METHODS = Collections.unmodifiableMap(map);
	}

	
	public void transform(JavaMethod javaMethod) {
		String shortName = javaMethod.name;
		String fullName = javaMethod.javaClass.className+"."+shortName;
		if (!javaMethod.parameters.isEmpty() && (METHODS.containsKey(shortName) || METHODS.containsKey(fullName))) {
			JavaField field =  (JavaField)javaMethod.parameters.get(0);
			String type = (String)METHODS.get(fullName);
			if (type == null) {
				type = (String)METHODS.get(shortName);
			}
			field.type = type;
			javaMethod.javaClass.includeImportForType(type);
			
		}
	}

}