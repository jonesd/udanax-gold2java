/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.abora.ug2java.JavaMethod;

public class ExcludeMethods implements MethodTransformation {

	private static final List REMOVE;
	static {
		List list = new ArrayList();
		list.add("IntegerPos.IntegerVar");
		list.add("IntegerPos.basicCast");
		list.add("MuTable.test");

		REMOVE = Collections.unmodifiableList(list);
	}

	
	public void transform(JavaMethod javaMethod) {
		String shortName = javaMethod.name;
		String fullName = javaMethod.javaClass.className+"."+shortName;
		if (REMOVE.contains(shortName) || REMOVE.contains(fullName)) {
			javaMethod.shouldInclude = false;
		}
	}

}