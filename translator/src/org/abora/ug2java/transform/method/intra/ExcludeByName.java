/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.transform.method.MethodTransformation;

public class ExcludeByName implements MethodTransformation {

	private static final List EXCLUDE_NAMES;
	static {
		List list = new ArrayList();
		list.add("ActualArray.search"); // seemes to be smalltalk only code - not int aware
		list.add("XnRegion.dox"); // smalltalk: special
		list.add("ScruTable.dox"); // smalltalk: special
		list.add("ScruTable.asOrderedCollection"); // smalltalk: special
		EXCLUDE_NAMES = Collections.unmodifiableList(list);
	}
	
	public void transform(JavaMethod javaMethod) {
		String name = javaMethod.name;
		String nameWithClass = javaMethod.javaClass.className+"."+name;
		if (EXCLUDE_NAMES.contains(nameWithClass) || EXCLUDE_NAMES.contains(name)) {
			javaMethod.shouldInclude = false;
		}
	}

}