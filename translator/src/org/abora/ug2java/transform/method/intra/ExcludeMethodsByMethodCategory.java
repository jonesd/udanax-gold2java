/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.transform.method.MethodTransformation;



public class ExcludeMethodsByMethodCategory implements MethodTransformation {

	private static final Set CLASS_METHOD_CATEGORIES;
	static {
		Set set = new HashSet();
		set.add("Heaper.stubble PROXY");
		set.add("Heaper.locking");
		CLASS_METHOD_CATEGORIES = Collections.unmodifiableSet(set);
	}

	public void transform(JavaMethod javaMethod) {
		String fullCategory = javaMethod.javaClass.className+"."+javaMethod.methodCategory;
		if (javaMethod.isStatic() && CLASS_METHOD_CATEGORIES.contains(fullCategory)) {
			javaMethod.shouldInclude = false;
		}
	}

}
