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

public class DeprecatedMethods implements MethodTransformation {

	private static final List METHODS;
	static {
		List list = new ArrayList();
		list.add("Pair.fetchLeft"); 
		list.add("Pair.fetchRight"); 
		list.add("Pair.pairWithNulls"); 
		METHODS = Collections.unmodifiableList(list);
	}
	
	public void transform(JavaMethod javaMethod) {
		String name = javaMethod.name;
		String nameWithClass = javaMethod.javaClass.className+"."+name;
		if (METHODS.contains(nameWithClass) || METHODS.contains(name)) {
			javaMethod.isDeprecated = true;
		}
	}

}