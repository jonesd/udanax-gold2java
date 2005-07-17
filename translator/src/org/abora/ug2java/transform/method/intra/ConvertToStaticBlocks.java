/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.transform.method.MethodTransformation;

public class ConvertToStaticBlocks implements MethodTransformation {

	private static final List STATIC_METHODS;
	static {
		List list = new ArrayList();
		list.add("initTimeNonInherited");
		list.add("linkTimeNonInherited");

		STATIC_METHODS = Collections.unmodifiableList(list);
	}

	private static final Set IGNORE_METHODS;
	static {
		Set set = new HashSet();
		set.add("Heaper.linkTimeNonInherited");
		set.add("StackExaminer.linkTimeNonInherited");
		IGNORE_METHODS = Collections.unmodifiableSet(set);
	}
	
	public void transform(JavaMethod javaMethod) {
		String shortName = javaMethod.name;
		String fullName = javaMethod.getQualifiedName();
		String parameterName = javaMethod.getQualifiedSignature();
		if (javaMethod.isStatic() && !IGNORE_METHODS.contains(fullName) && (STATIC_METHODS.contains(shortName) || STATIC_METHODS.contains(fullName) || STATIC_METHODS.contains(parameterName))) {
			javaMethod.javaClass.methods.remove(javaMethod);
			//TODO nicer implementation
			if (javaMethod.name.equals("linkTimeNonInherited")) {
				javaMethod.javaClass.addStaticBlockFirst(javaMethod);
			} else {
				javaMethod.javaClass.addStaticBlock(javaMethod);
			}
		}
	}
}