/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.abora.ug2java.JavaField;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.transform.method.MethodTransformation;

public class ConvertToStaticBlocks implements MethodTransformation {

	private static final List STATIC_METHODS;
	static {
		List list = new ArrayList();
		list.add("initTimeNonInherited"); 

		STATIC_METHODS = Collections.unmodifiableList(list);
	}

	
	public void transform(JavaMethod javaMethod) {
		String shortName = javaMethod.name;
		String fullName = javaMethod.javaClass.className+"."+shortName;
		String parameterName = fullName+parameterDeclaration(javaMethod);
		if (javaMethod.isStatic() && (STATIC_METHODS.contains(shortName) || STATIC_METHODS.contains(fullName) || STATIC_METHODS.contains(parameterName))) {
			javaMethod.shouldInclude = false;
			//TODO nicer implementation
			javaMethod.javaClass.staticBlocks.add(javaMethod);
		}
	}
	
	private String parameterDeclaration(JavaMethod javaMethod) {
		StringBuffer buffer = new StringBuffer();
		buffer.append('(');
		for (Iterator iter = javaMethod.parameters.iterator(); iter.hasNext();) {
			JavaField parameter = (JavaField) iter.next();
			buffer.append(parameter.type);
			if(iter.hasNext()) {
				buffer.append(',');
			}
		}
		buffer.append(')');
		return buffer.toString();
	}

}