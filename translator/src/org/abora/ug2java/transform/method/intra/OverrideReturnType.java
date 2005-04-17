/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import org.abora.ug2java.ClassParser;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.transform.method.MethodTransformation;

public class OverrideReturnType implements MethodTransformation {

	public void transform(JavaMethod javaMethod) {
		String methodName = javaMethod.name;
		String methodNameWithClass = javaMethod.javaClass.className+"."+methodName;
		String returnType = javaMethod.returnType;
		if (ClassParser.OVERRIDE_RETURN_TYPE.containsKey(methodNameWithClass)) {
			returnType = (String) ClassParser.OVERRIDE_RETURN_TYPE.get(methodNameWithClass);
		} else if (returnType.equals("void") && ClassParser.OVERRIDE_VOID_RETURN_TYPE.containsKey(methodNameWithClass)) {
			returnType = (String) ClassParser.OVERRIDE_VOID_RETURN_TYPE.get(methodNameWithClass);
			//TODOreturnType = lookupType(returnType);
		} else if (returnType.equals("void") && ClassParser.OVERRIDE_VOID_RETURN_TYPE_WITH_CLASS.contains(methodNameWithClass)) {
			returnType = javaMethod.javaClass.className;
		} else if (ClassParser.OVERRIDE_RETURN_TYPE.containsKey(methodName)) {
			returnType = (String) ClassParser.OVERRIDE_RETURN_TYPE.get(methodName);
			//TODOreturnType = lookupType(returnType);
		} else if (returnType.equals("void") && ClassParser.OVERRIDE_VOID_RETURN_TYPE.containsKey(methodName)) {
			returnType = (String) ClassParser.OVERRIDE_VOID_RETURN_TYPE.get(methodName);
			//TODOreturnType = lookupType(returnType);
		} else if (returnType.equals("void") && ClassParser.OVERRIDE_VOID_RETURN_TYPE_WITH_CLASS.contains(methodName)) {
			returnType = javaMethod.javaClass.className;
		}
		javaMethod.returnType = returnType;
	}

}