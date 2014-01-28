/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.ug2java.transform.method.intra;

import info.dgjones.abora.ug2java.ClassParser;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.transform.method.MethodTransformation;

public class OverrideReturnType implements MethodTransformation {

	public void transform(JavaMethod javaMethod) {
		String methodName = javaMethod.name;
		String methodParameters = javaMethod.getSignature();
		String methodNameWithClass = javaMethod.getQualifiedName();
		String methodSignature = javaMethod.getQualifiedSignature();
		String returnType = javaMethod.returnType;
		if (ClassParser.OVERRIDE_RETURN_TYPE.containsKey(methodSignature)) {
			returnType = (String) ClassParser.OVERRIDE_RETURN_TYPE.get(methodSignature);
		} else if (ClassParser.OVERRIDE_RETURN_TYPE.containsKey(methodNameWithClass)) {
			returnType = (String) ClassParser.OVERRIDE_RETURN_TYPE.get(methodNameWithClass);
		} else if (returnType.equals("void") && ClassParser.OVERRIDE_VOID_RETURN_TYPE.containsKey(methodSignature)) {
			returnType = (String) ClassParser.OVERRIDE_VOID_RETURN_TYPE.get(methodSignature);
		} else if (returnType.equals("void") && ClassParser.OVERRIDE_VOID_RETURN_TYPE.containsKey(methodNameWithClass)) {
			returnType = (String) ClassParser.OVERRIDE_VOID_RETURN_TYPE.get(methodNameWithClass);
		} else if (returnType.equals("void") && ClassParser.OVERRIDE_VOID_RETURN_TYPE_WITH_CLASS.contains(methodSignature)) {
			returnType = javaMethod.javaClass.className;
		} else if (returnType.equals("void") && ClassParser.OVERRIDE_VOID_RETURN_TYPE_WITH_CLASS.contains(methodNameWithClass)) {
			returnType = javaMethod.javaClass.className;
		} else if (ClassParser.OVERRIDE_RETURN_TYPE.containsKey(methodParameters)) {
			returnType = (String) ClassParser.OVERRIDE_RETURN_TYPE.get(methodParameters);
		} else if (returnType.equals("void") && ClassParser.OVERRIDE_VOID_RETURN_TYPE.containsKey(methodParameters)) {
			returnType = (String) ClassParser.OVERRIDE_VOID_RETURN_TYPE.get(methodParameters);
		} else if (returnType.equals("void") && ClassParser.OVERRIDE_VOID_RETURN_TYPE_WITH_CLASS.contains(methodParameters)) {
			returnType = javaMethod.javaClass.className;
		} else if (ClassParser.OVERRIDE_RETURN_TYPE.containsKey(methodName)) {
			returnType = (String) ClassParser.OVERRIDE_RETURN_TYPE.get(methodName);
		} else if (returnType.equals("void") && ClassParser.OVERRIDE_VOID_RETURN_TYPE.containsKey(methodName)) {
			returnType = (String) ClassParser.OVERRIDE_VOID_RETURN_TYPE.get(methodName);
		} else if (returnType.equals("void") && ClassParser.OVERRIDE_VOID_RETURN_TYPE_WITH_CLASS.contains(methodName)) {
			returnType = javaMethod.javaClass.className;
		}
		javaMethod.returnType = returnType;
	}

}