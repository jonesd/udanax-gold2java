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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.transform.method.MethodTransformation;

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