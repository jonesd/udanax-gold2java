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

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.transform.method.MethodTransformation;

public class RemoveTrailingReturn implements MethodTransformation {

	private static final Set METHODS;
	static {
		Set set = new HashSet();
		set.add("Recipe.isEqual");
		set.add("SpecialistRcvr.basicReceive");

		METHODS = Collections.unmodifiableSet(set);
	}

	
	public void transform(JavaMethod javaMethod) {
		String shortName = javaMethod.name;
		String fullName = javaMethod.javaClass.className+"."+shortName;
		String parameterName = javaMethod.getQualifiedSignature();
		if (!javaMethod.parameters.isEmpty() && (METHODS.contains(shortName) || METHODS.contains(fullName) || METHODS.contains(parameterName))) {
			int size = javaMethod.methodBody.tokens.size();
			javaMethod.methodBody.removeShouldMatch(size-1, JavaStatementTerminator.class);
			javaMethod.methodBody.removeShouldMatch(size-2, JavaIdentifier.class);
			javaMethod.methodBody.removeShouldMatch(size-3, JavaKeyword.class, "return");
		}
	}

}