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
import java.util.List;
import java.util.Set;

import info.dgjones.abora.ug2java.ClassParser;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaAssignment;
import info.dgjones.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformRecipeReference extends AbstractMethodBodyTransformation {

	//TODO review this deference stuff...
	private static final Set DEREFERENCE_METHODS;
	static {
		Set set = new HashSet();
		set.add("BootPlan.initTimeNonInherited");
		set.add("BackendBootMaker.initTimeNonInherited");
		set.add("CalcCreator.initTimeNonInherited");
		
		//TODO test case
		set.add("Test.recipeDereference");
		DEREFERENCE_METHODS = Collections.unmodifiableSet(set);
	}

	public TransformRecipeReference() {
		super();
	}
	public TransformRecipeReference(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaIdentifier.class, "BootCuisine|CalcCuisine|DiskCuisine|FebeCuisine|XppCuisine");
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		boolean shouldDeference = DEREFERENCE_METHODS.contains(javaMethod.getName()) || DEREFERENCE_METHODS.contains(javaMethod.getQualifiedName());
		
		JavaIdentifier identifier = (JavaIdentifier)tokens.get(i);
		tokens.remove(i);
		
		boolean isAssignment = tokens.get(i) instanceof JavaAssignment; 
				
		int j = i;
		tokens.add(j++, new JavaIdentifier("Smalltalk"));
		tokens.add(j++, new JavaCallKeywordStart(isAssignment ? "atPut" : "associationAt"));
		tokens.add(j++, new JavaIdentifier(ClassParser.transformSmalltalkSymbolToJava(identifier.value)));
		
		if (isAssignment) {
			tokens.remove(j);
			tokens.add(j++, new JavaCallArgumentSeparator());
			j = javaMethod.methodBody.findEndOfExpression(j);
			j++;
		} 
		tokens.add(j++, new JavaCallEnd());
		if (shouldDeference) {
			tokens.add(j++, new JavaCallStart("refValue"));
			tokens.add(j++, new JavaCallEnd());
		}
		
		return i-1;
	}
}
