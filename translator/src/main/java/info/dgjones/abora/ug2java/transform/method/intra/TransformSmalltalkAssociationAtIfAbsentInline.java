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

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaBlockStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformSmalltalkAssociationAtIfAbsentInline extends AbstractMethodBodyTransformation {

	private static final Set ONLY_METHODS;
	static {
		Set set = new HashSet();
		set.add("Recipe.staticTimeNonInherited");

		//TODO test case only
		set.add("Test.testSmalltalkAssociationAtIfAbsentInline");

		ONLY_METHODS = Collections.unmodifiableSet(set);
	}

	public TransformSmalltalkAssociationAtIfAbsentInline() {
		super();
	}
	public TransformSmalltalkAssociationAtIfAbsentInline(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class, "Smalltalk"),
				factory.token(JavaCallKeywordStart.class, "associationAtIfAbsent")
				);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!(ONLY_METHODS.contains(javaMethod.getName()) || ONLY_METHODS.contains(javaMethod.getQualifiedName()))) {
			return i;
		}
		
		List argStarts = javaMethod.methodBody.extractCallArgStarts(i+1);
		int arg1Start = ((Integer)argStarts.get(1)).intValue();
		javaMethod.methodBody.shouldMatch(arg1Start, JavaBlockStart.class);
		int blockEnd = javaMethod.methodBody.findEndOfBlock(arg1Start);
		
		tokens.remove(blockEnd);
		if (tokens.get(blockEnd-1) instanceof JavaStatementTerminator) {
			tokens.remove(blockEnd - 1);
		}
		tokens.remove(arg1Start);
		
		return i;

	}
}
