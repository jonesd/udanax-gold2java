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
import info.dgjones.abora.ug2java.javatoken.JavaBlockEnd;
import info.dgjones.abora.ug2java.javatoken.JavaComment;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.javatoken.JavaToken;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformCompilerFodder extends AbstractMethodBodyTransformation {
	/**
	 * Methods that are too complicated for the current simple code to analyse properly.
	 * Any methods matched against this should be left as is.
	 */
	private static final Set ignoreMethods;
	static {
		Set set = new HashSet();
		set.add("ResultRecorderPFinder.shouldTrigger");
		
		//TODO for tests only
		set.add("Test.testUnreachableCodeIgnore");
		ignoreMethods = Collections.unmodifiableSet(set);
	}
	


	public TransformCompilerFodder() {
		super();
	}
	public TransformCompilerFodder(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaBlockEnd.class),
				factory.token(JavaKeyword.class, "return"),
				factory.token(JavaToken.class),
				factory.token(JavaStatementTerminator.class),
				factory.any(
						factory.token(JavaComment.class, "compiler fodder"),
						factory.token(JavaComment.class, "Compiler fodder"),
						factory.token(JavaComment.class, "fodder")
				));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		String shortName = javaMethod.name;
		String fullName = javaMethod.javaClass.className+"."+shortName;
		if (ignoreMethods.contains(fullName) || ignoreMethods.contains(shortName)) {
			return i;
		}

		int blockStart = javaMethod.methodBody.findStartOfBlock(i);
		JavaToken preBlock = (JavaToken)tokens.get(blockStart - 1);
		if (preBlock instanceof JavaKeyword && ("else".equals(preBlock.value) || "finally".equals(preBlock.value))) {
			// Can only trim out the return if this return wont be called.
			// This is just a quick approximate check
			tokens.remove(i+4);
			tokens.remove(i+3);
			tokens.remove(i+2);
			tokens.remove(i+1);
		}
		return i;
	}
}
