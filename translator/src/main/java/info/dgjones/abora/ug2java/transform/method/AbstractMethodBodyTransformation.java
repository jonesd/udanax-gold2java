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
package info.dgjones.abora.ug2java.transform.method;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.MethodBody;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public abstract class AbstractMethodBodyTransformation implements MethodTransformation {

	private final TokenMatcher tokenMatcher;
	
	public AbstractMethodBodyTransformation() {
		this(new TokenMatcherFactory());
	}
	
	public AbstractMethodBodyTransformation(TokenMatcherFactory factory) {
		tokenMatcher = matchers(factory);
	}
	
	public void transform(JavaMethod javaMethod) {
		MethodBody methodBody = javaMethod.methodBody;
		List methodBodyTokens = methodBody.tokens;
		for (int i = 0; i < methodBodyTokens.size(); i++) {
			if (tokenMatcher.doesMatch(methodBodyTokens, i)) {
				int nextI = transform(javaMethod, methodBodyTokens, i);
				i = nextI;
			}
		}
	}
	
	protected abstract TokenMatcher matchers(TokenMatcherFactory factory);
	protected abstract int transform(JavaMethod javaMethod, List methodBodyTokens, int indexOfMatch);

	protected String regularExpressionOr(Collection c1, Collection c2) {
		//TODO insanity for c1.copyWithAll(c2)...
		Object[] array1 = c1.toArray();
		Object[] array2 = c2.toArray();
		Object[] array12 = new Object[array1.length + array2.length];
		System.arraycopy(array1, 0, array12, 0, array1.length);
		System.arraycopy(array2, 0, array12, array1.length, array2.length);
		return regularExpressionOr(Arrays.asList(array12));
	}

	protected String regularExpressionOrTrailing(Collection c) {
		List trailing = new ArrayList();
		for (Iterator iter = c.iterator(); iter.hasNext();) {
			String s = (String) iter.next();
			int lastSplitter = s.lastIndexOf('.');
			trailing.add(s.substring(lastSplitter+1));
		}
		return regularExpressionOr(trailing);
	}

	protected String regularExpressionOr(Collection c) {
		StringBuffer regularExpression = new StringBuffer();
		Iterator iterator = c.iterator();
		while (iterator.hasNext()) {
			regularExpression.append((String)iterator.next());
			if (iterator.hasNext()) {
				regularExpression.append("|");
			}
		}
		return regularExpression.toString();
	}

}
