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
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformCharacterCalls extends AbstractMethodBodyTransformation {

	private static final Set CALLS;
	static {
		Set set = new HashSet();
		set.add("isSeparator");
		CALLS = Collections.unmodifiableSet(set);
	}
	
	public TransformCharacterCalls() {
		super();
	}
	public TransformCharacterCalls(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
			factory.token(JavaCallStart.class, this.regularExpressionOr(CALLS)),
			factory.token(JavaCallEnd.class)
			);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier var = (JavaIdentifier)tokens.get(i);
		String varName = var.value;
		if (!"char".equals(javaMethod.findTypeOfVariable(varName))) {
			return i;
		}
		
		var.value = "AboraCharacterSupport";
		tokens.add(i+2, new JavaIdentifier(varName));
		return i;
	}
}
