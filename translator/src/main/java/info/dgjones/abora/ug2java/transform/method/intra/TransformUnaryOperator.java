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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaCast;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaToken;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;

public class TransformUnaryOperator extends AbstractMethodBodyTransformation {

	private static final Map operators;
	static {
		Map map = new HashMap();
		map.put("not", "!");
		map.put("bitInvert", "~");
		map.put("negated", "-");
		operators = Collections.unmodifiableMap(map);
	}

	private static final Map operatorCasts;
	static {
		Map map = new HashMap();
		map.put("asDouble", "double");
		map.put("asFloat", "float");
		map.put("asInteger", "int");
		operatorCasts = Collections.unmodifiableMap(map);
	}

	public TransformUnaryOperator() {
		super();
	}

	public TransformUnaryOperator(TokenMatcherFactory factory) {
		super(factory);
	}
	
	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallStart.class, regularExpressionOr(operators.keySet(), operatorCasts.keySet())),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		String smalltalkCall = ((JavaCallStart)tokens.get(i)).value;
		int expressionStart = javaMethod.methodBody.findStartOfExpressionMinimal(i-1);
		tokens.remove(i + 1);
		tokens.remove(i);
		JavaToken operator;
		String match = (String)operators.get(smalltalkCall);
		if (match != null) {
			operator = new JavaKeyword(match);
		} else {
			match = (String)operatorCasts.get(smalltalkCall);
			operator = new JavaCast(match);
		}
		tokens.add(expressionStart, operator);
		
		return i;
	}
}