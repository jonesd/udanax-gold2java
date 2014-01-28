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

import java.util.List;

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCast;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisEnd;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisStart;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformOrglRootMake extends AbstractMethodBodyTransformation {

	public TransformOrglRootMake() {
		super();
	}
	public TransformOrglRootMake(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		// return makeScruTable(((ScruTable) it));
		return factory.seq(
				factory.token(JavaKeyword.class, "return"), 
				factory.token(JavaCallKeywordStart.class, "makeScruTable"),
				factory.token(JavaParenthesisStart.class),
				factory.token(JavaCast.class, "ScruTable"), 
				factory.token(JavaIdentifier.class, "it"),
				factory.token(JavaParenthesisEnd.class),
				factory.token(JavaCallEnd.class));
	}
	
	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (! (javaMethod.name.equals("make") || javaMethod.javaClass.className.equals("OrglRoot"))) {
			return i;
		}
		javaMethod.methodBody.remove(i, i+6+1);
		tokens.add(i, new JavaKeyword("throw"));
		tokens.add(i+1, new JavaKeyword("new"));
		tokens.add(i+2, new JavaCallKeywordStart("UnsupportedOperationException"));
		tokens.add(i+3, new JavaCallEnd());
		return i;
	}
}
