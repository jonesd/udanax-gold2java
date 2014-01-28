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
import info.dgjones.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCast;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaLiteral;
import info.dgjones.abora.ug2java.javatoken.StringLiteral;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformPointerToStaticMember extends AbstractMethodBodyTransformation {

	
public TransformPointerToStaticMember() {
		super();
	}
	public TransformPointerToStaticMember(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallKeywordStart.class, "pointerToStaticMember"),
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallArgumentSeparator.class),
				factory.token(JavaLiteral.class),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier member = (JavaIdentifier)tokens.get(i+2);
		JavaLiteral memberType = (JavaLiteral)tokens.get(i+4);
		//TODO
		if (!member.value.startsWith("\"")) {
			tokens.remove(i+2);
			tokens.add(i+2, new StringLiteral(member.value));
		}
		String castType = memberType.value;
		if (castType.startsWith("\"")) {
			castType = castType.substring(1, castType.length()-1);
		}
		tokens.add(i, new JavaCast(castType));
		return i;
	}
}
