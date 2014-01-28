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
import info.dgjones.abora.ug2java.javatoken.JavaAssignment;
import info.dgjones.abora.ug2java.javatoken.JavaBlockStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaKeyword;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.javatoken.JavaType;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformTimeMillisecondsToRun extends AbstractMethodBodyTransformation {

	public TransformTimeMillisecondsToRun() {
		super();
	}
	public TransformTimeMillisecondsToRun(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaAssignment.class),
				factory.token(JavaIdentifier.class, "Time"),
				factory.token(JavaCallKeywordStart.class, "millisecondsToRun"), 
				factory.token(JavaBlockStart.class));
	}
	
	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier var = (JavaIdentifier)tokens.get(i);
		
		int start = javaMethod.methodBody.findStartOfExpression(i);
		int postCallEnd = javaMethod.methodBody.findClosingCallEnd(i+3);
		if (postCallEnd + 1 < tokens.size() && (tokens.get(postCallEnd + 1) instanceof JavaStatementTerminator)) {
			tokens.remove(postCallEnd + 1);
		}
		tokens.remove(postCallEnd);
		tokens.remove(postCallEnd-1);
		
		int j = postCallEnd-1;
		tokens.add(j++, new JavaIdentifier(var.value));
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new JavaIdentifier("System"));
		tokens.add(j++, new JavaCallStart("currentTimeMillis"));
		tokens.add(j++, new JavaCallEnd());
		tokens.add(j++, new JavaKeyword("-"));
		tokens.add(j++, new JavaIdentifier(var.value+"Start"));
		tokens.add(j++, new JavaStatementTerminator());
		
		javaMethod.methodBody.remove(i, i+5);

		j = i;
		tokens.add(j++, new JavaType("long"));
		tokens.add(j++, new JavaIdentifier(var.value+"Start"));
		tokens.add(j++, new JavaAssignment());
		tokens.add(j++, new JavaIdentifier("System"));
		tokens.add(j++, new JavaCallStart("currentTimeMillis"));
		tokens.add(j++, new JavaCallEnd());		
		tokens.add(j++, new JavaStatementTerminator());		
		
		return i;
	}
}
