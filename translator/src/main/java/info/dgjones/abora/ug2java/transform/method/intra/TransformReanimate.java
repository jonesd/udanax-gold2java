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
import info.dgjones.abora.ug2java.javatoken.JavaBlockEnd;
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



public class TransformReanimate extends AbstractMethodBodyTransformation {

	public TransformReanimate() {
		super();
	}
	public TransformReanimate(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallKeywordStart.class, "reanimate"), 
				factory.token(JavaBlockStart.class),
				factory.token(JavaType.class),
				factory.token(JavaIdentifier.class),
				factory.token(JavaStatementTerminator.class)
			);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaType tempType = (JavaType)tokens.get(i+2);
		JavaIdentifier tempName = (JavaIdentifier)tokens.get(i+3);
		
		int expressionStart = javaMethod.methodBody.findStartOfExpression(i-1);
		int blockEnd = javaMethod.methodBody.findEndOfBlock(i+1);
		//TODO should check to ensure only one statement within this block
		
		javaMethod.methodBody.shouldMatch(blockEnd+1, JavaCallEnd.class);
		javaMethod.methodBody.shouldMatch(blockEnd+2, JavaStatementTerminator.class);
		
		tokens.remove(blockEnd+2);
		tokens.remove(blockEnd+1);
		
		tokens.add(blockEnd+1, new JavaKeyword("finally"));
		tokens.add(blockEnd+2, new JavaBlockStart());
		tokens.add(blockEnd+3, new JavaIdentifier("AboraBlockSupport"));
		tokens.add(blockEnd+4, new JavaCallStart("exitRecorderFossilReanimate"));
		tokens.add(blockEnd+5, new JavaCallEnd());
		tokens.add(blockEnd+6, new JavaStatementTerminator());
		tokens.add(blockEnd+7, new JavaBlockEnd());

		javaMethod.methodBody.removeShouldMatch(i+4, JavaStatementTerminator.class);
		javaMethod.methodBody.removeShouldMatch(i+3, JavaIdentifier.class);
		javaMethod.methodBody.removeShouldMatch(i+2, JavaType.class);
		javaMethod.methodBody.removeShouldMatch(i, JavaCallStart.class, "reanimate");
		
		tokens.add(i, new JavaCallEnd());
		tokens.add(i+1, new JavaStatementTerminator());
		tokens.add(i+2, new JavaKeyword("try"));
		
		tokens.add(expressionStart, new JavaType(tempType.value));
		tokens.add(expressionStart+1, new JavaIdentifier(tempName.value));
		tokens.add(expressionStart+2, new JavaAssignment());
		tokens.add(expressionStart+3, new JavaIdentifier("AboraBlockSupport"));
		tokens.add(expressionStart+4, new JavaCallKeywordStart("enterRecorderFossilReanimate"));
		
		return i;
	}
}
