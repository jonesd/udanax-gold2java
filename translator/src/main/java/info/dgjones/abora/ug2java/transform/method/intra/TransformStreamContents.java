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
import info.dgjones.abora.ug2java.util.ClassHelper;



public class TransformStreamContents extends AbstractMethodBodyTransformation {


	public TransformStreamContents() {
		super();
	}
	public TransformStreamContents(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
//				factory.token(JavaKeyword.class, "return"),
				factory.token(JavaIdentifier.class, "String"),
				factory.token(JavaCallKeywordStart.class, "streamContents"),
				factory.token(JavaBlockStart.class),
				factory.token(JavaType.class, "Object"),
				factory.token(JavaIdentifier.class),
				factory.token(JavaStatementTerminator.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		int statementStart = javaMethod.methodBody.findStartOfStatement(i-1);
		int blockEnd = javaMethod.methodBody.findEndOfBlockQuietFail(i+2);
		if (blockEnd == -1) {
			System.out.println("--Failed to find end of block for:"+ClassHelper.getShortName(this.getClass())+" method:"+javaMethod.getQualifiedSignature());
			return i;
		}
		tokens.remove(blockEnd+1);
		tokens.remove(blockEnd);
		tokens.remove(i+3);
		tokens.remove(i+2);
		tokens.remove(i+1);
		tokens.remove(i+0);
		tokens.add(i, new JavaType("StringWriter"));
		tokens.add(i+1, new JavaIdentifier("stringWriter"));
		tokens.add(i+2, new JavaAssignment());
		tokens.add(i+3, new JavaKeyword("new"));
		tokens.add(i+4, new JavaCallStart("StringWriter"));
		tokens.add(i+5, new JavaCallEnd());
		tokens.add(i+6, new JavaStatementTerminator());
		tokens.add(i+7, new JavaType("PrintWriter"));
		tokens.add(i+9, new JavaAssignment());
		tokens.add(i+10, new JavaKeyword("new"));
		tokens.add(i+11, new JavaCallKeywordStart("PrintWriter"));
		tokens.add(i+12, new JavaIdentifier("stringWriter"));
		tokens.add(i+13, new JavaCallEnd());
		int newEnd = blockEnd+13-4;
		tokens.add(newEnd+0, new JavaIdentifier("stringWriter"));
		tokens.add(newEnd+1, new JavaCallStart("toString"));
		tokens.add(newEnd+2, new JavaCallEnd());
		
		javaMethod.methodBody.copy(statementStart, i, newEnd);
		javaMethod.methodBody.remove(statementStart, i);
		
		return i;
	}
}
