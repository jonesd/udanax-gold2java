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
import info.dgjones.abora.ug2java.javatoken.JavaBlockStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaStatementTerminator;
import info.dgjones.abora.ug2java.javatoken.JavaToken;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;
import info.dgjones.abora.ug2java.util.NameSupport;



public class TransformDefineFluid extends AbstractMethodBodyTransformation {
	

	public TransformDefineFluid() {
		super();
	}
	public TransformDefineFluid(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.token(JavaCallStart.class, "defineFluid"),
				factory.token(JavaIdentifier.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier identifier = (JavaIdentifier)tokens.get(i);
		JavaIdentifier name = (JavaIdentifier)tokens.get(i+2);
		int blockStart = javaMethod.methodBody.findNextTokenOfType(i+2, JavaBlockStart.class);
		int blockEnd = javaMethod.methodBody.findEndOfBlock(blockStart);
		int callEnd = javaMethod.methodBody.findClosingCallEnd(i+1);
		if (blockEnd > callEnd) {
			System.out.println("--Failed JavaBlock match for defineFluid");
			return i;
		}
		// Support primitive (in this case boolean literal) to object
		if (blockEnd == blockStart + 3) {
			JavaToken blockToken = (JavaToken)tokens.get(blockStart+1);
			if ("true".equals(blockToken.value) || "false".equals(blockToken.value)) {
				tokens.add(blockStart+1, new JavaIdentifier("Boolean"));
				blockToken.value = blockToken.value.toUpperCase();
				blockEnd += 1;
			}
		}
		
		tokens.remove(blockEnd);
		if (tokens.get(blockEnd-1) instanceof JavaStatementTerminator) {
			tokens.remove(blockEnd - 1);
		}
		tokens.remove(blockStart);
		tokens.add(i+2, new JavaIdentifier(identifier.value));
		//TODO why this embedded class
		tokens.add(i+3, new JavaIdentifier("class"));
		tokens.add(i+4, new JavaCallArgumentSeparator());
		identifier.value = "AboraSupport";
		
		name.value = "\"" + NameSupport.idToString(name.value) + "\"";
			
		return i;
	}
}
