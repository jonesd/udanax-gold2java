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
import java.util.Set;
import java.util.TreeSet;

import info.dgjones.abora.ug2java.Annotation;
import info.dgjones.abora.ug2java.ClassParser;
import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaArrayInitializerStart;
import info.dgjones.abora.ug2java.javatoken.JavaCallKeywordStart;
import info.dgjones.abora.ug2java.javatoken.JavaToken;
import info.dgjones.abora.ug2java.javatoken.StringLiteral;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformSignals extends AbstractMethodBodyTransformation {

	public TransformSignals() {
		super();
	}
	public TransformSignals(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallKeywordStart.class, "signals"),
				factory.token(JavaArrayInitializerStart.class)
				);
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (! ((i == 1 || i == 2) && ((JavaToken)tokens.get(0)).value.equals("return"))) {
			return i;
		}
		
		Set signals = (Set)javaMethod.getAnnotations().getIfNone(Annotation.PROBLEM_SIGNALS, new TreeSet());
		int endOfCall = javaMethod.methodBody.findClosingCallEnd(i);
		int j = i + 1;
		while (j < endOfCall) {
			JavaToken javaIdentifier = (JavaToken)tokens.get(j);
			if (javaIdentifier instanceof StringLiteral) {
				StringLiteral stringLiteral = (StringLiteral)javaIdentifier;
				String problem = ClassParser.transformSmalltalkSymbolToJava(stringLiteral.getStringValue());
				signals.add(problem);
			}
			j += 1;
		}
		javaMethod.getAnnotations().put(Annotation.PROBLEM_SIGNALS, signals);
		
		javaMethod.shouldInclude = false;
		
//		tokens.remove(i);
//		JavaCallKeywordStart signals = (JavaCallKeywordStart)tokens.get(i);
//		JavaIdentifier name = (JavaIdentifier)tokens.get(i+1);
//		tokens.add(i, new JavaKeyword("throw"));
//		tokens.add(i+1, new JavaKeyword("new"));
//		signals.value = ClassParser.ABORA_RUNTIME_EXCEPTION_CLASS;
//		//TODO parsing problem...
//		if (name.value.startsWith("(")) {
//			name.value = name.value.substring(1);
//		}
//		name.value = ClassParser.ABORA_RUNTIME_EXCEPTION_CLASS+"."+name.value;
//		javaMethod.javaClass.includeImportForType(ClassParser.ABORA_RUNTIME_EXCEPTION_CLASS);
		
		return i;
	}
}
