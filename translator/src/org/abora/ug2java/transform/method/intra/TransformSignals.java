/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.abora.ug2java.Annotation;
import org.abora.ug2java.ClassParser;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



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
				factory.token(JavaIdentifier.class)
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
			String problem = javaIdentifier.value;
			//TODO earlier parsing bug for #(Blah)
			if (problem.startsWith("(")) {
				problem = problem.substring(1);
			}
			//TODO further #(Blah Again) parsing issues
			if (javaIdentifier instanceof JavaCallStart) {
				//skip callEnd
				j += 1;
				problem = ClassParser.transformSmalltalkSymbolToJava(problem);
			}
			signals.add(problem);
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
