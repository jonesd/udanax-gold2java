package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformBitAndOrXor extends MethodBodyTransformation {

	public TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.any(
				factory.token(JavaCallKeywordStart.class, "bitAnd"), 
				factory.token(JavaCallKeywordStart.class, "bitOr"), 
				factory.token(JavaCallKeywordStart.class, "bitXor"));
	}

	public void transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallKeywordStart token = (JavaCallKeywordStart)tokens.get(i);
		int closingIndex = javaMethod.methodBody.findClosingCallEnd(i);
		tokens.remove(closingIndex);
		tokens.remove(i);
		if (token.value.equals("bitAnd")) {
			tokens.add(i, new JavaKeyword("&"));
		} else if (token.value.equals("bitOr")) {
			tokens.add(i, new JavaKeyword("|"));
		} else {
			tokens.add(i, new JavaKeyword("^"));
		}
	}
}
