package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformCreateCall extends MethodBodyTransformation {

	public TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.token(JavaCallStart.class, "create");
	}

	public void transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart)tokens.get(i);
		if (i > 0 && (tokens.get(i - 1) instanceof JavaIdentifier)) {
			JavaToken token = (JavaToken) tokens.get(i - 1);
			if (token.value.equals("super")) {
				return;
			}
			call.value = token.value;
			javaMethod.javaClass.includeImportForType(call.value);
			tokens.remove(i - 1);
			tokens.add(i - 1, new JavaKeyword("new"));
		} else {
			call.value = javaMethod.javaClass.className;
			tokens.add(i, new JavaKeyword("new"));
		}
	}
//	protected void transformCreateCall(MethodBody body) {
//	List tokens = body.tokens;
//	for (int i = 0; i < tokens.size(); i++) {
//		JavaToken call = (JavaToken) tokens.get(i);
//		if (call instanceof JavaCallStart && call.value.equals("create")) {
//			if (i > 0 && (tokens.get(i - 1) instanceof JavaIdentifier)) {
//				JavaToken token = (JavaToken) tokens.get(i - 1);
//				if (token.value.equals("super")) {
//					continue;
//				}
//				call.value = token.value;
//				javaClass.includeImportForType(call.value);
//				tokens.remove(i - 1);
//				tokens.add(i - 1, new JavaKeyword("new"));
//			} else {
//				call.value = javaClass.className;
//				tokens.add(i, new JavaKeyword("new"));
//			}
//		}
//	}
//}

}
