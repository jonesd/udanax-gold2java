package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformStaticCall extends MethodBodyTransformation {

	public TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallStart.class, "class"), 
				factory.token(JavaCallEnd.class),
				factory.token(JavaCallStart.class));
	}

	public void transform(JavaMethod javaMethod, List tokens, int i) {
		tokens.add(i, new JavaIdentifier(javaMethod.javaClass.className));
		tokens.remove(i + 1);
		tokens.remove(i + 1);
	}
//	protected void transformStaticCall(MethodBody body) {
//	List tokens = body.tokens;
//	for (int i = 0; i < tokens.size() - 2; i++) {
//		JavaToken call = (JavaToken) tokens.get(i);
//		if (call instanceof JavaCallStart && call.value.equals("class")) {
//			JavaToken callEnd = (JavaToken) tokens.get(i + 1);
//			if (callEnd instanceof JavaCallEnd) {
//				JavaToken nextCall = (JavaToken) tokens.get(i + 2);
//				if (nextCall instanceof JavaCallStart) {
//					tokens.add(i, new JavaIdentifier(javaClass.className));
//					tokens.remove(i + 1);
//					tokens.remove(i + 1);
//				}
//			}
//		}
//	}
//}

}
