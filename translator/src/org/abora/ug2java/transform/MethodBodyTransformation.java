package org.abora.ug2java.transform;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public abstract class MethodBodyTransformation {

	public void transform(JavaMethod javaMethod) {
		MethodBody methodBody = javaMethod.methodBody;
		List methodBodyTokens = methodBody.tokens;
		TokenMatcher matcher = matchers(new TokenMatcherFactory());
		for (int i = 0; i < methodBodyTokens.size(); i++) {
			if (matcher.doesMatch(methodBodyTokens, i)) {
				transform(javaMethod, methodBodyTokens, i);
			}
		}
	}
	
	public abstract TokenMatcher matchers(TokenMatcherFactory factory);
	public abstract void transform(JavaMethod javaMethod, List methodBodyTokens, int indexOfMatch);
}
