package org.abora.ug2java.transform;

import java.util.Iterator;
import java.util.List;

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.transform.tokenmatcher.MatchAny;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformOverrideCalls extends MethodBodyTransformation {

	public TokenMatcher matchers(TokenMatcherFactory factory) {
		MatchAny matchAny = new MatchAny();
		//TODO could be more efficient by matching just once on type, then go through all names
		for (Iterator iter = JavaClass.OVERRIDE_CALLS.keySet().iterator(); iter.hasNext();) {
			String call = (String) iter.next();
			matchAny.add(factory.token(JavaCallStart.class, call));
		}	
		return matchAny;
	}

	public void transform(JavaMethod javaMethod, List tokens, int i) {
		JavaCallStart call = (JavaCallStart)tokens.get(i);
		call.value = (String) JavaClass.OVERRIDE_CALLS.get(call.value);
	}
}
