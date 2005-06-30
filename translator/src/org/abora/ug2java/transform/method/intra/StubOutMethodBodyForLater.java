package org.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaStatementTerminator;
import org.abora.ug2java.javatoken.StringLiteral;
import org.abora.ug2java.transform.method.MethodTransformation;

public class StubOutMethodBodyForLater implements MethodTransformation {

	private static final Set METHODS;
	static {
		Set set = new HashSet();
		set.add("CoordinateSpace.verify");
		set.add("GenericCrossSpace.makeRcvr");
		set.add("Recipe.staticTimeNonInherited");
		METHODS = Collections.unmodifiableSet(set);
	}

	public void transform(JavaMethod javaMethod) {
		String fullName = javaMethod.javaClass.className + "." + javaMethod.name;
		if (METHODS.contains(fullName)) {
			replaceBodyWithThrow(javaMethod);
		}
	}

	private void replaceBodyWithThrow(JavaMethod method) {
		List tokens = new ArrayList();
		tokens.add(new JavaComment("Transform: Convert code later"));
		tokens.add(new JavaKeyword("throw"));
		tokens.add(new JavaKeyword("new"));
		tokens.add(new JavaCallKeywordStart("UnsupportedOperationException"));
		tokens.add(new StringLiteral("Implement later"));
		tokens.add(new JavaCallEnd());
		tokens.add(new JavaStatementTerminator());

		method.methodBody = new MethodBody(tokens);

	}
}