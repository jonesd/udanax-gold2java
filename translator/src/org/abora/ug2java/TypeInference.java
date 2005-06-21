package org.abora.ug2java;

import java.util.List;

import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaToken;



public class TypeInference {

	private final JavaMethod method;
	
	public TypeInference(JavaMethod method) {
		super();
		this.method = method;
	}
	
	public String inferType(MethodBody methodBody, int start) {
		String type = null;
		List tokens = methodBody.tokens;
		int i = start;
		while (i < tokens.size()) {
			JavaToken token = (JavaToken)tokens.get(i++);
			if (token instanceof JavaIdentifier) {
				type = inferTypeOfIdentifier(token.value);
				//TODO
//			} else if (token instanceof JavaLiteral) {
//				type = 
			}
			
		}
		return type;
	}
	
	public String inferTypeOfIdentifier(String identifier) {
		if (method.getJavaCodebase().isPrimitiveType(identifier)) {
			return identifier;
		}
		JavaClass type = method.getJavaCodebase().getJavaClass(identifier);
		if (type != null) {
			return type.className;
		}
		return method.findTypeOfVariable(identifier);
	}
}
