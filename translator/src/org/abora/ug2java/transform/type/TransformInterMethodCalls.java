package org.abora.ug2java.transform.type;

import java.util.Iterator;

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.transform.method.MethodTransformation;
import org.abora.ug2java.transform.method.inter.TransformInterMethod;



public class TransformInterMethodCalls implements ClassTransformer {

	public void transform(JavaClass javaClass) {
		MethodTransformation transformInterMethod = new TransformInterMethod();
		for (Iterator iter = javaClass.methods.iterator(); iter.hasNext();) {
			JavaMethod javaMethod = (JavaMethod) iter.next();
			transformInterMethod.transform(javaMethod);
		}
	}
}
