package org.abora.ug2java.transform.type;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.abora.ug2java.Annotation;
import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaField;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
import org.abora.ug2java.SmalltalkSource;
import org.abora.ug2java.javatoken.JavaBlockEnd;
import org.abora.ug2java.javatoken.JavaBlockStart;
import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaCast;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.JavaKeyword;
import org.abora.ug2java.javatoken.JavaLiteral;
import org.abora.ug2java.javatoken.JavaParenthesisEnd;
import org.abora.ug2java.javatoken.JavaParenthesisStart;
import org.abora.ug2java.javatoken.JavaStatementTerminator;



public class AddAboraRequires implements ClassTransformer {

	public void transform(JavaClass javaClass) {
		if (javaClass.className.equals("CalcCreator")) {
			addRequires(javaClass, "Recipe");
			
		} else if (javaClass.className.equals("BootPlan")) {
			addRequires(javaClass, "Recipe");
			
		} else if (javaClass.className.equals("BackendBootMaker")) {
			addRequires(javaClass, "Recipe");

		}
	}

	private void addRequires(JavaClass javaClass, String requiredClassName) {
		JavaMethod javaMethod = javaClass.getMethod("initTimeNonInherited");
		
		//TODO next section duplicated from TransformRequires...
		Set required = (Set)javaMethod.getAnnotations().get(Annotation.REQUIRES);
		if (required == null) {
			required = new HashSet();
			javaMethod.getAnnotations().put(Annotation.REQUIRES, required);
		}

		required.add(requiredClassName);
	}
	
}
