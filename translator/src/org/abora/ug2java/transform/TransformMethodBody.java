/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.abora.ug2java.JavaMethod;



public class TransformMethodBody implements MethodTransformation {

	final private List transformers;
	
	
	public TransformMethodBody() {
		this(createTransformers());
	}
	
	public TransformMethodBody(List transformers) {
		this.transformers = transformers;
	}
	
	private static List createTransformers() {		
		List transformers = new ArrayList();
		transformers.add(new TransformAndOrs());
		transformers.add(new EnsureIfTestInParentheses());
		transformers.add(new EnsureReasonableStatementTermination());
		transformers.add(new TransformSelfSends());
		transformers.add(new TransformUses());
		transformers.add(new TransformSmalltalkOnly());
		transformers.add(new TransformTranslateOnlyString());
		transformers.add(new TransformSuperCreate());
		transformers.add(new TransformCreateCall());
		transformers.add(new TransformWhileTrue());
		transformers.add(new TransformTimesRepeat());
		transformers.add(new TransformCritical());
		transformers.add(new TransformValueNowOrOnOnUnwindDo());
		transformers.add(new TransformOverrideCalls());
		transformers.add(new TransformBitAndOrXor());
		transformers.add(new TransformBlast());
		transformers.add(new TransformIsKindOf());
		transformers.add(new TransformCast());
		transformers.add(new TransformAlmostTo());
		transformers.add(new TransformSubclassResponsibility());
		transformers.add(new TransformDOTCalls());
		transformers.add(new TransformCAThashForEqual());
		transformers.add(new TransformIntegerIntegerVar());
		transformers.add(new TransformPrint());
		transformers.add(new TransformStaticCall());
		transformers.add(new TransformReturnVoid());
		transformers.add(new TransformClassReference());
		transformers.add(new TransformUnreachableCode());
		return Collections.unmodifiableList(transformers);
	}
	
	public void transform(JavaMethod javaMethod) {
		for (Iterator iter = transformers.iterator(); iter.hasNext();) {
			MethodTransformation transformation = (MethodTransformation) iter.next();
			transformation.transform(javaMethod);
		}
	}

}
