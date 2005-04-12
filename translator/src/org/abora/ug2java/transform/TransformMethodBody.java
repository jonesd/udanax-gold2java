
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
import org.abora.ug2java.javatoken.JavaIdentifier;



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
		transformers.add(new OverrideReturnType());
		transformers.add(new OverrideArgumentType());
		transformers.add(new TransformAndOrs());
		transformers.add(new EnsureIfTestInParentheses());
		transformers.add(new EnsureReasonableStatementTermination());
		transformers.add(new ExcludeByName());
		transformers.add(new ExcludeStaticHeaper());
		transformers.add(new ExcludeMethods());
		transformers.add(new DeprecatedMethods());
		transformers.add(new EnsureUniqueLocalVarNames());
		
		transformers.add(new TransformSelfSends());
		transformers.add(new TransformUses());
//		transformers.add(new TransformSmalltalkOnly());
		transformers.add(new TransformTranslateOnlyString());
		transformers.add(new TransformNewCreateCallOnClass());
		transformers.add(new TransformSuperCreate());
		transformers.add(new TransformCreateCall());
		transformers.add(new TransformCerr());
		transformers.add(new TransformXuTime());
		transformers.add(new TransformWhileTrue());
		transformers.add(new TransformTimesRepeat());
		transformers.add(new TransformCritical());
		transformers.add(new TransformValueNowOrOnOnUnwindDo());
		transformers.add(new TransformAtCalls());
		transformers.add(new TransformIntegerZero());
		transformers.add(new TransformBlast());
		transformers.add(new TransformIsKindOf());
		transformers.add(new TransformStar());
		transformers.add(new TransformCast());
		transformers.add(new TransformAlmostTo());
		transformers.add(new TransformSubclassResponsibility());
		transformers.add(new TransformDOTCalls());
		transformers.add(new TransformDOThashForEqual());
		transformers.add(new TransformCAThashForEqual());
		transformers.add(new TransformIntegerIntegerVar());
		transformers.add(new TransformPrint());
		transformers.add(new TransformBitAndOrXor());
		transformers.add(new TransformDiskManagerConsistent());
		transformers.add(new TransformCategoryName());
		transformers.add(new TransformReceiverReceiveHeaper());
		transformers.add(new TransformIntegerCall());
		transformers.add(new TransformSignals());
		transformers.add(new TransformShouldImplement());
		transformers.add(new TransformStrcmp());
		transformers.add(new TransformStrlen());
		transformers.add(new TransformMinMax());
		transformers.add(new TransformNewCreate());
		transformers.add(new TransformNewCall());
		transformers.add(new TransformFluidAccess());
		transformers.add(new TransformFluidBindDuring());
		transformers.add(new TransformDefineFluid());
		transformers.add(new TransformReanimate());
		transformers.add(new TransformUnaryOperator());
		transformers.add(new TransformAssert());
		transformers.add(new TransformConditionalOperator());
		transformers.add(new TransformCastIntoOthers());
		transformers.add(new TransformNotNULLElse());		
		transformers.add(new TransformUnimplemented());
		transformers.add(new TransformPasse());
		transformers.add(new ChooseTransformOnly());
				
		transformers.add(new TransformStaticCall());
		transformers.add(new TransformReturnVoid());
		transformers.add(new TransformClassReference());
		transformers.add(new TransformCompilerFodder());
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
