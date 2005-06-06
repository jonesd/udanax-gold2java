
/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.transform.method.MethodTransformation;



public class TransformIntraMethod implements MethodTransformation {

	final private List transformers;
	
	
	public TransformIntraMethod() {
		this(createTransformers());
	}
	
	public TransformIntraMethod(List transformers) {
		this.transformers = transformers;
	}
	
	private static List createTransformers() {		
		List transformers = new ArrayList();
		transformers.add(new OverrideReturnType());
		transformers.add(new OverrideArgumentType());
		transformers.add(new OverrideLocalVarType());
		transformers.add(new TransformAndOrs());
		transformers.add(new EnsureReasonableStatementTermination());
		transformers.add(new ExcludeStaticHeaper());
		transformers.add(new ExcludeMethods());
		transformers.add(new RewriteMethodBody());
		transformers.add(new ConvertToStaticBlocks());
		transformers.add(new DeprecatedMethods());
		//TODO this only produces a snapshot of locals
		transformers.add(new RecordLocalVars());
		transformers.add(new ForceReturn());
		
		transformers.add(new TransformSelfSends());
		transformers.add(new TransformUses());
		transformers.add(new TransformRequires());
//		transformers.add(new TransformSmalltalkOnly());
		transformers.add(new TransformTranslateOnlyString());
		transformers.add(new TransformNewCreateCallOnClass());
		transformers.add(new TransformSuperCreate());
		transformers.add(new TransformCreateCall());
		transformers.add(new TransformCerr());
		transformers.add(new TransformXuTime());
		transformers.add(new TransformRaisedTo());
		transformers.add(new TransformWhileTrue());
		transformers.add(new TransformTimesRepeat());
		transformers.add(new TransformStringDo());
		transformers.add(new TransformCritical());
		transformers.add(new TransformValueNowOrOnOnUnwindDo());
		transformers.add(new TransformAtCalls());
		transformers.add(new TransformIntegerZero());
		transformers.add(new TransformUint8());
		transformers.add(new TransformBlast());
		transformers.add(new TransformIsKindOf());
		transformers.add(new TransformStar());
		transformers.add(new TransformCast());
		transformers.add(new TransformAlmostTo());
		transformers.add(new TransformSubclassResponsibility());
		transformers.add(new TransformDOTCalls());
		transformers.add(new TransformDOThashForEqual());
		transformers.add(new TransformCAThashForEqual());
		transformers.add(new TransformHashDouble());
		transformers.add(new TransformDOTputCharacter());
		transformers.add(new TransformExponentDouble());
		transformers.add(new TransformIntegerIntegerVar());
		transformers.add(new TransformPrint());
		transformers.add(new TransformBinaryOperator());
		transformers.add(new TransformDiskManagerConsistent());
		transformers.add(new TransformCategoryName());
		transformers.add(new TransformReceiverReceiveHeaper());
		transformers.add(new TransformReceiverReceiveIntegerVar());
		transformers.add(new TransformIntegerCall());
		transformers.add(new TransformSignals());
		transformers.add(new TransformShouldImplement());
		transformers.add(new TransformStrcmp());
		transformers.add(new TransformStrlen());
		transformers.add(new TransformMathCalls());
		transformers.add(new TransformPrintStringRadix());
		transformers.add(new TransformPrintOnBase());
		transformers.add(new TransformPrintString());
		transformers.add(new TransformNewCreate());
		transformers.add(new TransformNewCall());
		transformers.add(new RenameCall());
		transformers.add(new RemoveCall());
		transformers.add(new TransformFluidAccess());
		transformers.add(new TransformFluidBindDuring());
		transformers.add(new TransformDefineFluid());
		transformers.add(new TransformReanimate());
		transformers.add(new TransformUnaryOperator());
		transformers.add(new TransformSendHeaper());
		transformers.add(new TransformSendIntegerVar());
		transformers.add(new TransformNextPutPrint());
		transformers.add(new TransformAssert());
		transformers.add(new TransformConditionalOperator());
		transformers.add(new TransformCastIntoOthers());
		transformers.add(new TransformCastCheck());
		transformers.add(new TransformNotNULLElse());		
		transformers.add(new TransformUnimplemented());
		transformers.add(new TransformPasse());
		transformers.add(new ChooseTransformOnly());
		transformers.add(new TransformStreamContents());
		transformers.add(new TransformLog());
		transformers.add(new TransformStringAsTextWriteStream());
		transformers.add(new TransformInitializeLocalVariable());
		transformers.add(new TransformNewBecome());
		transformers.add(new TransformOperatorIntNull());
		transformers.add(new TransformDeleteString());
		transformers.add(new TransformMuSetMake());
		transformers.add(new TransformOrglRootMake());
		transformers.add(new TransformAtIfAbsent());
		transformers.add(new TransformBlockReturn());
		transformers.add(new TransformSocket());
		transformers.add(new TransformCharacterStaticCall());
		transformers.add(new TransformPointerToStaticMember());
				
		transformers.add(new EnsureUniqueLocalVarNames());
		transformers.add(new EnsureIfTestInParentheses());
		transformers.add(new TransformExcessParantheses());
		transformers.add(new TransformClassX());
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
