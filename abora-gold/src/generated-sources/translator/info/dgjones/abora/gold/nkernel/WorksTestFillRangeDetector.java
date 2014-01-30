/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.WorksTestFillRangeDetector;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class WorksTestFillRangeDetector extends FeFillRangeDetector {

	protected String myTag;
	protected PrintWriter myOutput;
/*
udanax-top.st:19626:
FeFillRangeDetector subclass: #WorksTestFillRangeDetector
	instanceVariableNames: '
		myTag {Character star}
		myOutput {ostream star}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:19632:
(WorksTestFillRangeDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:19659:
WorksTestFillRangeDetector class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19662:
(WorksTestFillRangeDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(WorksTestFillRangeDetector.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void rangeFilled(FeEdition transclusions) {
	AboraSupport.smalltalkOnly();
	{
		myOutput.print(myTag);
		myOutput.print(transclusions);
		myOutput.print("\n"+
"");
	}
	AboraSupport.translateOnly();
	{
		/* (*myOutput) << myTag << transclusions << \"\\n\"; */
	}
/*
udanax-top.st:19637:WorksTestFillRangeDetector methodsFor: 'triggering'!
{void} rangeFilled: transclusions {FeEdition}
	[myOutput << myTag << transclusions << '
'] smalltalkOnly.
	'(*myOutput) << myTag << transclusions << "\n";' translateOnly.!
*/
}
public WorksTestFillRangeDetector(PrintWriter oo, String tag) {
	super();
	AboraSupport.smalltalkOnly();
	{
		myOutput = oo;
	}
	AboraSupport.translateOnly();
	{
		/* myOutput = &oo; */
	}
	myTag = tag;
/*
udanax-top.st:19645:WorksTestFillRangeDetector methodsFor: 'private: create'!
create: oo {ostream reference} with: tag {Character star}
	super create.
	[myOutput := oo] smalltalkOnly.
	'myOutput = &oo;' translateOnly.
	myTag := tag.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:19654:WorksTestFillRangeDetector methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:19656:WorksTestFillRangeDetector methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static FeFillRangeDetector make(PrintWriter oo, String tag) {
	return new WorksTestFillRangeDetector(oo, tag);
/*
udanax-top.st:19667:WorksTestFillRangeDetector class methodsFor: 'pseudo constructors'!
{FeFillRangeDetector} make: oo {ostream reference} with: tag {Character star}
	^self create: oo with: tag.!
*/
}
public WorksTestFillRangeDetector() {
/*

Generated during transformation
*/
}
public WorksTestFillRangeDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
