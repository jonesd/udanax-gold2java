/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.detect.FeFillDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.WorksTestFillDetector;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class WorksTestFillDetector extends FeFillDetector {

	protected String myTag;
	protected PrintWriter myOutput;
/*
udanax-top.st:19506:
FeFillDetector subclass: #WorksTestFillDetector
	instanceVariableNames: '
		myTag {Character star}
		myOutput {ostream star}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:19512:
(WorksTestFillDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:19539:
WorksTestFillDetector class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19542:
(WorksTestFillDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(WorksTestFillDetector.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void filled(FeRangeElement transclusion) {
	AboraSupport.smalltalkOnly();
	{
		myOutput.print(myTag);
		myOutput.print(transclusion);
		myOutput.print("\n"+
"");
	}
	AboraSupport.translateOnly();
	{
		/* (*myOutput) << myTag << transclusion << \"\\n\"; */
	}
/*
udanax-top.st:19517:WorksTestFillDetector methodsFor: 'triggering'!
{void} filled: transclusion {FeRangeElement}
	[myOutput << myTag << transclusion << '
'] smalltalkOnly.
	'(*myOutput) << myTag << transclusion << "\n";' translateOnly.!
*/
}
public WorksTestFillDetector(PrintWriter oo, String tag) {
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
udanax-top.st:19525:WorksTestFillDetector methodsFor: 'private: create'!
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
udanax-top.st:19534:WorksTestFillDetector methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:19536:WorksTestFillDetector methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static FeFillDetector make(PrintWriter oo, String tag) {
	return new WorksTestFillDetector(oo, tag);
/*
udanax-top.st:19547:WorksTestFillDetector class methodsFor: 'pseudo constructors'!
{FeFillDetector} make: oo {ostream reference} with: tag {Character star}
	^self create: oo with: tag.!
*/
}
public WorksTestFillDetector() {
/*

Generated during transformation
*/
}
public WorksTestFillDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
