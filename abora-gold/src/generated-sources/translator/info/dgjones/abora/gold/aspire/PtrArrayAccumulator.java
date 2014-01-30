/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.aspire;

import info.dgjones.abora.gold.aspire.PtrArrayAccumulator;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Accumulator;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * To save array copies, this class will hand out its internal array if the size is right.
 * If it does so it remembers so that if new elements are introduced, a copy can be made for
 * further use.
 */
public class PtrArrayAccumulator extends Accumulator {

	protected PtrArray myValues;
	protected int myN;
	protected boolean myValuesGiven;
/*
udanax-top.st:12260:
Accumulator subclass: #PtrArrayAccumulator
	instanceVariableNames: '
		myValues {PtrArray}
		myN {UInt4}
		myValuesGiven {BooleanVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-aspire'!
*/
/*
udanax-top.st:12267:
PtrArrayAccumulator comment:
'To save array copies, this class will hand out its internal array if the size is right.  If it does so it remembers so that if new elements are introduced, a copy can be made for further use.'!
*/
/*
udanax-top.st:12269:
(PtrArrayAccumulator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PtrArrayAccumulator.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Accumulator copy() {
	return new PtrArrayAccumulator(((PtrArray) myValues.copy()), myN);
/*
udanax-top.st:12274:PtrArrayAccumulator methodsFor: 'operations'!
{Accumulator} copy
	^PtrArrayAccumulator create: (myValues copy cast: PtrArray) with: myN!
*/
}
public void step(Heaper x) {
	if ( ! (myN + 1 < myValues.count())) {
		myValues = (PtrArray) (myValues.copyGrow(myValues.count() + 1));
	}
	myValues.store(myN, x);
	myN = myN + 1;
/*
udanax-top.st:12278:PtrArrayAccumulator methodsFor: 'operations'!
{void} step: x {Heaper}
	myN + 1 < myValues count ifFalse:
		[myValues := (myValues copyGrow: myValues count+1) cast: PtrArray].
	myValues at: myN store: x.
	myN := myN + 1.!
*/
}
public Heaper value() {
	if (myValues.count() == myN) {
		myValuesGiven = true;
		return myValues;
	}
	else {
		return myValues.copy(myN);
	}
/*
udanax-top.st:12285:PtrArrayAccumulator methodsFor: 'operations'!
{Heaper} value
	myValues count == myN
		ifTrue: [
			myValuesGiven := true.
			^ myValues]
		ifFalse: [
			^myValues copy: myN]!
*/
}
public PtrArrayAccumulator() {
	super();
	myValues = PtrArray.nulls(2);
	myN = 0;
	myValuesGiven = false;
/*
udanax-top.st:12296:PtrArrayAccumulator methodsFor: 'create'!
create
	super create.
	myValues := PtrArray nulls: 2.
	myN := UInt32Zero.
	myValuesGiven := false!
*/
}
public PtrArrayAccumulator(int count) {
	super();
	myValues = PtrArray.nulls(count);
	myN = 0;
	myValuesGiven = false;
/*
udanax-top.st:12302:PtrArrayAccumulator methodsFor: 'create'!
create: count {UInt32}
	super create.
	myValues := PtrArray nulls: count.
	myN := UInt32Zero.
	myValuesGiven := false!
*/
}
public PtrArrayAccumulator(PtrArray values, int n) {
	super();
	myValues = values;
	myN = n;
	myValuesGiven = false;
/*
udanax-top.st:12308:PtrArrayAccumulator methodsFor: 'create'!
create: values {PtrArray} with: n {UInt32}
	super create.
	myValues := values.
	myN := n.
	myValuesGiven := false!
*/
}
public PtrArrayAccumulator(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
