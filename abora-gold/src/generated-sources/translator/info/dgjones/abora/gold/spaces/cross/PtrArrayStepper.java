/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.cross;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.cross.PtrArrayStepper;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A Stepper for stepping over the elements of a PtrArray in ascending or descending order.
 * This is a TableStepper even though it is stepping over a PtrArray instead of a table.
 * Should probably eventually be generalized to PrimArrays. NOT.A.TYPE
 */
public class PtrArrayStepper extends TableStepper {

	protected PtrArray myArray;
	protected int myIndex;
	protected int myPastEnd;
	protected int myStep;
/*
udanax-top.st:56230:
TableStepper subclass: #PtrArrayStepper
	instanceVariableNames: '
		myArray {PtrArray of: Heaper}
		myIndex {Int32}
		myPastEnd {Int32}
		myStep {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Cross'!
*/
/*
udanax-top.st:56238:
PtrArrayStepper comment:
'A Stepper for stepping over the elements of a PtrArray in ascending or descending order.  This is a TableStepper even though it is stepping over a PtrArray instead of a table.  Should probably eventually be generalized to PrimArrays. NOT.A.TYPE'!
*/
/*
udanax-top.st:56240:
(PtrArrayStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
/*
udanax-top.st:56303:
PtrArrayStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:56306:
(PtrArrayStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PtrArrayStepper.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public Stepper copy() {
	return new PtrArrayStepper(((PtrArray) myArray.copy()), myIndex, myPastEnd, myStep);
/*
udanax-top.st:56245:PtrArrayStepper methodsFor: 'operations'!
{Stepper} copy
	
	^PtrArrayStepper create: (myArray copy cast: PtrArray) with: myIndex with: myPastEnd with: myStep!
*/
}
public Heaper fetch() {
	if (myIndex >= myPastEnd) {
		return null;
	}
	return myArray.fetch(myIndex);
/*
udanax-top.st:56249:PtrArrayStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	
	myIndex >= myPastEnd ifTrue:
		[^NULL].
	^myArray fetch: myIndex!
*/
}
public boolean hasValue() {
	return myIndex < myPastEnd;
/*
udanax-top.st:56255:PtrArrayStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	
	^myIndex < myPastEnd!
*/
}
public void step() {
	myIndex = myIndex + myStep;
/*
udanax-top.st:56259:PtrArrayStepper methodsFor: 'operations'!
{void} step
	myIndex _ myIndex + myStep!
*/
}
public int index() {
	if (myIndex >= myPastEnd) {
		throw new AboraRuntimeException(AboraRuntimeException.EMPTY_STEPPER);
	}
	return myIndex;
/*
udanax-top.st:56264:PtrArrayStepper methodsFor: 'special'!
{IntegerVar} index
	
	myIndex >= myPastEnd ifTrue:
		[Heaper BLAST: #EmptyStepper].
	^myIndex!
*/
}
public Position position() {
	if (myIndex >= myPastEnd) {
		throw new AboraRuntimeException(AboraRuntimeException.EMPTY_STEPPER);
	}
	return IntegerPos.make(myIndex);
/*
udanax-top.st:56270:PtrArrayStepper methodsFor: 'special'!
{Position} position
	
	myIndex >= myPastEnd ifTrue:
		[Heaper BLAST: #EmptyStepper].
	^myIndex integer!
*/
}
public PtrArrayStepper(PtrArray array, int start, int pastEnd, int step) {
	super();
	myArray = array;
	myIndex = start;
	myPastEnd = pastEnd;
	myStep = step;
/*
udanax-top.st:56278:PtrArrayStepper methodsFor: 'private: creation'!
create: array {PtrArray} with: start {Int32} with: pastEnd {Int32} with: step {Int32}
	super create.
	myArray := array.
	myIndex := start.
	myPastEnd := pastEnd.
	myStep := step!
*/
}
public PtrArrayStepper(Rcvr receiver) {
	super(receiver);
	myArray = (PtrArray) receiver.receiveHeaper();
	myIndex = receiver.receiveInt32();
	myPastEnd = receiver.receiveInt32();
	myStep = receiver.receiveInt32();
/*
udanax-top.st:56288:PtrArrayStepper methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myArray _ receiver receiveHeaper.
	myIndex _ receiver receiveInt32.
	myPastEnd _ receiver receiveInt32.
	myStep _ receiver receiveInt32.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myArray);
	xmtr.sendInt32(myIndex);
	xmtr.sendInt32(myPastEnd);
	xmtr.sendInt32(myStep);
/*
udanax-top.st:56295:PtrArrayStepper methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myArray.
	xmtr sendInt32: myIndex.
	xmtr sendInt32: myPastEnd.
	xmtr sendInt32: myStep.!
*/
}
/**
 * Note: this being a low level operation, and there being no lightweight form of immutable
 * or lazily copied PtrArray, it is my caller's responsibility to pass me a PtrArray which
 * will in fact not be changed during the life of this stepper.  This is an unchecked an
 * uncheckable precondition on my clients.
 */
public static TableStepper ascending(PtrArray array) {
	return new PtrArrayStepper(array, 0, array.count(), 1);
/*
udanax-top.st:56311:PtrArrayStepper class methodsFor: 'creation'!
{TableStepper} ascending: array {PtrArray}
	"Note: this being a low level operation, and there being no lightweight form of immutable or lazily copied PtrArray, it is my caller's responsibility to pass me a PtrArray which will in fact not be changed during the life of this stepper.  This is an unchecked an uncheckable precondition on my clients."
	 
	^self create: array with: Int32Zero with: array count with: 1!
*/
}
/**
 * Note: this being a low level operation, and there being no lightweight form of immutable
 * or lazily copied PtrArray, it is my caller's responsibility to pass me a PtrArray which
 * will in fact not be changed during the life of this stepper.  This is an unchecked an
 * uncheckable precondition on my clients.
 */
public static TableStepper descending(PtrArray array) {
	return new PtrArrayStepper(array, array.count() - 1, -1, -1);
/*
udanax-top.st:56316:PtrArrayStepper class methodsFor: 'creation'!
{TableStepper} descending: array {PtrArray}
	"Note: this being a low level operation, and there being no lightweight form of immutable or lazily copied PtrArray, it is my caller's responsibility to pass me a PtrArray which will in fact not be changed during the life of this stepper.  This is an unchecked an uncheckable precondition on my clients."
	
	^self create: array with: array count - 1 with: -1 with: -1!
*/
}
public PtrArrayStepper() {
/*

Generated during transformation
*/
}
}
