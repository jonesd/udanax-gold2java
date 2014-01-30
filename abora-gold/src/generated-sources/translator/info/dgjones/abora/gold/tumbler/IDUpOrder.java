/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.tumbler;

import info.dgjones.abora.gold.arrange.Arrangement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.tumbler.ExplicitArrangement;
import info.dgjones.abora.gold.tumbler.IDUpOrder;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceRegion;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class IDUpOrder extends OrderSpec {

	protected IDSpace myIDSpace;
/*
udanax-top.st:30792:
OrderSpec subclass: #IDUpOrder
	instanceVariableNames: 'myIDSpace {IDSpace}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-tumbler'!
*/
/*
udanax-top.st:30796:
(IDUpOrder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:30875:
IDUpOrder class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:30878:
(IDUpOrder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IDUpOrder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	return getCategory().hashForEqual();
/*
udanax-top.st:30801:IDUpOrder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^self getCategory hashForEqual!
*/
}
public boolean follows(Position x, Position y) {
	if (x instanceof ID) {
		ID a = (ID) x;
		if (y instanceof ID) {
			ID b = (ID) y;
			Ravi.thingToDo();
			/* more efficient comparison */
			return ! (b.backend().isGE(a.backend())) || ((a.backend().isEqual(b.backend())) && (a.number() >= b.number()));
		}
	}
	return false;
/*
udanax-top.st:30805:IDUpOrder methodsFor: 'testing'!
{BooleanVar} follows: x {Position} with: y {Position}
	x cast: ID into: [ :a | y cast: ID into: [ :b |
		Ravi thingToDo. "more efficient comparison"
		^(b backend isGE: a backend) not
			or: [(a backend isEqual: b backend)
				and: [a number >= b number]]]].
	^false "fodder"!
*/
}
public boolean isEqual(Heaper other) {
	return other instanceof IDUpOrder;
/*
udanax-top.st:30814:IDUpOrder methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	^other isKindOf: IDUpOrder!
*/
}
public boolean isFullOrder(XnRegion keys) {
	return true;
/*
udanax-top.st:30818:IDUpOrder methodsFor: 'testing'!
{BooleanVar} isFullOrder: keys {XnRegion default: NULL}
	^true!
*/
}
/**
 * Return true if some position in before is less than or equal to all positions in after.
 */
public boolean preceeds(XnRegion before, XnRegion after) {
	SequenceRegion beforeB;
	SequenceRegion afterB;
	Sequence bound;
	if (before instanceof IDRegion) {
		IDRegion beforeIDs = (IDRegion) before;
		if (after instanceof IDRegion) {
			IDRegion afterIDs = (IDRegion) after;
			beforeB = beforeIDs.backends();
			afterB = afterIDs.backends();
			if ( ! (SequenceSpace.make().ascending().preceeds(beforeB, afterB))) {
				return false;
			}
			if ( ! (beforeB.isBoundedBelow())) {
				return true;
			}
			bound = beforeB.lowerBound();
			if ( ! (bound.isEqual(afterB.lowerBound()))) {
				return true;
			}
			return IntegerSpace.make().ascending().preceeds((beforeIDs.iDNumbersFrom(bound)), (afterIDs.iDNumbersFrom(bound)));
		}
	}
	return false;
/*
udanax-top.st:30822:IDUpOrder methodsFor: 'testing'!
{BooleanVar} preceeds: before {XnRegion} with: after {XnRegion}
	"Return true if some position in before is less than or equal to all positions in after."
	
	| beforeB {SequenceRegion} afterB {SequenceRegion} bound {Sequence} |
	before cast: IDRegion into: [ :beforeIDs |
	after cast: IDRegion into: [ :afterIDs |
		beforeB := beforeIDs backends.
		afterB := afterIDs backends.
		(SequenceSpace make ascending preceeds: beforeB with: afterB) ifFalse:
			[^false].
		beforeB isBoundedBelow ifFalse:
			[^true].
		bound := beforeB lowerBound.
		(bound isEqual: afterB lowerBound) ifFalse:
			[^true].
		^IntegerSpace make ascending
			preceeds: (beforeIDs iDNumbersFrom: bound)
			with: (afterIDs iDNumbersFrom: bound)]].
	^false "fodder"!
*/
}
public Arrangement arrange(XnRegion region) {
	Stepper stepper;
	PtrArray array;
	if ( ! (region.isFinite())) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_FINITE);
	}
	stepper = ((IDRegion) region).stepper();
	array = (PtrArray) stepper.stepMany();
	if ( ! (stepper.end())) {
		throw new UnimplementedException();
	}
	return ExplicitArrangement.make(array);
/*
udanax-top.st:30844:IDUpOrder methodsFor: 'accessing'!
{Arrangement} arrange: region {XnRegion}
	| stepper {Stepper} array {PtrArray} |
	region isFinite ifFalse: [Heaper BLAST: #MustBeFinite].
	stepper := (region cast: IDRegion) stepper.
	array := stepper stepMany cast: PtrArray.
	stepper atEnd ifFalse: [self unimplemented].
	^ExplicitArrangement make: array!
*/
}
public CoordinateSpace coordinateSpace() {
	return myIDSpace;
/*
udanax-top.st:30853:IDUpOrder methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^myIDSpace!
*/
}
public IDUpOrder(IDSpace space) {
	super();
	myIDSpace = space;
/*
udanax-top.st:30859:IDUpOrder methodsFor: 'create'!
create: space {IDSpace}
	super create.
	myIDSpace := space.!
*/
}
public IDUpOrder(Rcvr receiver) {
	super(receiver);
	myIDSpace = (IDSpace) receiver.receiveHeaper();
/*
udanax-top.st:30866:IDUpOrder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myIDSpace _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myIDSpace);
/*
udanax-top.st:30870:IDUpOrder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myIDSpace.!
*/
}
public static OrderSpec make(IDSpace space) {
	return new IDUpOrder(space);
/*
udanax-top.st:30883:IDUpOrder class methodsFor: 'pseudo constructors'!
{OrderSpec} make: space {IDSpace}
	^self create: space.!
*/
}
public IDUpOrder() {
/*

Generated during transformation
*/
}
}
