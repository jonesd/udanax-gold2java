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
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.tumbler.ExplicitArrangement;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceEdge;
import info.dgjones.abora.gold.tumbler.SequenceRegion;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.tumbler.SequenceUpOrder;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class SequenceUpOrder extends OrderSpec {

/*
udanax-top.st:31117:
OrderSpec subclass: #SequenceUpOrder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-tumbler'!
*/
/*
udanax-top.st:31121:
(SequenceUpOrder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:31177:
SequenceUpOrder class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:31180:
(SequenceUpOrder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SequenceUpOrder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	return getCategory().hashForEqual();
/*
udanax-top.st:31126:SequenceUpOrder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^self getCategory hashForEqual!
*/
}
public boolean follows(Position x, Position y) {
	return (((Sequence) x).secretNumbers().compare(((Sequence) y).secretNumbers())) >= 0;
/*
udanax-top.st:31130:SequenceUpOrder methodsFor: 'testing'!
{BooleanVar} follows: x {Position} with: y {Position}
	^((x cast: Sequence) secretNumbers
		compare: (y cast: Sequence) secretNumbers) >= Int32Zero!
*/
}
public boolean isEqual(Heaper other) {
	return other instanceof SequenceUpOrder;
/*
udanax-top.st:31135:SequenceUpOrder methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	^other isKindOf: SequenceUpOrder!
*/
}
public boolean isFullOrder(XnRegion keys) {
	return true;
/*
udanax-top.st:31139:SequenceUpOrder methodsFor: 'testing'!
{BooleanVar} isFullOrder: keys {XnRegion unused default: NULL}
	^true!
*/
}
public boolean preceeds(XnRegion before, XnRegion after) {
	SequenceRegion first;
	SequenceRegion second;
	first = (SequenceRegion) before;
	second = (SequenceRegion) after;
	if ( ! (first.isBoundedBelow())) {
		return true;
	}
	if ( ! (second.isBoundedBelow())) {
		return false;
	}
	return ! (((SequenceEdge) (first.secretTransitions().fetch(0))).isGE(((SequenceEdge) (second.secretTransitions().fetch(0)))));
/*
udanax-top.st:31143:SequenceUpOrder methodsFor: 'testing'!
{BooleanVar} preceeds: before {XnRegion} with: after {XnRegion}
	
	| first {SequenceRegion} second {SequenceRegion} |
	first _ before cast: SequenceRegion.
	second _ after cast: SequenceRegion.
	first isBoundedBelow ifFalse: [^true].
	second isBoundedBelow ifFalse: [^false].
	^(((first secretTransitions fetch: Int32Zero) cast: SequenceEdge)
		isGE: ((second secretTransitions fetch: Int32Zero) cast: SequenceEdge)) not!
*/
}
public Arrangement arrange(XnRegion region) {
	Stepper stepper;
	PtrArray array;
	if ( ! (region.isFinite())) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_FINITE);
	}
	stepper = ((SequenceRegion) region).stepper();
	array = (PtrArray) stepper.stepMany();
	if ( ! (stepper.end())) {
		throw new UnimplementedException();
	}
	return ExplicitArrangement.make(array);
/*
udanax-top.st:31155:SequenceUpOrder methodsFor: 'accessing'!
{Arrangement} arrange: region {XnRegion}
	| stepper {Stepper} array {PtrArray} |
	region isFinite ifFalse: [Heaper BLAST: #MustBeFinite].
	stepper := (region cast: SequenceRegion) stepper.
	array := stepper stepMany cast: PtrArray.
	stepper atEnd ifFalse: [self unimplemented].
	^ExplicitArrangement make: array!
*/
}
public CoordinateSpace coordinateSpace() {
	return SequenceSpace.make();
/*
udanax-top.st:31164:SequenceUpOrder methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^SequenceSpace make!
*/
}
public SequenceUpOrder(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:31170:SequenceUpOrder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:31173:SequenceUpOrder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static OrderSpec make() {
	return new SequenceUpOrder();
/*
udanax-top.st:31185:SequenceUpOrder class methodsFor: 'pseudo constructors'!
{OrderSpec} make
	^self create!
*/
}
public SequenceUpOrder() {
/*

Generated during transformation
*/
}
}
