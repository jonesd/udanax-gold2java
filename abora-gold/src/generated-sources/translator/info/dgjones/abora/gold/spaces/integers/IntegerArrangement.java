/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.integers;

import info.dgjones.abora.gold.arrange.Arrangement;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerArrangement;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class IntegerArrangement extends Arrangement {

	protected OrderSpec myOrdering;
	protected IntegerRegion myRegion;
/*
udanax-top.st:12681:
Arrangement subclass: #IntegerArrangement
	instanceVariableNames: '
		myOrdering {OrderSpec}
		myRegion {IntegerRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Integers'!
*/
/*
udanax-top.st:12687:
(IntegerArrangement getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
/*
udanax-top.st:12800:
IntegerArrangement class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:12803:
(IntegerArrangement getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IntegerArrangement.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public void copyElements(PrimArray toArray, Dsp toDsp, PrimArray fromArray, Arrangement fromArrange, XnRegion fromRegion) {
	IntegerArrangement other;
	int start;
	int stop;
	int toStart;
	other = (IntegerArrangement) fromArrange;
	if ( ! (myOrdering.isEqual(other.ordering()))) {
		throw new UnimplementedException();
	}
	if ( ! (myRegion.isSimple() && (other.region().isSimple() && (fromRegion.isSimple())))) {
		throw new UnimplementedException();
	}
	Someone.knownBug();
	/* Assume ascending for the moment. */
	start = (fromArrange.indexOf((fromRegion.chooseOne(myOrdering))));
	stop = (fromArrange.indexOf((fromRegion.chooseOne(myOrdering.reversed()))));
	toStart = (indexOf((toDsp.of((fromRegion.chooseOne(myOrdering))))));
	/* stop < start ifTrue: [| tmp {Int32} | tmp _ start.  start _ stop.  stop _ tmp]. */
	toArray.storeMany(toStart, fromArray, stop + 1 - start, start);
/*
udanax-top.st:12692:IntegerArrangement methodsFor: 'accessing'!
{void} copyElements: toArray {PrimArray} with: toDsp {Dsp}
	with: fromArray {PrimArray} with: fromArrange {Arrangement} with: fromRegion {XnRegion}
	
	| other {IntegerArrangement} start {Int32} stop {Int32} toStart {Int32} |
	other _ fromArrange cast: IntegerArrangement.
	(myOrdering isEqual: other ordering) ifFalse: [self unimplemented].
	(myRegion isSimple and: [other region isSimple and: [fromRegion isSimple]]) ifFalse: [self unimplemented].
	self knownBug.  "Assume ascending for the moment."
	start _ (fromArrange indexOf: (fromRegion chooseOne: myOrdering)) DOTasLong.
	stop _ (fromArrange indexOf: (fromRegion chooseOne: myOrdering reversed)) DOTasLong.
	toStart _ (self indexOf: (toDsp of: (fromRegion chooseOne: myOrdering))) DOTasLong.
	"stop < start ifTrue: [| tmp {Int32} | tmp _ start.  start _ stop.  stop _ tmp]."
	toArray at: toStart
		storeMany: fromArray 
		with: stop + 1 - start
		with: start!
*/
}
/**
 * Return the index of position into my Region according to my OrderSpec.
 */
public int indexOf(Position position) {
	int sum;
	int intPos;
	sum = 0;
	intPos = ((IntegerPos) position).asIntegerVar();
	Stepper stomper = (myRegion.simpleRegions(myOrdering));
	for (; stomper.hasValue(); stomper.step()) {
		IntegerRegion region = (IntegerRegion) stomper.fetch();
		if (region == null) {
			continue ;
		}
		if (region.hasIntMember(intPos)) {
			return Math.abs(sum + (intPos - ((IntegerPos) (region.chooseOne(myOrdering))).asIntegerVar()));
		}
		else {
			sum = sum + region.count();
		}
	}
	stomper.destroy();
	throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
/*
udanax-top.st:12709:IntegerArrangement methodsFor: 'accessing'!
{IntegerVar} indexOf: position {Position}
	"Return the index of position into my Region according to my OrderSpec."
	| sum {IntegerVar} intPos {IntegerVar} |
	sum _ IntegerVar0.
	intPos _ (position cast: IntegerPos) asIntegerVar.
	(myRegion simpleRegions: myOrdering) forEach: 
		[:region {IntegerRegion} |
		(region hasIntMember: intPos)
			ifTrue: [^sum + (intPos - ((region chooseOne: myOrdering) cast: IntegerPos) asIntegerVar) abs]
			ifFalse: [sum _ sum + region count]].
	Heaper BLAST: #NotInTable.
	^ -1 "compiler fodder"!
*/
}
/**
 * Return the region of all the indices corresponding to positions in region.
 */
public IntegerRegion indicesOf(XnRegion region) {
	Someone.shouldImplement();
	return null;
/*
udanax-top.st:12723:IntegerArrangement methodsFor: 'accessing'!
{IntegerRegion} indicesOf: region {XnRegion}
	"Return the region of all the indices corresponding to positions in region."
	Someone shouldImplement.
	^NULL "fodder"!
*/
}
/**
 * Return the region that corresponds to a range of indices.
 */
public XnRegion keysOf(int start, int stop) {
	int offset;
	int left;
	int right;
	offset = start;
	left = -1;
	Stepper stomper = (myRegion.simpleRegions(myOrdering));
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion region = (XnRegion) stomper.fetch();
		if (region == null) {
			continue ;
		}
		if (region.count() <= offset) {
			offset = offset - region.count();
		}
		else {
			if (left == -1) {
				left = ((IntegerPos) (region.chooseOne(myOrdering))).asIntegerVar() + offset;
				offset = stop - (start - offset);
				if (offset <= region.count()) {
					return IntegerRegion.make(left, (((IntegerPos) (region.chooseOne(myOrdering))).asIntegerVar() + offset));
				}
			}
			else {
				right = ((IntegerPos) (region.chooseOne(myOrdering))).asIntegerVar() + offset;
				return IntegerRegion.make(left, right);
			}
		}
	}
	stomper.destroy();
	throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
/*
udanax-top.st:12729:IntegerArrangement methodsFor: 'accessing'!
{XnRegion} keysOf: start {Int32} with: stop {Int32}
	"Return the region that corresponds to a range of indices."
	| offset {Int32} left {Int32} right {Int32} | 
	offset _ start.
	left _ -1.
	(myRegion simpleRegions: myOrdering) forEach: 
		[:region {XnRegion} |
		region count <= offset 
			ifTrue: [offset _ offset - region count DOTasLong]
			ifFalse:
				[left == -1 
					ifTrue: 
						[left _ ((region chooseOne: myOrdering) cast: IntegerPos) asIntegerVar DOTasLong + offset.
						offset _ stop - (start - offset).
						offset <= region count DOTasLong ifTrue: 
							[^IntegerRegion make: left 
									with: (((region chooseOne: myOrdering) cast: IntegerPos) asIntegerVar + offset)]]
					ifFalse:
						[right _ ((region chooseOne: myOrdering) cast: IntegerPos) asIntegerVar DOTasLong + offset.
						^IntegerRegion make: left with: right]]].
	Heaper BLAST: #NotInTable.
	^ NULL "compiler fodder"!
*/
}
public OrderSpec ordering() {
	return myOrdering;
/*
udanax-top.st:12753:IntegerArrangement methodsFor: 'accessing'!
{OrderSpec} ordering
	^myOrdering!
*/
}
public XnRegion region() {
	return myRegion;
/*
udanax-top.st:12756:IntegerArrangement methodsFor: 'accessing'!
{XnRegion} region
	^myRegion!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myRegion);
	oo.print(", ");
	oo.print(myOrdering);
	oo.print(")");
/*
udanax-top.st:12761:IntegerArrangement methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myRegion << ', ' << myOrdering << ')'!
*/
}
public IntegerArrangement(XnRegion region, OrderSpec ordering) {
	super();
	if ( ! (region.isFinite())) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_FINITE);
	}
	myRegion = (IntegerRegion) region;
	myOrdering = ordering;
/*
udanax-top.st:12766:IntegerArrangement methodsFor: 'protected: creation'!
create: region {XnRegion} with: ordering {OrderSpec}
	super create.
	region isFinite ifFalse: [Heaper BLAST: #MustBeFinite].
	myRegion _ region cast: IntegerRegion.
	myOrdering _ ordering!
*/
}
public int actualHashForEqual() {
	return myOrdering.hashForEqual() + myRegion.hashForEqual();
/*
udanax-top.st:12774:IntegerArrangement methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^ myOrdering hashForEqual + myRegion hashForEqual!
*/
}
public int hashForEqual() {
	return myOrdering.hashForEqual() + myRegion.hashForEqual();
/*
udanax-top.st:12777:IntegerArrangement methodsFor: 'testing'!
{UInt32} hashForEqual
	^ myOrdering hashForEqual + myRegion hashForEqual!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof IntegerArrangement) {
		IntegerArrangement o = (IntegerArrangement) other;
		return (myOrdering.isEqual(o.ordering())) && (myRegion.isEqual(o.region()));
	}
	else {
		return false;
	}
/*
udanax-top.st:12780:IntegerArrangement methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other cast: IntegerArrangement
		  into: [:o {IntegerArrangement} |
		  	^ (myOrdering isEqual: o ordering) and: [myRegion isEqual: o region]]
		  others: [^ false].
	^ false "fodder"!
*/
}
public IntegerArrangement(Rcvr receiver) {
	super(receiver);
	myOrdering = (OrderSpec) receiver.receiveHeaper();
	myRegion = (IntegerRegion) receiver.receiveHeaper();
/*
udanax-top.st:12789:IntegerArrangement methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myOrdering _ receiver receiveHeaper.
	myRegion _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myOrdering);
	xmtr.sendHeaper(myRegion);
/*
udanax-top.st:12794:IntegerArrangement methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myOrdering.
	xmtr sendHeaper: myRegion.!
*/
}
public static IntegerArrangement make(XnRegion region, OrderSpec ordering) {
	return new IntegerArrangement(region, ordering);
/*
udanax-top.st:12808:IntegerArrangement class methodsFor: 'creation'!
make: region {XnRegion} with: ordering {OrderSpec} 
	^self create: region with: ordering!
*/
}
public IntegerArrangement() {
/*

Generated during transformation
*/
}
}
