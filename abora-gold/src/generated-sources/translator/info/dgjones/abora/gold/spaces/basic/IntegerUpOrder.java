/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.basic;

import info.dgjones.abora.gold.arrange.Arrangement;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.IntegerUpOrder;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerArrangement;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class IntegerUpOrder extends OrderSpec {

/*
udanax-top.st:30887:
OrderSpec subclass: #IntegerUpOrder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Basic'!
*/
/*
udanax-top.st:30891:
(IntegerUpOrder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:30955:
IntegerUpOrder class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:30958:
(IntegerUpOrder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IntegerUpOrder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	return HashHelper.hashForEqual(this.getClass()) + 1;
/*
udanax-top.st:30896:IntegerUpOrder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^#cat.U.IntegerUpOrder hashForEqual + 1!
*/
}
public boolean follows(Position x, Position y) {
	return ((IntegerPos) x).asIntegerVar() >= ((IntegerPos) y).asIntegerVar();
/*
udanax-top.st:30900:IntegerUpOrder methodsFor: 'testing'!
{BooleanVar} follows: x {Position} with: y {Position}
	^(x cast: IntegerPos) asIntegerVar >= (y cast: IntegerPos) asIntegerVar!
*/
}
/**
 * See discussion in XuInteger class comment about boxed vs unboxed integers
 */
public boolean followsInt(int x, int y) {
	return x >= y;
/*
udanax-top.st:30904:IntegerUpOrder methodsFor: 'testing'!
{BooleanVar} followsInt: x {IntegerVar} with: y {IntegerVar}
	"See discussion in XuInteger class comment about boxed vs unboxed integers"
	^ x >= y!
*/
}
public boolean isEqual(Heaper other) {
	return other instanceof IntegerUpOrder;
/*
udanax-top.st:30909:IntegerUpOrder methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	^other isKindOf: IntegerUpOrder!
*/
}
public boolean isFullOrder(XnRegion keys) {
	return true;
/*
udanax-top.st:30913:IntegerUpOrder methodsFor: 'testing'!
{BooleanVar} isFullOrder: keys {XnRegion unused default: NULL}
	
	^true!
*/
}
/**
 * Return true if some position in before is less than or equal to all positions in after.
 */
public boolean preceeds(XnRegion before, XnRegion after) {
	IntegerRegion first;
	IntegerRegion second;
	first = (IntegerRegion) before;
	second = (IntegerRegion) after;
	if ( ! (first.isBoundedBelow())) {
		return true;
	}
	if ( ! (second.isBoundedBelow())) {
		return false;
	}
	return first.start() <= second.start();
/*
udanax-top.st:30917:IntegerUpOrder methodsFor: 'testing'!
{BooleanVar} preceeds: before {XnRegion} with: after {XnRegion}
	"Return true if some position in before is less than or equal to all positions in after."
	
	| first {IntegerRegion} second {IntegerRegion} |
	first _ before cast: IntegerRegion.
	second _ after cast: IntegerRegion.
	first isBoundedBelow ifFalse: [^true].
	second isBoundedBelow ifFalse: [^false].
	^first start <= second start!
*/
}
public Arrangement arrange(XnRegion region) {
	return IntegerArrangement.make(region, this);
/*
udanax-top.st:30929:IntegerUpOrder methodsFor: 'accessing'!
{Arrangement} arrange: region {XnRegion}
	^IntegerArrangement make: region with: self.!
*/
}
/**
 * Return the first n positions in the region according to my ordering.
 */
public XnRegion chooseMany(XnRegion region, int n) {
	return (arrange(region)).keysOf(0, n);
/*
udanax-top.st:30932:IntegerUpOrder methodsFor: 'accessing'!
{XnRegion} chooseMany: region {XnRegion} with: n {IntegerVar}
	"Return the first n positions in the region according to my ordering."
	
	^(self arrange: region) keysOf: Int32Zero with: n DOTasLong!
*/
}
/**
 * Return the first position in the region according to my ordering.
 */
public Position chooseOne(XnRegion region) {
	return IntegerPos.make(((IntegerRegion) region).start());
/*
udanax-top.st:30937:IntegerUpOrder methodsFor: 'accessing'!
{Position} chooseOne: region {XnRegion}
	"Return the first position in the region according to my ordering."
	
	^IntegerPos make: (region cast: IntegerRegion) start!
*/
}
public CoordinateSpace coordinateSpace() {
	return IntegerSpace.make();
/*
udanax-top.st:30942:IntegerUpOrder methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	
	^IntegerSpace make!
*/
}
public IntegerUpOrder(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:30948:IntegerUpOrder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:30951:IntegerUpOrder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static OrderSpec make() {
	return new IntegerUpOrder();
/*
udanax-top.st:30963:IntegerUpOrder class methodsFor: 'pseudoconstructors'!
{OrderSpec} make
	^self create!
*/
}
public IntegerUpOrder() {
/*

Generated during transformation
*/
}
}
