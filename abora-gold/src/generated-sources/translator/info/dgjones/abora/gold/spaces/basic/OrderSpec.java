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
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.OrderEnum;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.ReverseOrder;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.tumbler.ExplicitArrangement;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * [documentation note: we need to hide the documentation about partial orders, but still
 * warn that the orders may become partial]. An OrderSpec for a given coordinate space
 * represents a partial ordering of all the Positions of that coordinate space.  The
 * fundamental ordering relationship is "follows".  The response of Positions to isGE defines
 * the natural, "ascending" partial order among the positions.  Every coordinate space will
 * have at least this ascending and the corresponding descending OrderSpecs.  OrderSpecs are
 * useful to specify in what order a stepper produced for stepping over positions should do
 * so.
 */
public class OrderSpec extends Heaper {

/*
udanax-top.st:30481:
Heaper subclass: #OrderSpec
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Basic'!
*/
/*
udanax-top.st:30485:
OrderSpec comment:
'[documentation note: we need to hide the documentation about partial orders, but still warn that the orders may become partial]. An OrderSpec for a given coordinate space represents a partial ordering of all the Positions of that coordinate space.  The fundamental ordering relationship is "follows".  The response of Positions to isGE defines the natural, "ascending" partial order among the positions.  Every coordinate space will have at least this ascending and the corresponding descending OrderSpecs.  OrderSpecs are useful to specify in what order a stepper produced for stepping over positions should do so.'!
*/
/*
udanax-top.st:30487:
(OrderSpec getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #COPY; yourself)!
*/
/*
udanax-top.st:30571:
OrderSpec class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:30574:
(OrderSpec getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OrderSpec.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public boolean isFullOrder() {
	return isFullOrder(null);
/*
udanax-top.st:30492:OrderSpec methodsFor: 'smalltalk: defaults'!
isFullOrder
	^self isFullOrder: NULL!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:30498:OrderSpec methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
/**
 * Say what the relative ordering relationship is between x and y
 */
public OrderEnum compare(Position x, Position y) {
	if (follows(x, y)) {
		if (follows(y, x)) {
			return EQUAL;
		}
		else {
			return GREATERUTHAN;
		}
	}
	else {
		if (follows(y, x)) {
			return LESSUTHAN;
		}
		else {
			return INCOMPARABLE;
		}
	}
/*
udanax-top.st:30502:OrderSpec methodsFor: 'testing'!
{OrderEnum} compare: x {Position} with: y {Position}
	"Say what the relative ordering relationship is between x and y"
	(self follows: x with: y)
		ifTrue: [(self follows: y with: x) ifTrue: [^#EQUAL] ifFalse: [^#GREATER.U.THAN]]
		ifFalse: [(self follows: y with: x) ifTrue: [^#LESS.U.THAN] ifFalse: [^#INCOMPARABLE]]!
*/
}
/**
 * Essential.  Compare the two and return true if x is known to follow y in the ordering.
 * This message is the 'greater than or equal to' equivalent for this ordering.  It must have
 * those properties a mathematician would demand of a '>=' on a partial order:
 * os->follows(a, a)   (reflexivity)
 * os->follows(a, b) && os->follows(b, c) implies os->follows(a, c)  (transitivity)
 * os->follows(a, b) && os->follows(b, a) implies a->isEqual(b)  (what's the name for this?)
 */
public boolean follows(Position x, Position y) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:30509:OrderSpec methodsFor: 'testing'!
{BooleanVar CLIENT} follows: x {Position} with: y {Position}
	"Essential.  Compare the two and return true if x is known to follow y in the ordering.  This message is the 'greater than or equal to' equivalent for this ordering.  It must have those properties a mathematician would demand of a '>=' on a partial order:
		os->follows(a, a)   (reflexivity)
		os->follows(a, b) && os->follows(b, c) implies os->follows(a, c)  (transitivity)
		os->follows(a, b) && os->follows(b, a) implies a->isEqual(b)  (what's the name for this?)"
	self subclassResponsibility!
*/
}
/**
 * See discussion in XuInteger class comment about boxed vs unboxed integers
 */
public boolean followsInt(int x, int y) {
	return follows(IntegerPos.make(x), IntegerPos.make(y));
/*
udanax-top.st:30517:OrderSpec methodsFor: 'testing'!
{BooleanVar} followsInt: x {IntegerVar} with: y {IntegerVar}
	"See discussion in XuInteger class comment about boxed vs unboxed integers"
	^self follows: x integer with: y integer!
*/
}
public boolean isEqual(Heaper other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:30522:OrderSpec methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	self subclassResponsibility!
*/
}
/**
 * Essential.  If this returns TRUE, then I define a full order over all positions in 'keys'
 * (or all positions in the space if 'keys' is NULL).  However, if I return FALSE, that
 * doesn't guarantee that I don't define a full ordering.  I may happen to define a full
 * ordering without knowing it.
 * A full ordering is one in which for each a, b in keys; either this->follows(a, b) or
 * this->follows(b, a).
 */
public boolean isFullOrder(XnRegion keys) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:30526:OrderSpec methodsFor: 'testing'!
{BooleanVar} isFullOrder: keys {XnRegion default: NULL}
	"Essential.  If this returns TRUE, then I define a full order over all positions in 'keys' (or all positions in the space if 'keys' is NULL).  However, if I return FALSE, that doesn't guarantee that I don't define a full ordering.  I may happen to define a full ordering without knowing it.
	
	A full ordering is one in which for each a, b in keys; either this->follows(a, b) or this->follows(b, a)."
	
	self subclassResponsibility!
*/
}
/**
 * Return true if some position in before is less than or equal to all positions in after.
 */
public boolean preceeds(XnRegion before, XnRegion after) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:30533:OrderSpec methodsFor: 'testing'!
{BooleanVar} preceeds: before {XnRegion} with: after {XnRegion}
	"Return true if some position in before is less than or equal to all positions in after."
	
	self subclassResponsibility!
*/
}
/**
 * Return an Arrangement of the positions in region according to the ordering of the
 * receiver.
 */
public Arrangement arrange(XnRegion region) {
	return ExplicitArrangement.make(((PtrArray) (region.stepper(this)).stepMany()));
/*
udanax-top.st:30540:OrderSpec methodsFor: 'accessing'!
{Arrangement} arrange: region {XnRegion}
	"Return an Arrangement of the positions in region according to the ordering of the receiver."
	
	^ExplicitArrangement make: ((region stepper: self) stepMany cast: PtrArray)!
*/
}
/**
 * Essential.  Like Positions, Dsps, and XuRegions, an OrderSpec is specific to one
 * coordinate space.  It is an error to use the generic protocol on objects from different
 * coordinate spaces.
 */
public CoordinateSpace coordinateSpace() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:30545:OrderSpec methodsFor: 'accessing'!
{CoordinateSpace CLIENT} coordinateSpace
	"Essential.  Like Positions, Dsps, and XuRegions, an OrderSpec is specific to one coordinate space.  It is an error to use the generic protocol on objects from different coordinate spaces."
	self subclassResponsibility!
*/
}
/**
 * Returns an OrderSpec representing the mirror image of my ordering.
 * o->follows(a, b) iff o->reverse()->follows(b, a)
 */
public OrderSpec reversed() {
	return ReverseOrder.make(this);
/*
udanax-top.st:30550:OrderSpec methodsFor: 'accessing'!
{OrderSpec CLIENT} reversed
	"Returns an OrderSpec representing the mirror image of my ordering.
	o->follows(a, b) iff o->reverse()->follows(b, a)"
	
	^ReverseOrder make: self!
*/
}
/**
 * @deprecated
 */
public PrimArray export() {
	throw new PasseException();
/*
udanax-top.st:30558:OrderSpec methodsFor: 'smalltalk: passe'!
{PrimArray} export
	self passe!
*/
}
public OrderSpec(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:30564:OrderSpec methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:30567:OrderSpec methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
/**
 * Use CoordinateSpace::fetch/getAscending
 * @deprecated
 */
public static OrderSpec ascending(CoordinateSpace cs) {
	throw new PasseException();
/*
udanax-top.st:30579:OrderSpec class methodsFor: 'smalltalk: passe'!
{OrderSpec} ascending: cs {CoordinateSpace} 
	"Use CoordinateSpace::fetch/getAscending"
	self passe!
*/
}
/**
 * Use CoordinateSpace::fetch/getDescending
 * @deprecated
 */
public static OrderSpec descending(CoordinateSpace cs) {
	throw new PasseException();
/*
udanax-top.st:30584:OrderSpec class methodsFor: 'smalltalk: passe'!
{OrderSpec} descending: cs {CoordinateSpace}
	"Use CoordinateSpace::fetch/getDescending"
	
	self passe!
*/
}
/**
 * {CoordinateSpace CLIENT} coordinateSpace
 * {BooleanVar CLIENT} follows: x {Position} with: y {Position}
 * {OrderSpec CLIENT} reversed
 */
public static void infostProtocol() {
/*
udanax-top.st:30591:OrderSpec class methodsFor: 'smalltalk: system'!
info.stProtocol
"{CoordinateSpace CLIENT} coordinateSpace
{BooleanVar CLIENT} follows: x {Position} with: y {Position}
{OrderSpec CLIENT} reversed
"!
*/
}
public OrderSpec() {
/*

Generated during transformation
*/
}
}
