/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.tumbler;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.filter.RealDsp;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.tumbler.AfterReal;
import info.dgjones.abora.gold.tumbler.BeforeReal;
import info.dgjones.abora.gold.tumbler.RealPos;
import info.dgjones.abora.gold.tumbler.RealRegion;
import info.dgjones.abora.gold.tumbler.RealSpace;
import info.dgjones.abora.gold.tumbler.RealUpOrder;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Non-arithmetic space of real numbers in which only certain positions are explicitly
 * representable.  In this release, the only exactly representable numbers are those real
 * numbers which can be represented in IEEE64 (double precision) format.  Future releases may
 * make more real numbers representable.
 */
public class RealSpace extends CoordinateSpace {

	protected static RealSpace TheRealSpace;
/*
udanax-top.st:15500:
CoordinateSpace subclass: #RealSpace
	instanceVariableNames: ''
	classVariableNames: 'TheRealSpace {RealSpace} '
	poolDictionaries: ''
	category: 'Xanadu-tumbler'!
*/
/*
udanax-top.st:15504:
RealSpace comment:
'Non-arithmetic space of real numbers in which only certain positions are explicitly representable.  In this release, the only exactly representable numbers are those real numbers which can be represented in IEEE64 (double precision) format.  Future releases may make more real numbers representable.'!
*/
/*
udanax-top.st:15506:
(RealSpace getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #ON.CLIENT; yourself)!
*/
/*
udanax-top.st:15604:
RealSpace class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:15607:
(RealSpace getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #ON.CLIENT; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RealSpace.class).setAttributes( new Set().add("PSEUDOCOPY").add("CONCRETE").add("ONCLIENT"));
/*

Generated during transformation: AddMethod
*/
}
public RealSpace() {
	super((RealRegion.make(false, PtrArray.empty())), (RealRegion.make(true, PtrArray.empty())), RealDsp.make(), RealUpOrder.make());
/*
udanax-top.st:15511:RealSpace methodsFor: 'create'!
create
	
	super create: (RealRegion make: false with: PtrArray empty)
		with: (RealRegion make: true with: PtrArray empty)
		with: RealDsp make
		with: RealUpOrder make!
*/
}
/**
 * The region consisting of all positions >= val if inclusive, or all > val if not inclusive.
 */
public RealRegion above(RealPos val, boolean inclusive) {
	if (inclusive) {
		return RealRegion.make(false, (PrimSpec.pointer().arrayWith((BeforeReal.make(val)))));
	}
	else {
		return RealRegion.make(false, (PrimSpec.pointer().arrayWith((AfterReal.make(val)))));
	}
/*
udanax-top.st:15520:RealSpace methodsFor: 'making'!
{RealRegion CLIENT} above: val {RealPos} with: inclusive {BooleanVar}
	"The region consisting of all positions >= val if inclusive, or all > val if not inclusive."
	inclusive ifTrue:
		[^RealRegion make: false
			with: (PrimSpec pointer arrayWith: (BeforeReal make: val))]
	ifFalse:
		[^RealRegion make: false
			with: (PrimSpec pointer arrayWith: (AfterReal make: val))]!
*/
}
/**
 * The region consisting of all positions <= val if inclusive, or all < val if not inclusive.
 */
public RealRegion below(RealPos val, boolean inclusive) {
	if (inclusive) {
		return RealRegion.make(true, (PrimSpec.pointer().arrayWith((AfterReal.make(val)))));
	}
	else {
		return RealRegion.make(true, (PrimSpec.pointer().arrayWith((BeforeReal.make(val)))));
	}
/*
udanax-top.st:15530:RealSpace methodsFor: 'making'!
{RealRegion CLIENT} below: val {RealPos} with: inclusive {BooleanVar}
	"The region consisting of all positions <= val if inclusive, or all < val if not inclusive."
	inclusive ifTrue:
		[^RealRegion make: true
			with: (PrimSpec pointer arrayWith: (AfterReal make: val))]
	ifFalse:
		[^RealRegion make: true
			with: (PrimSpec pointer arrayWith: (BeforeReal make: val))]!
*/
}
/**
 * Return a region of all numbers >= lower and < upper.
 */
public RealRegion interval(RealPos start, RealPos stop) {
	MarkM.thingToDo();
	/* use a single constructor */
	return (RealRegion) ((above(start, true)).intersect((below(stop, false))));
/*
udanax-top.st:15540:RealSpace methodsFor: 'making'!
{RealRegion CLIENT} interval: start {RealPos} with: stop {RealPos}
	"Return a region of all numbers >= lower and < upper."
	
	MarkM thingToDo. "use a single constructor"
	^((self above: start with: true) intersect: (self below: stop with: false)) cast: RealRegion!
*/
}
/**
 * The XuReal representing the same real number as that exactly represented by 'val'.  If
 * 'val' doesn't represent a real number (i.e., it is an infinity or a NAN), then this
 * message BLASTs.  If 'val' is a negative zero, it is silently converted to a positive zero
 */
public RealPos position(double val) {
	return RealPos.make(val);
/*
udanax-top.st:15546:RealSpace methodsFor: 'making'!
{RealPos CLIENT INLINE} position: val {IEEE64}
	"The XuReal representing the same real number as that exactly represented by 'val'.  If 'val' doesn't represent a real number (i.e., it is an infinity or a NAN), then this message BLASTs.  If 'val' is a negative zero, it is silently converted to a positive zero"
	
	^RealPos make: val!
*/
}
/**
 * The region consisting of all position >= val.
 * Should this just be supplanted by CoordinateSpace::region ()?
 */
public RealRegion after(double val) {
	Someone.thingToDo();
	/* update clients */
	return RealRegion.make(false, (PrimSpec.pointer().arrayWith((BeforeReal.make((RealPos.make(val)))))));
/*
udanax-top.st:15553:RealSpace methodsFor: 'obsolete:'!
{RealRegion} after: val {IEEE64}
	"The region consisting of all position >= val.
	Should this just be supplanted by CoordinateSpace::region ()?"
	self thingToDo. "update clients"
	^RealRegion make: false
		with: (PrimSpec pointer arrayWith: (BeforeReal make: (RealPos make: val)))!
*/
}
/**
 * The region consisting of all position <= val
 * Should this just be supplanted by CoordinateSpace::region ()?
 */
public RealRegion before(double val) {
	Someone.thingToDo();
	/* update clients */
	return RealRegion.make(true, (PrimSpec.pointer().arrayWith((AfterReal.make((RealPos.make(val)))))));
/*
udanax-top.st:15561:RealSpace methodsFor: 'obsolete:'!
{RealRegion} before: val {IEEE64}
	"The region consisting of all position <= val
	Should this just be supplanted by CoordinateSpace::region ()?"
	self thingToDo. "update clients"
	^RealRegion make: true
		with: (PrimSpec pointer arrayWith: (AfterReal make: (RealPos make: val)))!
*/
}
/**
 * The region consisting of all position > val
 * Should this just be supplanted by CoordinateSpace::region ()?
 * Add Boolean to after to say whether its inclusive?
 */
public RealRegion strictlyAfter(double val) {
	Someone.thingToDo();
	/* update clients */
	return RealRegion.make(false, (PrimSpec.pointer().arrayWith((AfterReal.make((RealPos.make(val)))))));
/*
udanax-top.st:15569:RealSpace methodsFor: 'obsolete:'!
{RealRegion} strictlyAfter: val {IEEE64}
	"The region consisting of all position > val
	Should this just be supplanted by CoordinateSpace::region ()?
	Add Boolean to after to say whether its inclusive?"
	self thingToDo. "update clients"
	^RealRegion make: false
		with: (PrimSpec pointer arrayWith: (AfterReal make: (RealPos make: val)))!
*/
}
/**
 * The region consisting of all position < val
 * Should this just be supplanted by CoordinateSpace::region ()?
 * Add Boolean to before to say whether its inclusive?
 */
public RealRegion strictlyBefore(double val) {
	Someone.thingToDo();
	/* update clients */
	return RealRegion.make(true, (PrimSpec.pointer().arrayWith((AfterReal.make((RealPos.make(val)))))));
/*
udanax-top.st:15578:RealSpace methodsFor: 'obsolete:'!
{RealRegion} strictlyBefore: val {IEEE64}
	"The region consisting of all position < val
	Should this just be supplanted by CoordinateSpace::region ()?
	Add Boolean to before to say whether its inclusive?"
	self thingToDo. "update clients"
	^RealRegion make: true
		with: (PrimSpec pointer arrayWith: (AfterReal make: (RealPos make: val)))!
*/
}
/**
 * is equal to any basic space on the same category of positions
 */
public int actualHashForEqual() {
	return getCategory().hashForEqual() + 1;
/*
udanax-top.st:15589:RealSpace methodsFor: 'testing'!
{UInt32} actualHashForEqual
	"is equal to any basic space on the same category of positions"
	^self getCategory hashForEqual + 1!
*/
}
/**
 * is equal to any basic space on the same category of positions
 */
public boolean isEqual(Heaper anObject) {
	return anObject.getCategory() == getCategory();
/*
udanax-top.st:15594:RealSpace methodsFor: 'testing'!
{BooleanVar} isEqual: anObject {Heaper}
	"is equal to any basic space on the same category of positions"
	^anObject getCategory == self getCategory!
*/
}
public void sendSelfTo(Xmtr xmtr) {
/*
udanax-top.st:15601:RealSpace methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}!
*/
}
public static RealSpace make() {
	return TheRealSpace;
/*
udanax-top.st:15612:RealSpace class methodsFor: 'creation'!
{RealSpace CLIENT INLINE} make
	^TheRealSpace!
*/
}
public static Heaper makeRcvr(Rcvr rcvr) {
	((SpecialistRcvr) rcvr).registerIbid(TheRealSpace);
	return TheRealSpace;
/*
udanax-top.st:15618:RealSpace class methodsFor: 'rcvr pseudo constructors'!
{Heaper} make.Rcvr: rcvr {Rcvr}
	(rcvr cast: SpecialistRcvr) registerIbid: TheRealSpace.
	^TheRealSpace!
*/
}
public static void initTimeNonInherited() {
	TheRealSpace = new RealSpace();
/*
udanax-top.st:15624:RealSpace class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	self REQUIRES: PrimSpec.
	TheRealSpace := self create!
*/
}
public static void linkTimeNonInherited() {
	TheRealSpace = null;
/*
udanax-top.st:15629:RealSpace class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheRealSpace := NULL!
*/
}
/**
 * {RealRegion CLIENT} above: val {IEEE64} with: inclusive {BooleanVar}
 * {RealRegion CLIENT} below: val {IEEE64} with: inclusive {BooleanVar}
 * {RealRegion CLIENT} interval: lower {XuRegion} with: upper {XuReal}
 * {XuReal CLIENT} position: val {IEEE64}
 */
public static void infostProtocol() {
/*
udanax-top.st:15635:RealSpace class methodsFor: 'smalltalk: system'!
info.stProtocol
"{RealRegion CLIENT} above: val {IEEE64} with: inclusive {BooleanVar}
{RealRegion CLIENT} below: val {IEEE64} with: inclusive {BooleanVar}
{RealRegion CLIENT} interval: lower {XuRegion} with: upper {XuReal}
{XuReal CLIENT} position: val {IEEE64}
"!
*/
}
public RealSpace(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
