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
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.id.SequenceStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.tumbler.AfterSequence;
import info.dgjones.abora.gold.tumbler.BeforeSequence;
import info.dgjones.abora.gold.tumbler.BeforeSequencePrefix;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceEdge;
import info.dgjones.abora.gold.tumbler.SequenceManager;
import info.dgjones.abora.gold.tumbler.SequenceRegion;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * Represents a Region of Sequences. We can efficiently represent unions of intervals,
 * delimited either by individual sequences or by a match with all sequences prefixed by some
 * sequence up to some index.
 */
public class SequenceRegion extends XnRegion {

	protected boolean myStartsInside;
	protected PtrArray myTransitions;
	protected int myTransitionsCount;
	protected static SequenceRegion TheEmptySequenceRegion;
	protected static SequenceRegion TheFullSequenceRegion;
	protected static SequenceManager TheManager;
/*
udanax-top.st:69532:
XnRegion subclass: #SequenceRegion
	instanceVariableNames: '
		myStartsInside {BooleanVar}
		myTransitions {PtrArray NOCOPY of: SequenceEdge}
		myTransitionsCount {Int32}'
	classVariableNames: '
		TheEmptySequenceRegion {SequenceRegion} 
		TheFullSequenceRegion {SequenceRegion} 
		TheManager {SequenceManager} '
	poolDictionaries: ''
	category: 'Xanadu-tumbler'!
*/
/*
udanax-top.st:69542:
SequenceRegion comment:
'Represents a Region of Sequences. We can efficiently represent unions of intervals, delimited either by individual sequences or by a match with all sequences prefixed by some sequence up to some index.'!
*/
/*
udanax-top.st:69544:
(SequenceRegion getOrMakeCxxClassDescription)
	friends:
'/- friends for class SequenceRegion -/
friend class Sequence;
friend class SequenceSpace;
friend class SequenceMapping;
friend class SequenceManager;
';
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
*/
/*
udanax-top.st:69815:
SequenceRegion class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:69818:
(SequenceRegion getOrMakeCxxClassDescription)
	friends:
'/- friends for class SequenceRegion -/
friend class Sequence;
friend class SequenceSpace;
friend class SequenceMapping;
friend class SequenceManager;
';
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SequenceRegion.class).setAttributes( new Set().add("CONCRETE").add("ONCLIENT").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public SequenceRegion(boolean startsInside, PtrArray transitions) {
	super();
	myStartsInside = startsInside;
	myTransitions = transitions;
	myTransitionsCount = transitions.count();
/*
udanax-top.st:69557:SequenceRegion methodsFor: 'create'!
create: startsInside {BooleanVar} with: transitions {PtrArray of: TransitionEdge}
	super create.
	myStartsInside := startsInside.
	myTransitions := transitions.
	myTransitionsCount := transitions count.!
*/
}
public SequenceRegion(boolean startsInside, PtrArray transitions, int count) {
	super();
	myStartsInside = startsInside;
	myTransitions = transitions;
	myTransitionsCount = count;
/*
udanax-top.st:69564:SequenceRegion methodsFor: 'create'!
create: startsInside {BooleanVar} with: transitions {PtrArray of: TransitionEdge} with: count {Int32}
	super create.
	myStartsInside := startsInside.
	myTransitions := transitions.
	myTransitionsCount := count!
*/
}
public XnRegion asSimpleRegion() {
	return TheManager.asSimpleRegion(this);
/*
udanax-top.st:69573:SequenceRegion methodsFor: 'accessing'!
{XnRegion} asSimpleRegion
	^TheManager asSimpleRegion: self!
*/
}
public CoordinateSpace coordinateSpace() {
	return SequenceSpace.make();
/*
udanax-top.st:69577:SequenceRegion methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^SequenceSpace make!
*/
}
/**
 * The largest sequence such that all the positions in the region are >= it.  Does not
 * necessarily lie in the region.  For example, the region of all numbers > 2.3 has a
 * lowerBound of 2.3. Mathematically, this is called the 'greatest lower bound'.
 */
public Sequence lowerBound() {
	return (Sequence) (TheManager.greatestLowerBound(this));
/*
udanax-top.st:69581:SequenceRegion methodsFor: 'accessing'!
{Sequence} lowerBound
	"The largest sequence such that all the positions in the region are >= it.  Does not necessarily lie in the region.  For example, the region of all numbers > 2.3 has a lowerBound of 2.3. Mathematically, this is called the 'greatest lower bound'."
	^(TheManager greatestLowerBound: self) cast: Sequence!
*/
}
/**
 * Essential. The Sequence associated with the lower edge of the Region. To find out where
 * the boundary is in relation to this sequence, check lowerEdgeType. BLASTS if unbounded
 * below.
 */
public Sequence lowerEdge() {
	if (myTransitionsCount == 0) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	return ((SequenceEdge) (myTransitions.fetch(0))).sequence();
/*
udanax-top.st:69586:SequenceRegion methodsFor: 'accessing'!
{Sequence CLIENT} lowerEdge
	"Essential. The Sequence associated with the lower edge of the Region. To find out where the boundary is in relation to this sequence, check lowerEdgeType. BLASTS if unbounded below."
	
	myTransitionsCount = Int32Zero ifTrue:
		[Heaper BLAST: #InvalidRequest].
	^((myTransitions fetch: Int32Zero) cast: SequenceEdge) sequence!
*/
}
/**
 * Essential. If lowerEdgeType is prefix, then it includes an Sequence matching each integer
 * in the lowerEdge up to and including lowerEdgePrefixLimit.
 */
public int lowerEdgePrefixLimit() {
	if (myTransitionsCount == 0) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	Heaper cast1 = (myTransitions.fetch(0));
	if (cast1 instanceof BeforeSequencePrefix) {
		BeforeSequencePrefix prefix = (BeforeSequencePrefix) cast1;
		prefix.limit();
	}
	return -1;
/*
udanax-top.st:69593:SequenceRegion methodsFor: 'accessing'!
{IntegerVar CLIENT} lowerEdgePrefixLimit
	"Essential. If lowerEdgeType is prefix, then it includes an Sequence matching each integer in the lowerEdge up to and including lowerEdgePrefixLimit."
	
	myTransitionsCount = Int32Zero ifTrue:
		[Heaper BLAST: #InvalidRequest].
	(myTransitions fetch: Int32Zero) cast: BeforeSequencePrefix into:
		[ :prefix | prefix limit].
	^ -1 "compiler fodder"!
*/
}
/**
 * Essential. The kind of Sequence associated with the lower edge of the Region. If
 * SequenceRegion::inclusive then it includes the lowerEdge; if exclusive, then it does not;
 * if prefix, then it includes any Sequence matching each integer in the lowerEdge up to and
 * including lowerEdgePrefixLimit.
 */
public int lowerEdgeType() {
	if (myTransitionsCount == 0) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	Heaper cast1 = (myTransitions.fetch(0));
	if (cast1 instanceof BeforeSequence) {
		BeforeSequence before = (BeforeSequence) cast1;
		return SequenceRegion.INCLUSIVE();
	}
	else if (cast1 instanceof AfterSequence) {
		AfterSequence after = (AfterSequence) cast1;
		return SequenceRegion.EXCLUSIVE();
	}
	else if (cast1 instanceof BeforeSequencePrefix) {
		BeforeSequencePrefix prefix = (BeforeSequencePrefix) cast1;
		return SequenceRegion.PREFIX();
	}
	return -1;
/*
udanax-top.st:69602:SequenceRegion methodsFor: 'accessing'!
{Int32 CLIENT} lowerEdgeType
	"Essential. The kind of Sequence associated with the lower edge of the Region. If SequenceRegion::inclusive then it includes the lowerEdge; if exclusive, then it does not; if prefix, then it includes any Sequence matching each integer in the lowerEdge up to and including lowerEdgePrefixLimit."
	
	myTransitionsCount = Int32Zero ifTrue:
		[Heaper BLAST: #InvalidRequest].
	(myTransitions fetch: Int32Zero) cast: BeforeSequence into:
		[ :before | ^SequenceRegion INCLUSIVE]
	cast: AfterSequence into:
		[ :after | ^SequenceRegion EXCLUSIVE]
	cast: BeforeSequencePrefix into:
		[ :prefix | ^SequenceRegion PREFIX].
	^ -1 "compiler fodder"!
*/
}
/**
 * The smallest Sequence such that all the positions in the region are <= it.  Does not
 * necessarily lie in the region.  For example, the region of all numbers < 2.3 has an
 * upperBound of 2.3. Mathematically, this is called the 'least upper bound'.
 */
public Sequence upperBound() {
	return (Sequence) (TheManager.leastUpperBound(this));
/*
udanax-top.st:69615:SequenceRegion methodsFor: 'accessing'!
{Sequence} upperBound
	"The smallest Sequence such that all the positions in the region are <= it.  Does not necessarily lie in the region.  For example, the region of all numbers < 2.3 has an upperBound of 2.3. Mathematically, this is called the 'least upper bound'."
	^(TheManager leastUpperBound: self) cast: Sequence!
*/
}
/**
 * Essential. The Sequence associated with the upper edge of the Region. To find out where
 * the boundary is in relation to this sequence, check upperEdgeType. BLASTS if unbounded
 * below.
 */
public Sequence upperEdge() {
	if (myTransitionsCount == 0) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	return ((SequenceEdge) (myTransitions.fetch(myTransitionsCount - 1))).sequence();
/*
udanax-top.st:69620:SequenceRegion methodsFor: 'accessing'!
{Sequence CLIENT} upperEdge
	"Essential. The Sequence associated with the upper edge of the Region. To find out where the boundary is in relation to this sequence, check upperEdgeType. BLASTS if unbounded below."
	
	myTransitionsCount = Int32Zero ifTrue:
		[Heaper BLAST: #InvalidRequest].
	^((myTransitions fetch: myTransitionsCount - 1) cast: SequenceEdge) sequence!
*/
}
/**
 * Essential. If upperEdgeType is prefix, then it includes a Sequence matching each integer
 * in the upperEdge up to and including upperEdgePrefixLimit.
 */
public int upperEdgePrefixLimit() {
	if (myTransitionsCount == 0) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	Heaper cast1 = (myTransitions.fetch(myTransitionsCount - 1));
	if (cast1 instanceof BeforeSequencePrefix) {
		BeforeSequencePrefix prefix = (BeforeSequencePrefix) cast1;
		prefix.limit();
	}
	return 0;
/*
udanax-top.st:69627:SequenceRegion methodsFor: 'accessing'!
{IntegerVar CLIENT} upperEdgePrefixLimit
	"Essential. If upperEdgeType is prefix, then it includes a Sequence matching each integer in the upperEdge up to and including upperEdgePrefixLimit."
	
	myTransitionsCount = Int32Zero ifTrue:
		[Heaper BLAST: #InvalidRequest].
	(myTransitions fetch: myTransitionsCount - 1) cast: BeforeSequencePrefix into:
		[ :prefix | prefix limit].
	^IntegerVarZero "fodder"!
*/
}
/**
 * Essential. The kind of Sequence associated with the upper edge of the Region. If
 * SequenceRegion::inclusive then it includes the upperEdge; if exclusive, then it does not;
 * if prefix, then it includes any Sequence matching each integer in the upperEdge up to and
 * including upperEdgePrefixLimit.
 */
public int upperEdgeType() {
	if (myTransitionsCount == 0) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	Heaper cast1 = (myTransitions.fetch(0));
	if (cast1 instanceof BeforeSequence) {
		BeforeSequence before = (BeforeSequence) cast1;
		return SequenceRegion.EXCLUSIVE();
	}
	else if (cast1 instanceof AfterSequence) {
		AfterSequence after = (AfterSequence) cast1;
		return SequenceRegion.INCLUSIVE();
	}
	else if (cast1 instanceof BeforeSequencePrefix) {
		BeforeSequencePrefix prefix = (BeforeSequencePrefix) cast1;
		return SequenceRegion.PREFIX();
	}
	return -1;
/*
udanax-top.st:69636:SequenceRegion methodsFor: 'accessing'!
{Int32 CLIENT} upperEdgeType
	"Essential. The kind of Sequence associated with the upper edge of the Region. If SequenceRegion::inclusive then it includes the upperEdge; if exclusive, then it does not; if prefix, then it includes any Sequence matching each integer in the upperEdge up to and including upperEdgePrefixLimit."
	
	myTransitionsCount = Int32Zero ifTrue:
		[Heaper BLAST: #InvalidRequest].
	(myTransitions fetch: Int32Zero) cast: BeforeSequence into:
		[ :before | ^SequenceRegion EXCLUSIVE]
	cast: AfterSequence into:
		[ :after | ^SequenceRegion INCLUSIVE]
	cast: BeforeSequencePrefix into:
		[ :prefix | ^SequenceRegion PREFIX].
	^ -1 "compiler fodder"!
*/
}
public int actualHashForEqual() {
	return (getCategory().hashForEqual() ^ (myTransitions.elementsHash(myTransitionsCount))) ^ ((myStartsInside) ? 255 : 0);
/*
udanax-top.st:69651:SequenceRegion methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^(self getCategory hashForEqual
		bitXor: (myTransitions elementsHash: myTransitionsCount))
		bitXor: (myStartsInside ifTrue: [255] ifFalse: [UInt32Zero])!
*/
}
public boolean hasMember(Position position) {
	return TheManager.hasMember(this, position);
/*
udanax-top.st:69657:SequenceRegion methodsFor: 'testing'!
{BooleanVar} hasMember: position {Position} 
	^TheManager hasMember: self with: position!
*/
}
/**
 * Same meaning as IntegerRegion::isBoundedAbove
 */
public boolean isBoundedAbove() {
	return TheManager.isBoundedRight(this);
/*
udanax-top.st:69661:SequenceRegion methodsFor: 'testing'!
{BooleanVar CLIENT INLINE} isBoundedAbove
	"Same meaning as IntegerRegion::isBoundedAbove"
	
	^TheManager isBoundedRight: self!
*/
}
/**
 * Same meaning as IntegerRegion::isBoundedBelow
 */
public boolean isBoundedBelow() {
	return TheManager.isBoundedLeft(this);
/*
udanax-top.st:69666:SequenceRegion methodsFor: 'testing'!
{BooleanVar CLIENT INLINE} isBoundedBelow
	"Same meaning as IntegerRegion::isBoundedBelow"
	
	^TheManager isBoundedLeft: self!
*/
}
public boolean isEmpty() {
	return TheManager.isEmpty(this);
/*
udanax-top.st:69671:SequenceRegion methodsFor: 'testing'!
{BooleanVar} isEmpty
	^TheManager isEmpty: self!
*/
}
public boolean isEnumerable(OrderSpec order) {
	return isFinite();
/*
udanax-top.st:69675:SequenceRegion methodsFor: 'testing'!
{BooleanVar} isEnumerable: order {OrderSpec unused default: NULL}
	^self isFinite!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof SequenceRegion) {
		SequenceRegion region = (SequenceRegion) other;
		return myStartsInside == region.startsInside() && (myTransitionsCount == region.secretTransitionsCount() && (myTransitions.elementsEqual(0, region.secretTransitions(), 0, myTransitionsCount)));
	}
	else {
		return false;
	}
/*
udanax-top.st:69679:SequenceRegion methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other cast: SequenceRegion into: [ :region |
		^myStartsInside == region startsInside
			and: [myTransitionsCount == region secretTransitionsCount
			and: [myTransitions elementsEqual: Int32Zero with: region secretTransitions with: Int32Zero with: myTransitionsCount]]]
	others:
		[^false].
	^ false "compiler fodder"!
*/
}
public boolean isFinite() {
	return TheManager.isFinite(this);
/*
udanax-top.st:69689:SequenceRegion methodsFor: 'testing'!
{BooleanVar} isFinite
	^TheManager isFinite: self!
*/
}
public boolean isFull() {
	return TheManager.isFull(this);
/*
udanax-top.st:69693:SequenceRegion methodsFor: 'testing'!
{BooleanVar} isFull
	^TheManager isFull: self!
*/
}
public boolean isSimple() {
	return TheManager.isSimple(this);
/*
udanax-top.st:69697:SequenceRegion methodsFor: 'testing'!
{BooleanVar} isSimple
	^TheManager isSimple: self!
*/
}
public boolean isSubsetOf(XnRegion other) {
	return TheManager.isSubsetOf(this, other);
/*
udanax-top.st:69701:SequenceRegion methodsFor: 'testing'!
{BooleanVar} isSubsetOf: other {XnRegion}
	
	^TheManager isSubsetOf: self with: other!
*/
}
public Stepper actualStepper(OrderSpec order) {
	shouldNotImplement();
	return null;
/*
udanax-top.st:69707:SequenceRegion methodsFor: 'protected: enumerating'!
{Stepper of: Position} actualStepper: order {OrderSpec} 
	self shouldNotImplement.
	^NULL "fodder"!
*/
}
public int count() {
	return TheManager.count(this);
/*
udanax-top.st:69714:SequenceRegion methodsFor: 'enumerating'!
{IntegerVar} count
	^TheManager count: self!
*/
}
public ScruSet distinctions() {
	return TheManager.distinctions(this);
/*
udanax-top.st:69718:SequenceRegion methodsFor: 'enumerating'!
{ScruSet of: XnRegion} distinctions
	^TheManager distinctions: self!
*/
}
/**
 * Essential. Break this up into disjoint intervals
 */
public Stepper intervals(OrderSpec order) {
	return simpleRegions(order);
/*
udanax-top.st:69722:SequenceRegion methodsFor: 'enumerating'!
{Stepper CLIENT INLINE of: SequenceRegion} intervals: order {OrderSpec default: NULL}
	"Essential. Break this up into disjoint intervals"
	
	^ self simpleRegions: order!
*/
}
/**
 * Whether this Region is a non-empty interval, i.e. if A, B in the Region and A <= C <= B
 * then C is in the Region. This includes inequalities (e.g. {x | x > 5.3}) and the
 * fullRegion in addition to ordinary two-ended intervals.
 */
public boolean isInterval() {
	return isSimple();
/*
udanax-top.st:69727:SequenceRegion methodsFor: 'enumerating'!
{BooleanVar CLIENT INLINE} isInterval
	"Whether this Region is a non-empty interval, i.e. if A, B in the Region and A <= C <= B then C is in the Region. This includes inequalities (e.g. {x | x > 5.3}) and the fullRegion in addition to ordinary two-ended intervals."
	
	^self isSimple!
*/
}
public Stepper simpleRegions(OrderSpec order) {
	return TheManager.simpleRegions(this, order);
/*
udanax-top.st:69732:SequenceRegion methodsFor: 'enumerating'!
{Stepper} simpleRegions: order {OrderSpec default: NULL} 
	^TheManager simpleRegions: self with: order!
*/
}
public Stepper stepper(OrderSpec order) {
	if ( ! (isFinite())) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_ENUMERABLE);
	}
	return new SequenceStepper(myTransitions, myTransitionsCount);
/*
udanax-top.st:69736:SequenceRegion methodsFor: 'enumerating'!
{Stepper of: Position} stepper: order {OrderSpec default: NULL} 
	self isFinite ifFalse:
		[Heaper BLAST: #NotEnumerable].
	^SequenceStepper create: myTransitions with: myTransitionsCount.!
*/
}
public XnRegion complement() {
	return TheManager.complement(this);
/*
udanax-top.st:69744:SequenceRegion methodsFor: 'operations'!
{XnRegion} complement
	^TheManager complement: self!
*/
}
public XnRegion intersect(XnRegion other) {
	return TheManager.intersect(this, other);
/*
udanax-top.st:69748:SequenceRegion methodsFor: 'operations'!
{XnRegion} intersect: other {XnRegion} 
	^TheManager intersect: self with: other!
*/
}
public XnRegion simpleUnion(XnRegion other) {
	return TheManager.simpleUnion(this, other);
/*
udanax-top.st:69752:SequenceRegion methodsFor: 'operations'!
{XnRegion} simpleUnion: other {XnRegion} 
	^TheManager simpleUnion: self with: other!
*/
}
public XnRegion unionWith(XnRegion other) {
	return TheManager.unionWith(this, other);
/*
udanax-top.st:69756:SequenceRegion methodsFor: 'operations'!
{XnRegion} unionWith: other {XnRegion} 
	^TheManager unionWith: self with: other!
*/
}
public XnRegion with(Position pos) {
	return TheManager.with(this, pos);
/*
udanax-top.st:69760:SequenceRegion methodsFor: 'operations'!
{XnRegion} with: pos {Position} 
	^TheManager with: self with: pos!
*/
}
public void printOn(PrintWriter oo) {
	TheManager.printRegionOn(this, oo);
/*
udanax-top.st:69766:SequenceRegion methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	TheManager printRegionOn: self with: oo!
*/
}
public PtrArray secretTransitions() {
	return myTransitions;
/*
udanax-top.st:69772:SequenceRegion methodsFor: 'secret'!
{PtrArray INLINE of: SequenceEdge} secretTransitions
	^myTransitions!
*/
}
public int secretTransitionsCount() {
	return myTransitionsCount;
/*
udanax-top.st:69776:SequenceRegion methodsFor: 'secret'!
{Int32 INLINE} secretTransitionsCount
	^myTransitionsCount!
*/
}
public boolean startsInside() {
	return myStartsInside;
/*
udanax-top.st:69780:SequenceRegion methodsFor: 'secret'!
{BooleanVar INLINE} startsInside
	^myStartsInside!
*/
}
public Stepper intervals() {
	return intervals(null);
/*
udanax-top.st:69786:SequenceRegion methodsFor: 'smalltalk: defaults'!
{Stepper CLIENT of: SequenceRegion} intervals
	^self intervals: NULL!
*/
}
public void receiveSequenceRegion(Rcvr rcvr) {
	myTransitions = PtrArray.nulls(myTransitionsCount);
	for (int i = 0; i < myTransitionsCount; i ++ ) {
		myTransitions.store(i, rcvr.receiveHeaper());
	}
/*
udanax-top.st:69791:SequenceRegion methodsFor: 'hooks:'!
{void RECEIVE.HOOK} receiveSequenceRegion: rcvr {Rcvr}
	myTransitions := PtrArray nulls: myTransitionsCount.
	Int32Zero almostTo: myTransitionsCount do: [:i {Int32} |
		myTransitions at: i store: rcvr receiveHeaper]!
*/
}
public void sendSequenceRegion(Xmtr xmtr) {
	for (int i = 0; i < myTransitionsCount; i ++ ) {
		xmtr.sendHeaper((myTransitions.fetch(i)));
	}
/*
udanax-top.st:69796:SequenceRegion methodsFor: 'hooks:'!
{void SEND.HOOK} sendSequenceRegion: xmtr {Xmtr}
	Int32Zero almostTo: myTransitionsCount do: [:i {Int32} |
		xmtr sendHeaper: (myTransitions fetch: i)]!
*/
}
public SequenceRegion(Rcvr receiver) {
	super(receiver);
	myStartsInside = receiver.receiveBooleanVar();
	myTransitionsCount = receiver.receiveInt32();
	receiveSequenceRegion(receiver);
/*
udanax-top.st:69802:SequenceRegion methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myStartsInside _ receiver receiveBooleanVar.
	myTransitionsCount _ receiver receiveInt32.
	self receiveSequenceRegion: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendBooleanVar(myStartsInside);
	xmtr.sendInt32(myTransitionsCount);
	sendSequenceRegion(xmtr);
/*
udanax-top.st:69808:SequenceRegion methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendBooleanVar: myStartsInside.
	xmtr sendInt32: myTransitionsCount.
	self sendSequenceRegion: xmtr.!
*/
}
/**
 * @deprecated
 */
public static SequenceRegion with(Sequence sequence) {
	throw new PasseException();
/*
udanax-top.st:69831:SequenceRegion class methodsFor: 'smalltalk: passe'!
{SequenceRegion} with: sequence {Sequence}
	self passe.
	^self create: false
		with: (PrimSpec pointer arrayWithTwo: (BeforeSequence make: sequence)
			with: (AfterSequence make: sequence))!
*/
}
public static SequenceRegion above(Sequence sequence) {
	return new SequenceRegion(false, ((PtrArray) (PrimSpec.pointer().arrayWith((BeforeSequence.make(sequence))))));
/*
udanax-top.st:69839:SequenceRegion class methodsFor: 'pseudo constructors'!
{SequenceRegion} above: sequence {Sequence}
	^self create: false
		with: ((PrimSpec pointer arrayWith: (BeforeSequence make: sequence)) cast: PtrArray)!
*/
}
public static SequenceRegion below(Sequence sequence) {
	return new SequenceRegion(true, ((PtrArray) (PrimSpec.pointer().arrayWith((AfterSequence.make(sequence))))));
/*
udanax-top.st:69844:SequenceRegion class methodsFor: 'pseudo constructors'!
{SequenceRegion} below: sequence {Sequence}
	^self create: true
		with: ((PrimSpec pointer arrayWith: (AfterSequence make: sequence)) cast: PtrArray)!
*/
}
public static SequenceRegion empty() {
	return TheEmptySequenceRegion;
/*
udanax-top.st:69849:SequenceRegion class methodsFor: 'pseudo constructors'!
{SequenceRegion INLINE} empty
	
	^TheEmptySequenceRegion!
*/
}
public static SequenceRegion full() {
	return TheFullSequenceRegion;
/*
udanax-top.st:69853:SequenceRegion class methodsFor: 'pseudo constructors'!
{SequenceRegion INLINE} full
	
	^TheFullSequenceRegion!
*/
}
/**
 * All sequences matching the given up to and including the number at limit
 */
public static SequenceRegion prefixedBy(Sequence sequence, int limit) {
	return new SequenceRegion(false, ((PtrArray) (PrimSpec.pointer().arrayWithTwo((BeforeSequencePrefix.below(sequence, limit)), (BeforeSequencePrefix.above(sequence, limit))))));
/*
udanax-top.st:69857:SequenceRegion class methodsFor: 'pseudo constructors'!
{SequenceRegion} prefixedBy: sequence {Sequence} with: limit {IntegerVar}
	"All sequences matching the given up to and including the number at limit"
	
	^self create: false
		     with: ((PrimSpec pointer
					arrayWithTwo: (BeforeSequencePrefix below: sequence with: limit)
						       with: (BeforeSequencePrefix above: sequence with: limit)) cast: PtrArray)!
*/
}
public static SequenceRegion strictlyAbove(Sequence sequence) {
	return new SequenceRegion(false, ((PtrArray) (PrimSpec.pointer().arrayWith((AfterSequence.make(sequence))))));
/*
udanax-top.st:69865:SequenceRegion class methodsFor: 'pseudo constructors'!
{SequenceRegion} strictlyAbove: sequence {Sequence}
	^self create: false
		with: ((PrimSpec pointer arrayWith: (AfterSequence make: sequence)) cast: PtrArray)!
*/
}
public static SequenceRegion strictlyBelow(Sequence sequence) {
	return new SequenceRegion(true, ((PtrArray) (PrimSpec.pointer().arrayWith((BeforeSequence.make(sequence))))));
/*
udanax-top.st:69870:SequenceRegion class methodsFor: 'pseudo constructors'!
{SequenceRegion} strictlyBelow: sequence {Sequence}
	^self create: true
		with: ((PrimSpec pointer arrayWith: (BeforeSequence make: sequence)) cast: PtrArray)!
*/
}
public static void initTimeNonInherited() {
	TheManager = new SequenceManager();
	TheEmptySequenceRegion = new SequenceRegion(false, PtrArray.empty());
	TheFullSequenceRegion = new SequenceRegion(true, PtrArray.empty());
/*
udanax-top.st:69877:SequenceRegion class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	
	self REQUIRES: EdgeManager.
	TheManager := SequenceManager create.
	self REQUIRES: Sequence.
	self REQUIRES: SequenceSpace.
	self REQUIRES: PtrArray.
	TheEmptySequenceRegion := self create: false with: PtrArray empty.
	TheFullSequenceRegion := self create: true with: PtrArray empty.!
*/
}
public static void linkTimeNonInherited() {
	TheManager = null;
	TheEmptySequenceRegion = null;
	TheFullSequenceRegion = null;
/*
udanax-top.st:69887:SequenceRegion class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	
	TheManager := NULL.
	TheEmptySequenceRegion := NULL.
	TheFullSequenceRegion := NULL.!
*/
}
/**
 * Make a new region, reusing the given array. No one else should ever modify it!!
 */
public static SequenceRegion usingx(boolean startsInside, PtrArray transitions) {
	return new SequenceRegion(startsInside, transitions);
/*
udanax-top.st:69895:SequenceRegion class methodsFor: 'private:'!
{SequenceRegion} usingx: startsInside {BooleanVar}
	with: transitions {PtrArray of: TransitionEdge}
	"Make a new region, reusing the given array. No one else should ever modify it!!"
	^self create: startsInside with: transitions!
*/
}
public static int EXCLUSIVE() {
	return 2;
/*
udanax-top.st:69903:SequenceRegion class methodsFor: 'constants'!
{Int32 constFn CLIENT INLINE} EXCLUSIVE
	^2!
*/
}
public static int INCLUSIVE() {
	return 1;
/*
udanax-top.st:69906:SequenceRegion class methodsFor: 'constants'!
{Int32 constFn CLIENT INLINE} INCLUSIVE
	^1!
*/
}
public static int PREFIX() {
	return 3;
/*
udanax-top.st:69909:SequenceRegion class methodsFor: 'constants'!
{Int32 constFn CLIENT INLINE} PREFIX
	^3!
*/
}
/**
 * {Int32 constFn CLIENT INLINE} EXCLUSIVE
 * {Int32 constFn CLIENT INLINE} INCLUSIVE
 * {Int32 constFn CLIENT INLINE} PREFIX
 * {Stepper CLIENT of: SequenceRegion} intervals: order {OrderSpec default: NULL}
 * {BooleanVar CLIENT} isBoundedAbove
 * {BooleanVar CLIENT} isBoundedBelow
 * {BooleanVar CLIENT} isInterval
 * {Sequence CLIENT} lowerEdge
 * {IntegerVar CLIENT} lowerEdgePrefixLimit
 * {Int32 CLIENT} lowerEdgeType
 * {Sequence CLIENT} upperEdge
 * {IntegerVar CLIENT} upperEdgePrefixLimit
 * {Int32 CLIENT} upperEdgeType
 */
public static void infostProtocol() {
/*
udanax-top.st:69914:SequenceRegion class methodsFor: 'smalltalk: system'!
info.stProtocol
"{Int32 constFn CLIENT INLINE} EXCLUSIVE
{Int32 constFn CLIENT INLINE} INCLUSIVE
{Int32 constFn CLIENT INLINE} PREFIX
{Stepper CLIENT of: SequenceRegion} intervals: order {OrderSpec default: NULL}
{BooleanVar CLIENT} isBoundedAbove
{BooleanVar CLIENT} isBoundedBelow
{BooleanVar CLIENT} isInterval
{Sequence CLIENT} lowerEdge
{IntegerVar CLIENT} lowerEdgePrefixLimit
{Int32 CLIENT} lowerEdgeType
{Sequence CLIENT} upperEdge
{IntegerVar CLIENT} upperEdgePrefixLimit
{Int32 CLIENT} upperEdgeType
"!
*/
}
public SequenceRegion() {
/*

Generated during transformation
*/
}
}
