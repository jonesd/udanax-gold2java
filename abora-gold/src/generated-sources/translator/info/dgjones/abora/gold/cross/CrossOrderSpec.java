/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cross;

import info.dgjones.abora.gold.collection.basic.PrimIntArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.cross.CrossOrderSpec;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.CrossSpace;
import info.dgjones.abora.gold.spaces.cross.GenericCrossRegion;
import info.dgjones.abora.gold.spaces.cross.GenericCrossSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * myLexOrder lists the lexicographic order in which each dimension should be processed.
 * Every dimension should be listed exactly one, from most significant (at index 0) to least
 * significant.
 * mySubOrders are indexed by *dimension*, not by lexicographic order.  In order to index by
 * lex order, look up the dimension in myLexOrder, and then look up the resulting dimension
 * number in mySubOrders.
 */
public class CrossOrderSpec extends OrderSpec {

	protected CrossSpace mySpace;
	protected PtrArray mySubOrders;
	protected PrimIntArray myLexOrder;
/*
udanax-top.st:30597:
OrderSpec subclass: #CrossOrderSpec
	instanceVariableNames: '
		mySpace {CrossSpace}
		mySubOrders {PtrArray of: OrderSpec}
		myLexOrder {PrimIntArray}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cross'!
*/
/*
udanax-top.st:30604:
CrossOrderSpec comment:
'myLexOrder lists the lexicographic order in which each dimension should be processed.  Every dimension should be listed exactly one, from most significant (at index 0) to least significant.
mySubOrders are indexed by *dimension*, not by lexicographic order.  In order to index by lex order, look up the dimension in myLexOrder, and then look up the resulting dimension number in mySubOrders.'!
*/
/*
udanax-top.st:30608:
(CrossOrderSpec getOrMakeCxxClassDescription)
	friends:
'friend class GenericCrossSpace;
';
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
*/
/*
udanax-top.st:30703:
CrossOrderSpec class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:30706:
(CrossOrderSpec getOrMakeCxxClassDescription)
	friends:
'friend class GenericCrossSpace;
';
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CrossOrderSpec.class).setAttributes( new Set().add("CONCRETE").add("ONCLIENT").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public CrossOrderSpec(CrossSpace space, PtrArray subOrders, PrimIntArray lexOrder) {
	super();
	mySpace = space;
	mySubOrders = subOrders;
	myLexOrder = lexOrder;
/*
udanax-top.st:30616:CrossOrderSpec methodsFor: 'private: creation'!
create: space {CrossSpace} with: subOrders {PtrArray of: OrderSpec} with: lexOrder {PrimIntArray}
	super create.
	mySpace := space.
	mySubOrders := subOrders.
	myLexOrder := lexOrder!
*/
}
public CoordinateSpace coordinateSpace() {
	return mySpace;
/*
udanax-top.st:30625:CrossOrderSpec methodsFor: 'accessing'!
{CoordinateSpace INLINE} coordinateSpace
	
	^mySpace!
*/
}
/**
 * Lists the lexicographic order in which each dimension should be processed.  Every
 * dimension is listed exactly once, from most significant (at index 0) to least significant.
 */
public PrimIntArray lexOrder() {
	return (PrimIntArray) myLexOrder.copy();
/*
udanax-top.st:30629:CrossOrderSpec methodsFor: 'accessing'!
{PrimIntArray CLIENT} lexOrder
	"Lists the lexicographic order in which each dimension should be processed.  Every dimension is listed exactly once, from most significant (at index 0) to least significant."
	^myLexOrder copy cast: PrimIntArray!
*/
}
/**
 * The sub OrderSpec used for the given axis. Note that this is *not* in lex order.
 */
public OrderSpec subOrder(int i) {
	return (OrderSpec) (mySubOrders.fetch(i));
/*
udanax-top.st:30634:CrossOrderSpec methodsFor: 'accessing'!
{OrderSpec CLIENT} subOrder: i {Int32}
	"The sub OrderSpec used for the given axis. Note that this is *not* in lex order."
	
	^(mySubOrders fetch: i) cast: OrderSpec!
*/
}
/**
 * The sub OrderSpec used for each axis in the space. Note that this is *not* in lex order,
 * but rather indexed by axis number.
 */
public PtrArray subOrders() {
	return (PtrArray) mySubOrders.copy();
/*
udanax-top.st:30639:CrossOrderSpec methodsFor: 'accessing'!
{PtrArray CLIENT of: OrderSpec} subOrders
	"The sub OrderSpec used for each axis in the space. Note that this is *not* in lex order, but rather indexed by axis number."
	
	^mySubOrders copy cast: PtrArray!
*/
}
public int actualHashForEqual() {
	return mySpace.hashForEqual() ^ (mySubOrders.hashForEqual() ^ myLexOrder.hashForEqual());
/*
udanax-top.st:30646:CrossOrderSpec methodsFor: 'testing'!
{UInt32} actualHashForEqual
	
	^mySpace hashForEqual bitXor: (mySubOrders hashForEqual  bitXor: myLexOrder hashForEqual ).!
*/
}
public boolean follows(Position x, Position y) {
	MarkM.shouldImplement();
	return false;
/*
udanax-top.st:30651:CrossOrderSpec methodsFor: 'testing'!
{BooleanVar} follows: x {Position unused} with: y {Position unused}
	
	MarkM shouldImplement.
	^false "fodder"!
*/
}
public boolean isEqual(Heaper other) {
	Someone.shouldImplement();
	return false;
/*
udanax-top.st:30656:CrossOrderSpec methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper unused}
	Someone shouldImplement.
	^false "fodder"!
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
	return false;
/*
udanax-top.st:30661:CrossOrderSpec methodsFor: 'testing'!
{BooleanVar} isFullOrder: keys {XnRegion unused default: NULL}
	"Essential.  If this returns TRUE, then I define a full order over all positions in 'keys' (or all positions in the space if 'keys' is NULL).  However, if I return FALSE, that doesn't guarantee that I don't define a full ordering.  I may happen to define a full ordering without knowing it.
	
	A full ordering is one in which for each a, b in keys; either this->follows(a, b) or this->follows(b, a)."
	^false. "any 2 d or greater space has no fullordering"
	"Someone shouldImplement."
	"fodder"!
*/
}
/**
 * Return true if some position in before is less than or equal to all positions in after.
 */
public boolean preceeds(XnRegion before, XnRegion after) {
	if (before instanceof GenericCrossRegion) {
		GenericCrossRegion bc = (GenericCrossRegion) before;
		if (after instanceof GenericCrossRegion) {
			GenericCrossRegion ac = (GenericCrossRegion) after;
			for (int i = 0; i < myLexOrder.count(); i ++ ) {
				int dim;
				OrderSpec sub;
				dim = (myLexOrder.integerAt(i));
				sub = (OrderSpec) (mySubOrders.get(dim));
				for (int bi = 0; bi < bc.boxCount(); bi ++ ) {
					XnRegion bp;
					bp = bc.boxProjection(bi, dim);
					for (int ai = 0; ai < ac.boxCount(); ai ++ ) {
						XnRegion ap;
						ap = ac.boxProjection(ai, dim);
						if (sub.preceeds(bp, ap)) {
							return true;
						}
					}
				}
			}
			return false;
		}
		else {
			throw new UnimplementedException();
		}
	}
	else {
		throw new UnimplementedException();
	}
/*
udanax-top.st:30669:CrossOrderSpec methodsFor: 'testing'!
{BooleanVar} preceeds: before {XnRegion} with: after {XnRegion}
	"Return true if some position in before is less than or equal to all positions in after."
	
	before cast: GenericCrossRegion into: [ :bc |
	after cast: GenericCrossRegion into: [ :ac |
			Int32Zero almostTo: myLexOrder count do: [ :i {Int32} |
				| dim {Int32} sub {OrderSpec} |
				dim := (myLexOrder integerAt: i) DOTasLong.
				sub := (mySubOrders get: dim) cast: OrderSpec.
				Int32Zero almostTo: bc boxCount do: [ :bi {Int32} | | bp {XnRegion} |
					bp := bc boxProjection: bi with: dim.
					Int32Zero almostTo: ac boxCount do: [ :ai {Int32} | | ap {XnRegion} |
					ap := ac boxProjection: ai with: dim.
					(sub preceeds: bp with: ap) ifTrue: [^true]]]].
				^false]
	others: [self unimplemented]]
	others: [self unimplemented].
	^false "fodder"!
*/
}
public CrossOrderSpec(Rcvr receiver) {
	super(receiver);
	mySpace = (CrossSpace) receiver.receiveHeaper();
	mySubOrders = (PtrArray) receiver.receiveHeaper();
	myLexOrder = (PrimIntArray) receiver.receiveHeaper();
/*
udanax-top.st:30690:CrossOrderSpec methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	mySpace _ receiver receiveHeaper.
	mySubOrders _ receiver receiveHeaper.
	myLexOrder _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(mySpace);
	xmtr.sendHeaper(mySubOrders);
	xmtr.sendHeaper(myLexOrder);
/*
udanax-top.st:30696:CrossOrderSpec methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: mySpace.
	xmtr sendHeaper: mySubOrders.
	xmtr sendHeaper: myLexOrder.!
*/
}
public static CrossOrderSpec make(CrossSpace space, PtrArray subOrderings, PrimIntArray lexOrder) {
	PrimIntArray lexO;
	PtrArray subOrders;
	subOrders = PtrArray.nulls(space.axisCount());
	for (int i = 0; i < subOrders.count(); i ++ ) {
		subOrders.store(i, (space.axis(i)).fetchAscending());
	}
	if (subOrderings != null) {
		for (int i1 = 0; i1 < subOrders.count(); i1 ++ ) {
			OrderSpec subOrder;
			subOrder = (OrderSpec) (subOrderings.fetch(i1));
			if (subOrder == null) {
				if ( ! ((subOrders.fetch(i1)) != null)) {
					throw new AboraAssertionException("Must have an ordering from each space");
				}
			}
			else {
				subOrders.store(i1, subOrder);
			}
		}
	}
	if (lexOrder == null) {
		lexO = PrimIntArray.zeros(32, subOrders.count());
		for (int i2 = 0; i2 < subOrders.count(); i2 ++ ) {
			lexO.storeInteger(i2, i2);
		}
	}
	else {
		lexO = lexOrder;
	}
	return new CrossOrderSpec(space, subOrders, lexO);
/*
udanax-top.st:30714:CrossOrderSpec class methodsFor: 'pseudoconstructors'!
make: space {CrossSpace} 
	with: subOrderings {(PtrArray of: OrderSpec | NULL) default: NULL} 
	with: lexOrder {PrimIntArray default: NULL}
	| lexO {PrimIntArray} 
	  subOrders {PtrArray of: OrderSpec} |
	  
	subOrders := PtrArray nulls: space axisCount.
	Int32Zero almostTo: subOrders count do: [:i {Int32} |
		subOrders at: i store: (space axis: i) fetchAscending].
	
	subOrderings ~~ NULL ifTrue:
		[Int32Zero almostTo: subOrders count do: [:i {Int32} |
			| subOrder {OrderSpec | NULL} |
			subOrder := (subOrderings fetch: i) cast: OrderSpec.
			subOrder == NULL ifTrue:
				[(subOrders fetch: i) ~~ NULL assert: 'Must have an ordering from each space']
			ifFalse:
				[subOrders at: i store: subOrder]]].
	
	lexOrder == NULL ifTrue:
		[lexO := PrimIntArray zeros: 32 with: subOrders count.
		Int32Zero almostTo: subOrders count do: [:i {Int32} |
			lexO at: i storeInteger: i]]
	ifFalse:
		[lexO := lexOrder].
	^self create: space with: subOrders with: lexO!
*/
}
/*
udanax-top.st:30744:CrossOrderSpec class methodsFor: 'smalltalk: defaults'!
make: space
	
	^self make space with: NULL with: NULL!
*/
/*
udanax-top.st:30748:CrossOrderSpec class methodsFor: 'smalltalk: defaults'!
make: space with: subOrderings
	
	^self make space with: subOrderings with: NULL!
*/
/**
 * Only used during construction; must pass the array in explicitly since the space isnt
 * initialized yet
 */
public static CrossOrderSpec fetchAscending(GenericCrossSpace space, PtrArray subSpaces) {
	PtrArray result;
	PrimIntArray lex;
	result = PtrArray.nulls(subSpaces.count());
	lex = PrimIntArray.zeros(32, subSpaces.count());
	for (int dimension = 0; dimension < result.count(); dimension ++ ) {
		OrderSpec sub;
		sub = ((CoordinateSpace) (subSpaces.fetch(dimension))).fetchAscending();
		if (sub == null) {
			return null;
		}
		result.store(dimension, sub);
		lex.storeInteger(dimension, dimension);
	}
	return new CrossOrderSpec(space, result, lex);
/*
udanax-top.st:30754:CrossOrderSpec class methodsFor: 'private: pseudo constructors'!
{CrossOrderSpec} fetchAscending: space {GenericCrossSpace}
	with: subSpaces {PtrArray of: CoordinateSpace}
	"Only used during construction; must pass the array in explicitly since the space isnt initialized yet"
	
	| result {PtrArray of: OrderSpec} lex {PrimIntArray} |
	result := PtrArray nulls: subSpaces count.
	lex := PrimIntArray zeros: 32 with: subSpaces count.
	Int32Zero almostTo: result count do: [ :dimension {Int32} | | sub {OrderSpec} |
		sub := ((subSpaces fetch: dimension) cast: CoordinateSpace) fetchAscending.
		sub == NULL ifTrue:
			[^NULL].
		result at: dimension store: sub.
		lex at: dimension storeInteger: dimension].
	^self create: space with: result with: lex!
*/
}
/**
 * Only used during construction; must pass the array in explicitly since the space isnt
 * initialized yet
 */
public static CrossOrderSpec fetchDescending(GenericCrossSpace space, PtrArray subSpaces) {
	PtrArray result;
	PrimIntArray lex;
	result = PtrArray.nulls(subSpaces.count());
	lex = PrimIntArray.zeros(32, subSpaces.count());
	for (int dimension = 0; dimension < result.count(); dimension ++ ) {
		OrderSpec sub;
		sub = ((CoordinateSpace) (subSpaces.fetch(dimension))).fetchAscending();
		if (sub == null) {
			return null;
		}
		result.store(dimension, sub);
		lex.storeInteger(dimension, dimension);
	}
	return new CrossOrderSpec(space, result, lex);
/*
udanax-top.st:30769:CrossOrderSpec class methodsFor: 'private: pseudo constructors'!
{CrossOrderSpec} fetchDescending: space {GenericCrossSpace}
	with: subSpaces {PtrArray of: CoordinateSpace}
	"Only used during construction; must pass the array in explicitly since the space isnt initialized yet"
	
	| result {PtrArray of: OrderSpec} lex {PrimIntArray} |
	result := PtrArray nulls: subSpaces count.
	lex := PrimIntArray zeros: 32 with: subSpaces count.
	Int32Zero almostTo: result count do: [ :dimension {Int32} | | sub {OrderSpec} |
		sub := ((subSpaces fetch: dimension) cast: CoordinateSpace) fetchAscending.
		sub == NULL ifTrue:
			[^NULL].
		result at: dimension store: sub.
		lex at: dimension storeInteger: dimension].
	^self create: space with: result with: lex!
*/
}
/**
 * {Int32Array CLIENT} lexOrder
 * {OrderSpec CLIENT} subOrder: i {Int32}
 * {PtrArray CLIENT of: OrderSpec} subOrders
 */
public static void infostProtocol() {
/*
udanax-top.st:30786:CrossOrderSpec class methodsFor: 'smalltalk: system'!
info.stProtocol
"{Int32Array CLIENT} lexOrder
{OrderSpec CLIENT} subOrder: i {Int32}
{PtrArray CLIENT of: OrderSpec} subOrders
"!
*/
}
public CrossOrderSpec() {
/*

Generated during transformation
*/
}
public static CrossOrderSpec make(CrossSpace space) {
	return make(space, null );
/*

Generated during transformation: AddDefaultParameter
*/
}
public static CrossOrderSpec make(CrossSpace space, PtrArray subOrderings) {
	return make(space, subOrderings, null );
/*

Generated during transformation: AddDefaultParameter
*/
}
}
