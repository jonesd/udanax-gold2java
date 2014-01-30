/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.basic;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.DisjointRegionStepper;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.Signal;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * The design of a new coordinate space consists mostly in the design of the XuRegions which
 * can be used to describe (possibly infinite) sets of positions in that coordinate space.
 * It will generally not be the case (for a given coordinate space) that all mathematically
 * describable sets of positions will be representable by an XuRegion in that space.  This
 * should not be seen as a temporary deficiency of the current implementation of a space, but
 * rather part of the design of what a given space *means*.
 * For example, in IntegerSpace, one cannot form the XuRegion whose members are exactly the
 * even numbers.  If this were possible, other desirable properties which are part of the
 * intent of IntegerSpaces would no longer be possible.  For example, any XuRegion should be
 * able to break itself up into a finite number of simple XuRegions ("simple" is described
 * below).  Were an even number region possible, this would have undesirable consequences for
 * the definition of "simple" in this space.  If you want (for example) to be able to have a
 * XuRegion which can represent all the even numbers, it probably makes more sense to define
 * a whole new space in which these new XuRegions apply.
 * XuRegions should be closed under a large set of operations, such as intersection,
 * unionWith, complement and minus.  ("closed" means that the result of performing this
 * operation on XuRegions of a given space is another valid XuRegion in the same space.)
 * Additional guarantees are documented with each operation.
 * A XuRegion may be classified at one of three levels of "simplicity":
 * 1) The simplest are the *distinctions*.  Distinctions are those that answer with (at most)
 * a single set containing themselves in response to the message "distinctions".  (The reason
 * I say "at most" is that a full region (one that covers the entire coordinate space) may
 * answer with the empty set.)  Distinctions are the simplest XuRegions of a given space out
 * of which all other XuRegions of that space can be finitely composed.  There should
 * probably be a message "isDistinction" for which exactly the distinctions answer "true".
 * The complement of a distinction is a distinction.  Three examples of distinctions in
 * spaces are:
 * a) in IntegerSpace, any simple inequality.  For example, all integers < 37.
 * b) in one kind of 3-space, any half space (all the space on one side of some plane)
 * c) in another kind of 3-space, any sphere or spherical hole.
 * Note that "c" could not just have spheres as the distinction because distinctions must be
 * closed under complement.  (We are here ignoring the quite substantial problems that arise
 * in dealing with approximate (e.g., floating point) which would almost necessarily have to
 * arise in doing any decent 3-space.  3-space is nevertheless a good intuition pump.)
 * 2) Next are the *simple regions*.  Simple regions are exactly those that say "true" to
 * "isSimple".  All distinctions are also simple regions.  In response to the message
 * "distinctions", and simple region must return a finite set of distinctions which, when
 * intersected together, yield the original simple region.  Generally, one tries to define
 * the simple regions for a space to correspond to some notion of locality in the space.  For
 * example, it may be good for a simple region not to be able to have a hole in it.  Or
 * perhaps a simple region is which must be connected (whatever that means in a given space).
 * Example non-distinction simple regions for the above example spaces would be:
 * a) The interval from 3 inclusive to 17 exclusive (intersection of all integers >= 3 and
 * all < 17)
 * b) A convex hull (intersection of half spaces)
 * c) Whatever you get by intersecting a bunch of spheres and sherical holes.
 * The simple regions for both "a" and "b" would be connected, without holes, and even
 * convex.  This follows directly from the definition of our distinctions.  None of these
 * nice properties holds for "c", and this also follows directly from our decision to start
 * with spheres.  "c" is still perfectly valid, just less preferable by some criteria.
 * 3) Finally, there are the regions of a space in general.  Any region must respond to the
 * message "simpleRegions" with a stepper which will produce a finite number of simple
 * regions that, when unioned together, yields the original region.  A simple region will
 * return a stepper that will return at most itself ("at most" because an empty region (which
 * covers no positions) may return an empty stepper).  Example non-simple regions are:
 * a) all integers < 3 and all integers >= 17
 * b) two convex hulls
 * c) two disjoint spheres
 * Note that "a" is the complement of the earlier "a" example, thereby showing why the
 * complement of a simple region isn`t necessarily simple.  Even though the "c" space is so
 * unconstrained in the properties of its simple regions, there is no way to interect a
 * finite number of spheres and spherical holes to produce a pair of disjoint spheres.
 * Therefore the pair is non-simple.  Not all spaces must have non-simple regions (or even
 * non-distinctions).  It is interesting to observe for "b" and "c" that even though there is
 * a natural conversion between their respective positions, (except for the empty and full
 * regions) there is no conversion at all between their respective regions.  The kinds of
 * sets of positions representable in one space is completely different than those
 * representable in the other space.
 * We will use these three example spaces repeatedly in documenting the protocol.
 */
public class XnRegion extends Heaper {

	protected static Signal CantMixCoordSpacesSignal;
	protected static Signal EmptyRegionSignal;
/*
udanax-top.st:65150:
Heaper subclass: #XnRegion
	instanceVariableNames: ''
	classVariableNames: '
		CantMixCoordSpacesSignal {Signal smalltalk} 
		EmptyRegionSignal {Signal smalltalk} '
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Basic'!
*/
/*
udanax-top.st:65156:
XnRegion comment:
'The design of a new coordinate space consists mostly in the design of the XuRegions which can be used to describe (possibly infinite) sets of positions in that coordinate space.  It will generally not be the case (for a given coordinate space) that all mathematically describable sets of positions will be representable by an XuRegion in that space.  This should not be seen as a temporary deficiency of the current implementation of a space, but rather part of the design of what a given space *means*.  
	
	For example, in IntegerSpace, one cannot form the XuRegion whose members are exactly the even numbers.  If this were possible, other desirable properties which are part of the intent of IntegerSpaces would no longer be possible.  For example, any XuRegion should be able to break itself up into a finite number of simple XuRegions ("simple" is described below).  Were an even number region possible, this would have undesirable consequences for the definition of "simple" in this space.  If you want (for example) to be able to have a XuRegion which can represent all the even numbers, it probably makes more sense to define a whole new space in which these new XuRegions apply.
	
	XuRegions should be closed under a large set of operations, such as intersection, unionWith, complement and minus.  ("closed" means that the result of performing this operation on XuRegions of a given space is another valid XuRegion in the same space.)  Additional guarantees are documented with each operation.
	
	A XuRegion may be classified at one of three levels of "simplicity":
	
	1) The simplest are the *distinctions*.  Distinctions are those that answer with (at most) a single set containing themselves in response to the message "distinctions".  (The reason I say "at most" is that a full region (one that covers the entire coordinate space) may answer with the empty set.)  Distinctions are the simplest XuRegions of a given space out of which all other XuRegions of that space can be finitely composed.  There should probably be a message "isDistinction" for which exactly the distinctions answer "true".  The complement of a distinction is a distinction.  Three examples of distinctions in spaces are:
	
		a) in IntegerSpace, any simple inequality.  For example, all integers < 37.
		b) in one kind of 3-space, any half space (all the space on one side of some plane)
		c) in another kind of 3-space, any sphere or spherical hole.
		
	Note that "c" could not just have spheres as the distinction because distinctions must be closed under complement.  (We are here ignoring the quite substantial problems that arise in dealing with approximate (e.g., floating point) which would almost necessarily have to arise in doing any decent 3-space.  3-space is nevertheless a good intuition pump.)
	
	2) Next are the *simple regions*.  Simple regions are exactly those that say "true" to "isSimple".  All distinctions are also simple regions.  In response to the message "distinctions", and simple region must return a finite set of distinctions which, when intersected together, yield the original simple region.  Generally, one tries to define the simple regions for a space to correspond to some notion of locality in the space.  For example, it may be good for a simple region not to be able to have a hole in it.  Or perhaps a simple region is which must be connected (whatever that means in a given space).  Example non-distinction simple regions for the above example spaces would be:
	
		a) The interval from 3 inclusive to 17 exclusive (intersection of all integers >= 3 and all < 17)
		b) A convex hull (intersection of half spaces)
		c) Whatever you get by intersecting a bunch of spheres and sherical holes.
		
	The simple regions for both "a" and "b" would be connected, without holes, and even convex.  This follows directly from the definition of our distinctions.  None of these nice properties holds for "c", and this also follows directly from our decision to start with spheres.  "c" is still perfectly valid, just less preferable by some criteria.
	
	3) Finally, there are the regions of a space in general.  Any region must respond to the message "simpleRegions" with a stepper which will produce a finite number of simple regions that, when unioned together, yields the original region.  A simple region will return a stepper that will return at most itself ("at most" because an empty region (which covers no positions) may return an empty stepper).  Example non-simple regions are:
	
		a) all integers < 3 and all integers >= 17
		b) two convex hulls
		c) two disjoint spheres
		
	Note that "a" is the complement of the earlier "a" example, thereby showing why the complement of a simple region isn`t necessarily simple.  Even though the "c" space is so unconstrained in the properties of its simple regions, there is no way to interect a finite number of spheres and spherical holes to produce a pair of disjoint spheres.  Therefore the pair is non-simple.  Not all spaces must have non-simple regions (or even non-distinctions).  It is interesting to observe for "b" and "c" that even though there is a natural conversion between their respective positions, (except for the empty and full regions) there is no conversion at all between their respective regions.  The kinds of sets of positions representable in one space is completely different than those representable in the other space.
	
	We will use these three example spaces repeatedly in documenting the protocol.'!
*/
/*
udanax-top.st:65190:
(XnRegion getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:65474:
XnRegion class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:65477:
(XnRegion getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(XnRegion.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Return a simple region containing all positions contained by myself.
 * If I am simple, then the result must be me.  Otherwise,
 * the resulting region will contain more positions than I do, but it
 * must contain all those that I do.  It would be good for the resulting
 * simple region to not contain many more points than it needs in order
 * to satisfy these constraints; but this is a preference, not a
 * specification. Particular spaces may specify stronger guarantees,
 * but as far as class XuRegion is concerned it is correct (though silly)
 * for this message to always return the full region for the space.
 */
public XnRegion asSimpleRegion() {
	return simpleUnion(this);
/*
udanax-top.st:65195:XnRegion methodsFor: 'accessing'!
{XnRegion} asSimpleRegion
	"Return a simple region containing all positions contained by myself. 
	If I am simple, then the result must be me.  Otherwise,
	the resulting region will contain more positions than I do, but it 
	must contain all those that I do.  It would be good for the resulting 
	simple region to not contain many more points than it needs in order 
	to satisfy these constraints; but this is a preference, not a 
	specification. Particular spaces may specify stronger guarantees, 
	but as far as class XuRegion is concerned it is correct (though silly) 
	for this message to always return the full region for the space."
	^self simpleUnion: self!
*/
}
/**
 * Essential.  The coordinate space in which this is a region
 */
public CoordinateSpace coordinateSpace() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65208:XnRegion methodsFor: 'accessing'!
{CoordinateSpace CLIENT} coordinateSpace
	"Essential.  The coordinate space in which this is a region"
	
	self subclassResponsibility!
*/
}
/**
 * Essential.  Return a region of containing exactly those positions not in this region. The
 * complement of a distinction must be a distinction.
 */
public XnRegion complement() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65215:XnRegion methodsFor: 'operations'!
{XnRegion CLIENT} complement
	"Essential.  Return a region of containing exactly those positions not in this region. The complement of a distinction must be a distinction."
	self subclassResponsibility!
*/
}
/**
 * The region where they differ.
 * a->delta(b) ->isEqual (a->minus(b)->unionWith(b->minus(a)))
 */
public XnRegion delta(XnRegion region) {
	return (minus(region)).unionWith((region.minus(this)));
/*
udanax-top.st:65220:XnRegion methodsFor: 'operations'!
{XnRegion} delta: region {XnRegion}
	"The region where they differ.  
	a->delta(b) ->isEqual (a->minus(b)->unionWith(b->minus(a)))"
	
	^(self minus: region) unionWith: (region minus: self)!
*/
}
/**
 * Essential.  The intersection of two simple regions must be simple. The intersection of two
 * distinctions must therefore be a simple region. The result has exactly those members which
 * both the original regions have.
 */
public XnRegion intersect(XnRegion other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65226:XnRegion methodsFor: 'operations'!
{XnRegion CLIENT} intersect: other {XnRegion unused} 
	"Essential.  The intersection of two simple regions must be simple. The intersection of two distinctions must therefore be a simple region. The result has exactly those members which both the original regions have."
	self subclassResponsibility!
*/
}
/**
 * The region containing all my position which aren't in other.
 */
public XnRegion minus(XnRegion other) {
	if (other.isEmpty()) {
		return this;
	}
	else {
		return intersect(other.complement());
	}
/*
udanax-top.st:65231:XnRegion methodsFor: 'operations'!
{XnRegion CLIENT} minus: other {XnRegion}
	"The region containing all my position which aren't in other."
	
	other isEmpty
		ifTrue: [^self ]
		ifFalse: [ ^self intersect: other complement ]!
*/
}
/**
 * The result must contain all positions contained by either of the two
 * original regions, and the result must be simple. However, the result
 * may contain additional positions. See the comment on
 * 'XuRegion::asSimpleRegion'.  a->simpleUnion(b) satisfies the same specification
 * as (a->unionWith(b))->asSimpleRegion(). However, the two results do
 * not have to be the same region.
 */
public XnRegion simpleUnion(XnRegion other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65238:XnRegion methodsFor: 'operations'!
{XnRegion} simpleUnion: other {XnRegion unused} 
	"The result must contain all positions contained by either of the two 
	original regions, and the result must be simple. However, the result 
	may contain additional positions. See the comment on 
	'XuRegion::asSimpleRegion'.  a->simpleUnion(b) satisfies the same specification 
	as (a->unionWith(b))->asSimpleRegion(). However, the two results do 
	not have to be the same region."
	self subclassResponsibility!
*/
}
/**
 * The result has as members exactly those positions which are members of either of the
 * original two regions. No matter how simple the two original regions are, the result may be
 * non-simple.
 * The only reason this is called 'unionWith' instead of 'union' is that the latter is a C++
 * keyword.
 */
public XnRegion unionWith(XnRegion other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65248:XnRegion methodsFor: 'operations'!
{XnRegion CLIENT} unionWith: other {XnRegion unused} 
	"The result has as members exactly those positions which are members of either of the original two regions. No matter how simple the two original regions are, the result may be non-simple.
	
	The only reason this is called 'unionWith' instead of 'union' is that the latter is a C++ keyword."
	self subclassResponsibility!
*/
}
/**
 * the region with one more position. Actually, if I already contain pos, then the result is
 * just me.
 */
public XnRegion with(Position pos) {
	return unionWith(pos.asRegion());
/*
udanax-top.st:65255:XnRegion methodsFor: 'operations'!
{XnRegion CLIENT} with: pos {Position} 
	"the region with one more position. Actually, if I already contain pos, then the result is just me."
	^self unionWith: pos asRegion!
*/
}
/**
 * the region with one less position. Actually if I already don't contain pos, then the
 * result is just me.
 */
public XnRegion without(Position pos) {
	return minus(pos.asRegion());
/*
udanax-top.st:65260:XnRegion methodsFor: 'operations'!
{XnRegion CLIENT} without: pos {Position} 
	"the region with one less position. Actually if I already don't contain pos, then the result is just me."
	^self minus: pos asRegion!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:65267:XnRegion methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
/**
 * Do I contain this position? More than anything else, the behavior of this message is the
 * defining characteristic of an XuRegion. All other messages (except for the simplicity
 * characterization) should be specifiable in terms of the behavior of this message. What an
 * XuRegion *is* (mostly) is a finite decision procedure for accepting or rejecting any given
 * position.
 */
public boolean hasMember(Position atPos) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65270:XnRegion methodsFor: 'testing'!
{BooleanVar CLIENT} hasMember: atPos {Position unused} 
	"Do I contain this position? More than anything else, the behavior of this message is the defining characteristic of an XuRegion. All other messages (except for the simplicity characterization) should be specifiable in terms of the behavior of this message. What an XuRegion *is* (mostly) is a finite decision procedure for accepting or rejecting any given position."
	self subclassResponsibility!
*/
}
/**
 * Essential.  tell whether it has any points in common
 */
public boolean intersects(XnRegion other) {
	if (isEmpty()) {
		return false;
	}
	else {
		if (other.isEmpty()) {
			return false;
		}
		else {
			return ! (intersect(other)).isEmpty();
		}
	}
/*
udanax-top.st:65275:XnRegion methodsFor: 'testing'!
{BooleanVar CLIENT} intersects: other {XnRegion} 
	"Essential.  tell whether it has any points in common"
	
	self isEmpty
		ifTrue: [ ^ false ]
		ifFalse:
			[other isEmpty
				ifTrue: [^ false]
				ifFalse: [^(self intersect: other) isEmpty not]]!
*/
}
/**
 * Am I a distinction.  See XuRegion class comment for implications of being a distinction.
 */
public boolean isDistinction() {
	return isSimple() && (distinctions().count() <= 1);
/*
udanax-top.st:65285:XnRegion methodsFor: 'testing'!
{BooleanVar} isDistinction
	"Am I a distinction.  See XuRegion class comment for implications of being a distinction."
	
	^self isSimple and: [self distinctions count <= 1]!
*/
}
/**
 * Every coordinate space has exactly one empty region. It is the one containing no
 * positions. It and only it responds 'true' to this message.
 */
public boolean isEmpty() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65290:XnRegion methodsFor: 'testing'!
{BooleanVar CLIENT} isEmpty
	"Every coordinate space has exactly one empty region. It is the one containing no positions. It and only it responds 'true' to this message."
	self subclassResponsibility!
*/
}
/**
 * Two regions are equal iff they contain exactly the same set of positions
 */
public boolean isEqual(Heaper other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65295:XnRegion methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	"Two regions are equal iff they contain exactly the same set of positions"
	
	self subclassResponsibility!
*/
}
/**
 * Essential.  Do I contain a finite number of positions? If I do, then the 'count' message
 * will say how many, and I will gladly provide a stepper which will step over all of them.
 * I.e., isFinite implies isEnumerable.
 */
public boolean isFinite() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65300:XnRegion methodsFor: 'testing'!
{BooleanVar CLIENT} isFinite
	"Essential.  Do I contain a finite number of positions? If I do, then the 'count' message will say how many, and I will gladly provide a stepper which will step over all of them. I.e., isFinite implies isEnumerable."
	self subclassResponsibility!
*/
}
/**
 * true if this is the largest possible region in this space -- the region that contains all
 * positions in the space. Note that in a space which has no positions (which is perfectly
 * valid), the one XuRegion would be both empty (since it has no positions) and full (since
 * it has all the positions in the space).
 */
public boolean isFull() {
	return complement().isEmpty();
/*
udanax-top.st:65305:XnRegion methodsFor: 'testing'!
{BooleanVar CLIENT} isFull
	"true if this is the largest possible region in this space -- the region that contains all positions in the space. Note that in a space which has no positions (which is perfectly valid), the one XuRegion would be both empty (since it has no positions) and full (since it has all the positions in the space)."
	^self complement isEmpty!
*/
}
/**
 * Am I a simple region.  See XuRegion class comment for implications of being simple.
 */
public boolean isSimple() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65310:XnRegion methodsFor: 'testing'!
{BooleanVar} isSimple
	"Am I a simple region.  See XuRegion class comment for implications of being simple."
	
	self subclassResponsibility!
*/
}
/**
 * I'm a subset of other if I don't have any positions that he doesn't. Note that if we are
 * equal, then I am still a subset of him. If you want to know if I'm a strict subset, you
 * can ask
 * a->isSubsetOf(b) && !! a->isEqual(b)
 */
public boolean isSubsetOf(XnRegion other) {
	return (minus(other)).isEmpty();
/*
udanax-top.st:65315:XnRegion methodsFor: 'testing'!
{BooleanVar CLIENT} isSubsetOf: other {XnRegion} 
	"I'm a subset of other if I don't have any positions that he doesn't. Note that if we are equal, then I am still a subset of him. If you want to know if I'm a strict subset, you can ask 
	a->isSubsetOf(b) && !! a->isEqual(b)"
	^(self minus: other) isEmpty!
*/
}
public XnRegion chooseMany(int n) {
	return chooseMany(n, null);
/*
udanax-top.st:65323:XnRegion methodsFor: 'smalltalk: defaults'!
{XnRegion CLIENT} chooseMany: n {IntegerVar}
	^self chooseMany: n with: NULL!
*/
}
public Position chooseOne() {
	return chooseOne(null);
/*
udanax-top.st:65327:XnRegion methodsFor: 'smalltalk: defaults'!
{Position CLIENT} chooseOne
	^self chooseOne: NULL!
*/
}
/**
 * emulate default argument of NULL
 */
public Stepper disjointSimpleRegions() {
	return disjointSimpleRegions(null);
/*
udanax-top.st:65331:XnRegion methodsFor: 'smalltalk: defaults'!
disjointSimpleRegions
	"emulate default argument of NULL"
	^self disjointSimpleRegions: NULL!
*/
}
/**
 * emulate default argument of NULL
 */
public boolean isEnumerable() {
	return isEnumerable(null);
/*
udanax-top.st:65335:XnRegion methodsFor: 'smalltalk: defaults'!
{BooleanVar} isEnumerable
	"emulate default argument of NULL"
	^self isEnumerable: NULL!
*/
}
/*
udanax-top.st:65339:XnRegion methodsFor: 'smalltalk: defaults'!
{Mapping} mapping: data {PrimArray}
	^self mapping: data with: NULL!
*/
/**
 * emulate default argument of NULL
 */
public Stepper simpleRegions() {
	return simpleRegions(null);
/*
udanax-top.st:65343:XnRegion methodsFor: 'smalltalk: defaults'!
simpleRegions
	"emulate default argument of NULL"
	^self simpleRegions: NULL!
*/
}
/**
 * emulate default argument of NULL
 */
public Stepper stepper() {
	return stepper(null);
/*
udanax-top.st:65347:XnRegion methodsFor: 'smalltalk: defaults'!
{Stepper CLIENT of: Position} stepper
	"emulate default argument of NULL"
	^self stepper: NULL!
*/
}
/**
 * If an OrderSpec is given, return the first n elements according to that OrderSpec. If no
 * OrderSpec is given, then iff I contain at least n positions, return n of them; otherwise
 * BLAST. This should be implemented even by regions that aren't enumerable.  Inspired by the
 * axiom of choice.
 */
public XnRegion chooseMany(int n, OrderSpec order) {
	Someone.shouldImplement();
	return null;
/*
udanax-top.st:65353:XnRegion methodsFor: 'enumerating'!
{XnRegion CLIENT} chooseMany: n {IntegerVar} with: order {OrderSpec default: NULL}
	"If an OrderSpec is given, return the first n elements according to that OrderSpec. If no OrderSpec is given, then iff I contain at least n positions, return n of them; otherwise BLAST. This should be implemented even by regions that aren't enumerable.  Inspired by the axiom of choice."
	
	Someone shouldImplement.
	^NULL "fodder"!
*/
}
/**
 * Essential.  If an OrderSpec is given, return the first element according to that
 * OrderSpec. If no OrderSpec is given, then iff I contain at least one position, return one
 * of them; otherwise BLAST. This should be implemented even by regions that aren't
 * enumerable.  Inspired by the axiom of choice.
 */
public Position chooseOne(OrderSpec order) {
	if (isEmpty()) {
		throw new AboraRuntimeException(AboraRuntimeException.EMPTY_REGION);
	}
	Someone.thingToDo();
	/* self isEnumerable assert: 'Must be overridden otherwise'. */
	return (Position) (stepper(order)).get();
/*
udanax-top.st:65359:XnRegion methodsFor: 'enumerating'!
{Position CLIENT} chooseOne: order {OrderSpec default: NULL}
	"Essential.  If an OrderSpec is given, return the first element according to that OrderSpec. If no OrderSpec is given, then iff I contain at least one position, return one of them; otherwise BLAST. This should be implemented even by regions that aren't enumerable.  Inspired by the axiom of choice."
	
	self isEmpty ifTrue:
		[Heaper BLAST: #EmptyRegion].
	self thingToDo.	"self isEnumerable assert: 'Must be overridden otherwise'."
	^(self stepper: order) get cast: Position!
*/
}
/**
 * How many positions do I contain?  If I am not 'isFinite', then this message will BLAST.
 */
public int count() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65367:XnRegion methodsFor: 'enumerating'!
{IntegerVar CLIENT} count
	"How many positions do I contain?  If I am not 'isFinite', then this message will BLAST."
	
	self subclassResponsibility!
*/
}
/**
 * break it up into a set of non-empty simple regions which don't
 * overlap. This message satisfies all the specs of 'simpleRegions', and
 * in addition provides for lack of overlap. It may be significantly more
 * expensive than 'simpleRegions' which is why they both exist.
 */
public Stepper disjointSimpleRegions(OrderSpec order) {
	return DisjointRegionStepper.make(this, order);
/*
udanax-top.st:65372:XnRegion methodsFor: 'enumerating'!
{Stepper INLINE of: XnRegion} disjointSimpleRegions: order {OrderSpec default: NULL} 
	"break it up into a set of non-empty simple regions which don't 
	overlap. This message satisfies all the specs of 'simpleRegions', and 
	in addition provides for lack of overlap. It may be significantly more 
	expensive than 'simpleRegions' which is why they both exist."
	^DisjointRegionStepper make: self with: order!
*/
}
/**
 * Break it up into a set of non-full distinctions. It is an error to send
 * this to a non-simple region. A full region will respond with the null
 * set. Other distinctions will respond with a singleton set containing
 * themselves, and simple regions will respond with a set of distinctions
 * which, when intersected together, yield the original region.
 */
public ScruSet distinctions() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65380:XnRegion methodsFor: 'enumerating'!
{ScruSet of: XnRegion} distinctions
	"Break it up into a set of non-full distinctions. It is an error to send 
	this to a non-simple region. A full region will respond with the null 
	set. Other distinctions will respond with a singleton set containing 
	themselves, and simple regions will respond with a set of distinctions 
	which, when intersected together, yield the original region."
	self subclassResponsibility!
*/
}
/**
 * Break myself up into a finite set of non-empty simple regions which, when
 * unionWith'ed together will yield me. May be sent to any region. If I
 * am isEmpty, I will respond with the empty stepper. Otherwise, if I am
 * simple I will respond with a stepper producing just myself.
 * Please only use NULL for the 'order' argument for now unless the
 * documentation for a particular region or coordinate space says that
 * it will deal with the 'order' argument meaningfully. When no order is
 * specified then I may return the simple regions in any order. When the
 * ordering functionality is implemented, then I am constrained to
 * produce the simple regions in an order consistent with the argument's
 * ordering of my positions. When the simple regions don't overlap, and
 * don't surround each other in the ordering, then the meaning is clear.
 * Otherwise, there are several plausible options for how we should
 * specify this message.
 */
public Stepper simpleRegions(OrderSpec order) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65389:XnRegion methodsFor: 'enumerating'!
{Stepper} simpleRegions: order {OrderSpec default: NULL} 
	"Break myself up into a finite set of non-empty simple regions which, when 
	unionWith'ed together will yield me. May be sent to any region. If I 
	am isEmpty, I will respond with the empty stepper. Otherwise, if I am 
	simple I will respond with a stepper producing just myself. 
	
	Please only use NULL for the 'order' argument for now unless the 
	documentation for a particular region or coordinate space says that 
	it will deal with the 'order' argument meaningfully. When no order is 
	specified then I may return the simple regions in any order. When the 
	ordering functionality is implemented, then I am constrained to 
	produce the simple regions in an order consistent with the argument's 
	ordering of my positions. When the simple regions don't overlap, and 
	don't surround each other in the ordering, then the meaning is clear. 
	Otherwise, there are several plausible options for how we should 
	specify this message."
	self subclassResponsibility!
*/
}
/**
 * Essential.  If my positions are enumerable in the order specified, then return a stepper
 * which will so enumerate them. If 'order' is NULL, then I may treat this as a request to
 * enumerate according to any order I choose, except that if I am enumerable in ascending
 * order, then I must be enumerable given NULL. For example, if I choose to regard NULL as
 * implying ascending order, and I am only enumerable in descending order, then given NULL, I
 * may blast even though there is an order in which I am enumerable.
 * In fact, right now the ability to respond to an 'order' argument is in such a
 * to-be-implemented state that it should only be considered safe to provide a NULL argument,
 * unless the documentation on a particular space or region says otherwise.
 * The eventual specification of this message is clear, and is upwards compatible from the
 * current behavior: If I can enumerate in an order consistent with 'order', do so. If
 * 'order' is NULL, then if I can be enumerated at all (if there is any counting sequence),
 * then I still do so. For example, I should be able to get an (infinite) stepper for
 * stepping through all the integers, but not all the reals. As the above example shows,
 * being enumerable doesn't imply being finite.
 * Also, being able to produce a stepper that continues to yield more positions in the
 * specified order is not sufficient to imply being enumerable.  To be enumerable, it must be
 * the case that any given position which is a member of the region will eventually be
 * reached by the stepper.  Not all implementations currently succeed in guaranteeing this
 * (See UnionCrossRegion::isEnumerable).
 * See ScruTable::stepper.
 */
public Stepper stepper(OrderSpec order) {
	OrderSpec ord;
	ord = order;
	if (ord == null) {
		ord = coordinateSpace().fetchAscending();
	}
	if (ord == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_ENUMERABLE);
	}
	return actualStepper(ord);
/*
udanax-top.st:65408:XnRegion methodsFor: 'enumerating'!
{Stepper CLIENT of: Position} stepper: order {OrderSpec default: NULL} 
	"Essential.  If my positions are enumerable in the order specified, then return a stepper which will so enumerate them. If 'order' is NULL, then I may treat this as a request to enumerate according to any order I choose, except that if I am enumerable in ascending order, then I must be enumerable given NULL. For example, if I choose to regard NULL as implying ascending order, and I am only enumerable in descending order, then given NULL, I may blast even though there is an order in which I am enumerable. 
	
	In fact, right now the ability to respond to an 'order' argument is in such a to-be-implemented state that it should only be considered safe to provide a NULL argument, unless the documentation on a particular space or region says otherwise. 
	
	The eventual specification of this message is clear, and is upwards compatible from the current behavior: If I can enumerate in an order consistent with 'order', do so. If 'order' is NULL, then if I can be enumerated at all (if there is any counting sequence), then I still do so. For example, I should be able to get an (infinite) stepper for stepping through all the integers, but not all the reals. As the above example shows, being enumerable doesn't imply being finite.  
	
	Also, being able to produce a stepper that continues to yield more positions in the specified order is not sufficient to imply being enumerable.  To be enumerable, it must be the case that any given position which is a member of the region will eventually be reached by the stepper.  Not all implementations currently succeed in guaranteeing this (See UnionCrossRegion::isEnumerable).
	
	See ScruTable::stepper."
	| ord {OrderSpec | NULL} |
	ord := order.
	ord == NULL ifTrue:
		[ord := self coordinateSpace fetchAscending].
	ord == NULL ifTrue:
		[Heaper BLAST: #NotEnumerable].
	^self actualStepper: ord!
*/
}
/**
 * Iff I contain exactly one position, return it.  Otherwise BLAST. The idea for this message
 * is taken from the THE function of ONTIC (reference McAllester)
 */
public Position theOne() {
	Stepper stepper;
	Position result;
	if ( ! (isFinite() && (count() == 1))) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_ONE_ELEMENT);
	}
	stepper = stepper();
	result = (Position) stepper.fetch();
	stepper.destroy();
	return result;
/*
udanax-top.st:65427:XnRegion methodsFor: 'enumerating'!
{Position CLIENT} theOne
	"Iff I contain exactly one position, return it.  Otherwise BLAST. The idea for this message is taken from the THE function of ONTIC (reference McAllester)"
	
	| stepper {Stepper} result {Position} |
	(self isFinite and: [self count == 1]) ifFalse:
		[ Heaper BLAST: #NotOneElement ].
	stepper _ self stepper.
	result _ stepper fetch cast: Position.
	stepper destroy.
	^ result!
*/
}
/*
udanax-top.st:65440:XnRegion methodsFor: 'smalltalk: special'!
{void} do: aBlock {BlockClosure of: Position}
	self stepper forEach: aBlock!
*/
/**
 * Only called if I've already said I'm enumerable in the originally stated order.  Also, if
 * the originally stated order was NULL, I get a guaranteed non-null order.  Subclasses which
 * override 'stepper' to a method which doesn't send 'actualStepper' may override
 * 'actualStepper' to a stub method which always BLASTs.
 */
public Stepper actualStepper(OrderSpec order) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:65446:XnRegion methodsFor: 'protected: enumerating'!
{Stepper of: Position} actualStepper: order {OrderSpec} 
	"Only called if I've already said I'm enumerable in the originally stated order.  Also, if the originally stated order was NULL, I get a guaranteed non-null order.  Subclasses which override 'stepper' to a method which doesn't send 'actualStepper' may override 'actualStepper' to a stub method which always BLASTs."
	self subclassResponsibility!
*/
}
/**
 * Returns all the Positions in the region in order according to 'order'.  If the region
 * isn't finite, then this BLASTs.
 * @deprecated
 */
public PtrArray asArray(OrderSpec order) {
	throw new PasseException();
/*
udanax-top.st:65453:XnRegion methodsFor: 'smalltalk: passe'!
{PtrArray of: Position} asArray: order {OrderSpec default: NULL}
	"Returns all the Positions in the region in order according to 'order'.  If the region isn't finite, then this BLASTs."
	self passe
	"| result {PtrArray of: Position} i {Int32} |
	self isFinite not ifTrue:
		[Heaper BLAST: #NotFinite].
	result := PtrArray make: self count DOTasLong.
	i := Int32Zero.
	(self stepper: order) forEach: [:pos {Position} |
		result at: i store: pos.
		i := i + 1].
	(self count == i) assert: 'My stepper must yield same count of positions that I report'.
	^result"!
*/
}
/**
 * See comment in XuRegion::stepper.
 * a->stepper(os) won't BLAST iff a->isEnumerable(os)
 * @deprecated
 */
public boolean isEnumerable(OrderSpec order) {
	throw new PasseException();
/*
udanax-top.st:65467:XnRegion methodsFor: 'smalltalk: passe'!
{BooleanVar} isEnumerable: order {OrderSpec default: NULL}
	"See comment in XuRegion::stepper.  
	a->stepper(os) won't BLAST iff a->isEnumerable(os)"
	
	self subclassResponsibility!
*/
}
/**
 * Make a set containing all the positions in the region
 */
public static ImmuSet immuSetmake(XnRegion region) {
	if ( ! (region.isFinite())) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_FINITE);
	}
	return (MuSet.fromStepper(region.stepper())).asImmuSet();
/*
udanax-top.st:65482:XnRegion class methodsFor: 'pseudo constructors'!
{ImmuSet} immuSet.make: region {XnRegion}
	"Make a set containing all the positions in the region"
	
	region isFinite ifFalse:
		[Heaper BLAST: #MustBeFinite].
	^(MuSet fromStepper: region stepper) asImmuSet!
*/
}
/**
 * {Position CLIENT} chooseOne: order {OrderSpec default: NULL}
 * {XuRegion CLIENT} complement
 * {CoordinateSpace CLIENT} coordinateSpace
 * {IntegerVar CLIENT} count
 * {BooleanVar CLIENT} hasMember: atPos {Position unused}
 * {XuRegion CLIENT} intersect: other {XuRegion unused}
 * {BooleanVar CLIENT} intersects: other {XuRegion}
 * {BooleanVar CLIENT} isEmpty
 * {BooleanVar CLIENT} isFinite
 * {BooleanVar CLIENT} isFull
 * {BooleanVar CLIENT} isSubsetOf: other {XuRegion}
 * {XuRegion CLIENT} minus: other {XuRegion}
 * {Stepper CLIENT of: Position} stepper: order {OrderSpec default: NULL}
 * {Position CLIENT} theOne
 * {XuRegion CLIENT} unionWith: other {XuRegion unused}
 * {XuRegion CLIENT} with: pos {Position}
 * {XuRegion CLIENT} without: pos {Position}
 */
public static void infostProtocol() {
/*
udanax-top.st:65491:XnRegion class methodsFor: 'smalltalk: system'!
info.stProtocol
"{Position CLIENT} chooseOne: order {OrderSpec default: NULL}
{XuRegion CLIENT} complement
{CoordinateSpace CLIENT} coordinateSpace
{IntegerVar CLIENT} count
{BooleanVar CLIENT} hasMember: atPos {Position unused}
{XuRegion CLIENT} intersect: other {XuRegion unused}
{BooleanVar CLIENT} intersects: other {XuRegion}
{BooleanVar CLIENT} isEmpty
{BooleanVar CLIENT} isFinite
{BooleanVar CLIENT} isFull
{BooleanVar CLIENT} isSubsetOf: other {XuRegion}
{XuRegion CLIENT} minus: other {XuRegion}
{Stepper CLIENT of: Position} stepper: order {OrderSpec default: NULL}
{Position CLIENT} theOne
{XuRegion CLIENT} unionWith: other {XuRegion unused}
{XuRegion CLIENT} with: pos {Position}
{XuRegion CLIENT} without: pos {Position}
"!
*/
}
public XnRegion() {
/*

Generated during transformation
*/
}
public XnRegion(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
