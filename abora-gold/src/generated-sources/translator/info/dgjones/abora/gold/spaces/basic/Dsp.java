/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.basic;

import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.SimpleMapping;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * A Dsp is a mapping from a coordinate space to itself that preserves simple regions.  Every
 * coordinate space must have an identity Dsp (which maps all positions of that space onto
 * themselves). Dsps are necessarily invertable and composable.
 * (Removed from CoordinateSpace because Dsps are still internal.:
 * Dsp -- The transformations that can be applied to positions and regions of this cordinate
 * space.  A Dsp is necessarily invertible but generally not order-preserving. The
 * composition of two Dsps is always a Dsp. If you can subtract two Dsps, the result will be
 * another Dsp.  The Dsp of a Position in this space is always another Position in this
 * space.  The Dsp of a simple region is always another simple region.)
 * Considering a Mapping as a set of pairs, a Dsp is one for which each position appears
 * exactly once in the first elements of the pairs, and exactly once in the second elements.
 * Composition of Dsps isn''t necessarily commutative, though there are currently no
 * counter-examples.  Therefore we must be extra careful to avoid embodying commutativity
 * assumptions in our code, as we currently have no way of finding such bugs.
 */
public class Dsp extends Mapping {

/*
udanax-top.st:29020:
Mapping subclass: #Dsp
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Basic'!
*/
/*
udanax-top.st:29024:
Dsp comment:
'A Dsp is a mapping from a coordinate space to itself that preserves simple regions.  Every coordinate space must have an identity Dsp (which maps all positions of that space onto themselves). Dsps are necessarily invertable and composable.
(Removed from CoordinateSpace because Dsps are still internal.:
Dsp -- The transformations that can be applied to positions and regions of this cordinate space.  A Dsp is necessarily invertible but generally not order-preserving. The composition of two Dsps is always a Dsp. If you can subtract two Dsps, the result will be another Dsp.  The Dsp of a Position in this space is always another Position in this space.  The Dsp of a simple region is always another simple region.)
	
	
	Considering a Mapping as a set of pairs, a Dsp is one for which each position appears exactly once in the first elements of the pairs, and exactly once in the second elements.  Composition of Dsps isn''t necessarily commutative, though there are currently no counter-examples.  Therefore we must be extra careful to avoid embodying commutativity assumptions in our code, as we currently have no way of finding such bugs.'!
*/
/*
udanax-top.st:29032:
(Dsp getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Dsp.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * For Dsp's, it is identical to compose.
 */
public Mapping appliedAfter(Dsp dsp) {
	return compose(dsp);
/*
udanax-top.st:29037:Dsp methodsFor: 'accessing'!
{Mapping INLINE} appliedAfter: dsp {Dsp}
	"For Dsp's, it is identical to compose."
	
	^self compose: dsp!
*/
}
/**
 * the coordinate space of the domain and range of the Dsp
 */
public CoordinateSpace coordinateSpace() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:29042:Dsp methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	"the coordinate space of the domain and range of the Dsp"
	self subclassResponsibility!
*/
}
/**
 * Must be valid everywhere in the domain for a Dsp.
 */
public XnRegion domain() {
	return coordinateSpace().fullRegion();
/*
udanax-top.st:29047:Dsp methodsFor: 'accessing'!
{XnRegion} domain
	"Must be valid everywhere in the domain for a Dsp."
	
	^self coordinateSpace fullRegion!
*/
}
public Dsp fetchDsp() {
	return this;
/*
udanax-top.st:29052:Dsp methodsFor: 'accessing'!
{(Dsp | NULL) INLINE} fetchDsp
	^ self!
*/
}
public boolean isComplete() {
	return false;
/*
udanax-top.st:29055:Dsp methodsFor: 'accessing'!
{BooleanVar INLINE} isComplete
	^false!
*/
}
/**
 * Says whether this Dsp maps every Position onto itself
 */
public boolean isIdentity() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:29059:Dsp methodsFor: 'accessing'!
{BooleanVar} isIdentity
	"Says whether this Dsp maps every Position onto itself"
	
	self subclassResponsibility!
*/
}
/**
 * a->compose(b) is the same as b->preCompose(a).  Don't use it, use
 * compose instead.
 */
public Mapping preCompose(Dsp dsp) {
	return dsp.compose(this);
/*
udanax-top.st:29064:Dsp methodsFor: 'accessing'!
{Mapping} preCompose: dsp {Dsp} 
	"a->compose(b) is the same as b->preCompose(a).  Don't use it, use
	compose instead."
	^dsp compose: self!
*/
}
public XnRegion range() {
	return coordinateSpace().fullRegion();
/*
udanax-top.st:29070:Dsp methodsFor: 'accessing'!
{XnRegion} range
	
	^self coordinateSpace fullRegion!
*/
}
/**
 * Same as the domain space
 */
public CoordinateSpace rangeSpace() {
	return coordinateSpace();
/*
udanax-top.st:29074:Dsp methodsFor: 'accessing'!
{CoordinateSpace INLINE} rangeSpace
	"Same as the domain space"
	^ self coordinateSpace!
*/
}
/**
 * A Dsp is a simpleMapping already, so this just returns the singleton set containing me
 */
public ImmuSet simpleMappings() {
	return ImmuSet.make().with(this);
/*
udanax-top.st:29078:Dsp methodsFor: 'accessing'!
{ImmuSet of: Mapping} simpleMappings
	"A Dsp is a simpleMapping already, so this just returns the singleton set containing me"
	^ ImmuSet make with: self.!
*/
}
/**
 * The domain of a Dsp is the simple region covering the whole coordinate space, so
 * I just return a singleton set containing myself
 */
public ImmuSet simpleRegionMappings() {
	return ImmuSet.make().with(this);
/*
udanax-top.st:29082:Dsp methodsFor: 'accessing'!
{ImmuSet of: Mapping} simpleRegionMappings
	"The domain of a Dsp is the simple region covering the whole coordinate space, so
	I just return a singleton set containing myself"
	^ ImmuSet make with: self.!
*/
}
/**
 * For Dsp's, it is identical to preCompose.
 */
public Mapping transformedBy(Dsp dsp) {
	return dsp.compose(this);
/*
udanax-top.st:29088:Dsp methodsFor: 'accessing'!
{Mapping INLINE} transformedBy: dsp {Dsp}
	"For Dsp's, it is identical to preCompose."
	
	^dsp compose: self!
*/
}
/**
 * Return the composition of the two Dsps. Two Dsps of the same space are always composable.
 * (a->compose(b) ->minus(b))->isEqual (a)
 * (a->compose(b) ->of(pos))->isEqual (a->of (b->of (pos))
 */
public Dsp compose(Dsp other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:29095:Dsp methodsFor: 'combining'!
{Dsp} compose: other {Dsp}
	"Return the composition of the two Dsps. Two Dsps of the same space are always composable.
	(a->compose(b) ->minus(b))->isEqual (a)
	(a->compose(b) ->of(pos))->isEqual (a->of (b->of (pos))"
	self subclassResponsibility!
*/
}
/**
 * Return the inverse of this transformation. Considering the Dsp as a set of pairs
 * (see class comment), return the Dsp which has the mirror image of all my
 * pairs.
 */
public Mapping inverse() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:29102:Dsp methodsFor: 'combining'!
{Mapping} inverse
	"Return the inverse of this transformation. Considering the Dsp as a set of pairs 
	(see class comment), return the Dsp which has the mirror image of all my 
	pairs."
	self subclassResponsibility!
*/
}
/**
 * Return the difference of the two Dsps.
 * (a->compose(b) ->minus(b))->isEqual (a)
 */
public Dsp minus(Dsp other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:29109:Dsp methodsFor: 'combining'!
{Dsp} minus: other {Dsp}
	"Return the difference of the two Dsps.
	(a->compose(b) ->minus(b))->isEqual (a)"
	self subclassResponsibility!
*/
}
/**
 * If 'reg' is a simple region, then the result must also be simple
 */
public XnRegion ofAll(XnRegion reg) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:29117:Dsp methodsFor: 'transforming'!
{XnRegion} ofAll: reg {XnRegion}
	"If 'reg' is a simple region, then the result must also be simple"
	
	self subclassResponsibility!
*/
}
public Mapping restrict(XnRegion region) {
	return SimpleMapping.restrictTo(region, this);
/*
udanax-top.st:29124:Dsp methodsFor: 'operations'!
{Mapping INLINE} restrict: region {XnRegion}
	^SimpleMapping restrictTo: region with: self!
*/
}
public Mapping restrictRange(XnRegion region) {
	return SimpleMapping.restrictTo((inverseOfAll(region)), this);
/*
udanax-top.st:29128:Dsp methodsFor: 'operations'!
{Mapping} restrictRange: region {XnRegion}
	^SimpleMapping restrictTo: (self inverseOfAll: region) with: self!
*/
}
public Mapping fetchCombine(Mapping mapping) {
	if (isEqual(mapping)) {
		return this;
	}
	else {
		return null;
	}
/*
udanax-top.st:29134:Dsp methodsFor: 'protected:'!
{Mapping} fetchCombine: mapping {Mapping}
	(self isEqual: mapping)
		ifTrue: [^self]
		ifFalse: [^NULL]!
*/
}
/**
 * Since Dsps always represent a unique mapping in either direction, the permission to BLAST
 * in the Mapping constract no longer applies.
 * a->inverseOf(b) ->isEqual (a->inverse()->of(b))
 */
public Position inverseOf(Position pos) {
	return ((Dsp) inverse()).of(pos);
/*
udanax-top.st:29142:Dsp methodsFor: 'deferred transforming'!
{Position} inverseOf: pos {Position}
	"Since Dsps always represent a unique mapping in either direction, the permission to BLAST
	in the Mapping constract no longer applies.
	a->inverseOf(b) ->isEqual (a->inverse()->of(b))"
	
	^(self inverse cast: Dsp) of: pos!
*/
}
/**
 * Inverse transform a region.  A simple region must yield a simple region.
 * a->inverseOfAll(b) ->isEqual (a->inverseAll()->of(b))
 */
public XnRegion inverseOfAll(XnRegion reg) {
	return ((Dsp) inverse()).ofAll(reg);
/*
udanax-top.st:29149:Dsp methodsFor: 'deferred transforming'!
{XnRegion} inverseOfAll: reg {XnRegion}
	"Inverse transform a region.  A simple region must yield a simple region.
	a->inverseOfAll(b) ->isEqual (a->inverseAll()->of(b))"
	^(self inverse cast: Dsp) ofAll: reg!
*/
}
/**
 * Since Dsps always represent a unique mapping in either direction, the permission to BLAST
 * in the Mapping constract no longer applies.
 */
public Position of(Position pos) {
	return (ofAll(pos.asRegion())).theOne();
/*
udanax-top.st:29155:Dsp methodsFor: 'deferred transforming'!
{Position} of: pos {Position}
	"Since Dsps always represent a unique mapping in either direction, the permission to BLAST
	in the Mapping constract no longer applies."
	
	^(self ofAll: pos asRegion) theOne!
*/
}
/**
 * Return the composition of my inverse with the other.
 * a->inverseCompose(b) ->isEqual (a->inverse()->compose(b))
 */
public Dsp inverseCompose(Dsp other) {
	return ((Dsp) inverse()).compose(other);
/*
udanax-top.st:29163:Dsp methodsFor: 'deferred combining'!
{Dsp} inverseCompose: other {Dsp}
	"Return the composition of my inverse with the other.
	a->inverseCompose(b) ->isEqual (a->inverse()->compose(b))"
	^(self inverse cast: Dsp) compose: other!
*/
}
public Dsp() {
/*

Generated during transformation
*/
}
public Dsp(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
