/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.be.basic.BeCarrier;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.PrimDataArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.Pair;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.FeCompletionDetector;
import info.dgjones.abora.gold.java.missing.FeFillInDetector;
import info.dgjones.abora.gold.java.missing.TwoStepper;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nbacken.EditionStepper;
import info.dgjones.abora.gold.nkernel.FeArrayBundle;
import info.dgjones.abora.gold.nkernel.FeBundle;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeElementBundle;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeLabel;
import info.dgjones.abora.gold.nkernel.FePlaceHolderBundle;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * The kind of FeRangeElement that consists of an immutable organization of RangeElements,
 * indexed by Positions in some CoordinateSpace.
 * R1 prohibits cyclic containment.
 * Set notation is used in the comments documenting some of the methods of this class.  In
 * each case the cleartext explanation stands alone, and the set notation is a separate, more
 * formal, expression of the actions of the method, in terms of key(position)/label/value
 * triples ("<k,l,v>").
 */
public class FeEdition extends FeRangeElement {

	protected BeEdition myBeEdition;
	protected FeLabel myLabel;
/*
udanax-top.st:20708:
FeRangeElement subclass: #FeEdition
	instanceVariableNames: '
		myBeEdition {BeEdition}
		myLabel {FeLabel}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:20714:
FeEdition comment:
'The kind of FeRangeElement that consists of an immutable organization of RangeElements, indexed by Positions in some CoordinateSpace.
 R1 prohibits cyclic containment.
Set notation is used in the comments documenting some of the methods of this class.  In each case the cleartext explanation stands alone, and the set notation is a separate, more formal, expression of the actions of the method, in terms of key(position)/label/value triples ("<k,l,v>").'!
*/
/*
udanax-top.st:20719:
(FeEdition getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:21407:
FeEdition class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:21410:
(FeEdition getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeEdition.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential.  Return a new FeEdition containing the contents of boththe receiver and the
 * argument Editions, and with the label of the receiving edition; where they share
 * positions, they must have the same RangeElement.  Currently the two may not share
 * positions.  It is unclear whether to elevate this from an implementation restriction to a
 * specification.  The advantage of so specifying is that 'combine' becomes timing
 * independent, i.e. a failing combine could otherwise succeed after the differing range
 * elements were unified (by FeRangeElement::makeIdentical()).  See FeEdition::mapSharedOnto
 * and FeEdition::transformedBy.
 * { <k,l,v> | <k,l,v> in self or <k,l,v> in other }
 * requires:
 * currently: { k | <k,la,v1> in self and <k,lb,v2> in other } is empty
 * eventually maybe: { k | v1 not same as v2
 * and <k,la,v1> in self and <k,lb,v2> in other } is empty
 */
public FeEdition combine(FeEdition other) {
	return FeEdition.on((myBeEdition.combine(other.beEdition())), myLabel);
/*
udanax-top.st:20724:FeEdition methodsFor: 'operations'!
{FeEdition CLIENT} combine: other {FeEdition}
	"Essential.  Return a new FeEdition containing the contents of boththe receiver and the argument Editions, and with the label of the receiving edition; where they share positions, they must have the same RangeElement.  Currently the two may not share positions.  It is unclear whether to elevate this from an implementation restriction to a specification.  The advantage of so specifying is that 'combine' becomes timing independent, i.e. a failing combine could otherwise succeed after the differing range elements were unified (by FeRangeElement::makeIdentical()).  See FeEdition::mapSharedOnto and FeEdition::transformedBy.
	
	{ <k,l,v> | <k,l,v> in self or <k,l,v> in other }
	requires:
		currently: { k | <k,la,v1> in self and <k,lb,v2> in other } is empty
		eventually maybe: { k | v1 not same as v2 
									and <k,la,v1> in self and <k,lb,v2> in other } is empty"
	
	^FeEdition on: (myBeEdition combine: other beEdition) with: myLabel!
*/
}
/**
 * Return a new FeEdition which is the subset of this Edition with the domain restricted to
 * the given set of positions  The new edition has the same label as this edition.
 * { <k,l,v> | k in positions and <k,l,v> in self }
 */
public FeEdition copy(XnRegion positions) {
	return FeEdition.on((myBeEdition.copy(positions)), myLabel);
/*
udanax-top.st:20735:FeEdition methodsFor: 'operations'!
{FeEdition CLIENT} copy: positions {XnRegion}
	"Return a new FeEdition which is the subset of this Edition with the domain restricted to the given set of positions  The new edition has the same label as this edition.
	
	{ <k,l,v> | k in positions and <k,l,v> in self }"
	
	^FeEdition on: (myBeEdition copy: positions) with: myLabel!
*/
}
/**
 * Return a new FeEdition with the label of the current Edition and the contents of both
 * Editions; where they share positions, use the contents and labels of the other Edition.
 * Equivalent to
 * this->copy (other->domain ()->complement ())->combine (other).
 * { <k,l,v> | <k,l,v> in other or (<k,l,v> in self and <k,l2,v2> not in other }
 */
public FeEdition replace(FeEdition other) {
	return FeEdition.on((myBeEdition.replace(other.beEdition())), myLabel);
/*
udanax-top.st:20742:FeEdition methodsFor: 'operations'!
{FeEdition CLIENT} replace: other {FeEdition}
	"Return a new FeEdition with the label of the current Edition and the contents of both Editions; where they share positions, use the contents and labels of the other Edition. Equivalent to
		this->copy (other->domain ()->complement ())->combine (other).
		
	{ <k,l,v> | <k,l,v> in other or (<k,l,v> in self and <k,l2,v2> not in other }"
		
	^FeEdition on: (myBeEdition replace: other beEdition) with: myLabel!
*/
}
/**
 * Essential.  Return a new FeEdition containing the contents and label of the current
 * Edition with the positions transformed according to the given Mapping. Where the Mapping
 * takes several positions in the domain to a single position in the range, this Edition must
 * have the same RangeElement and label at all the domain positions.  Currently the mapping
 * must be 'onto', i.e., no more that one domain position may map onto any given range
 * position.  It is unclear whether to elevate this from an implementation restriction to a
 * specification.  See FeEdition::mapSharedOnto and FeEdition::combine.
 * { <k2,l1,v1> | <k1,l1,v1> in self and <k1,k2> in mapping }
 * requires:
 * Currently: not exists k1a, k1b : k1a !!= k1b and <k1a,k2> in mapping and <k1b,k2> in
 * mapping.
 * Maybe eventually: for all v1, v2 : <k,l1,v1> in result and <k,l2,v2> in result, v1 is same
 * as v2
 */
public FeEdition transformedBy(Mapping mapping) {
	return FeEdition.on((myBeEdition.transformedBy(mapping)), myLabel);
/*
udanax-top.st:20750:FeEdition methodsFor: 'operations'!
{FeEdition CLIENT} transformedBy: mapping {Mapping}
	"Essential.  Return a new FeEdition containing the contents and label of the current Edition with the positions transformed according to the given Mapping. Where the Mapping takes several positions in the domain to a single position in the range, this Edition must have the same RangeElement and label at all the domain positions.  Currently the mapping must be 'onto', i.e., no more that one domain position may map onto any given range position.  It is unclear whether to elevate this from an implementation restriction to a specification.  See FeEdition::mapSharedOnto and FeEdition::combine.
	
	{ <k2,l1,v1> | <k1,l1,v1> in self and <k1,k2> in mapping }
	requires:
		Currently: not exists k1a, k1b : k1a !!= k1b and <k1a,k2> in mapping and <k1b,k2> in mapping.
		Maybe eventually: for all v1, v2 : <k,l1,v1> in result and <k,l2,v2> in result, v1 is same as v2"
	
	^FeEdition on: (myBeEdition transformedBy: mapping) with: myLabel!
*/
}
/**
 * Return a new FeEditionwith the same contents and label as this Edition, except for the
 * addition or substitution of a RangeElement at a specified position.
 * (The difference between with() and rebind() is exactly that rebind() preserves the old
 * label at position, while with() installs the label attached to the value argument.)
 * Equivalent to:
 * this->replace (FeServer::current ()->makeEditionWith (position, value))
 */
public FeEdition with(Position position, FeRangeElement value) {
	return FeEdition.on((myBeEdition.with(position, value.carrier())), myLabel);
/*
udanax-top.st:20760:FeEdition methodsFor: 'operations'!
{FeEdition CLIENT} with: position {Position} with: value {FeRangeElement}
	"Return a new FeEditionwith the same contents and label as this Edition, except for the addition or substitution of a RangeElement at a specified position.
	(The difference between with() and rebind() is exactly that rebind() preserves the old label at position, while with() installs the label attached to the value argument.)
	Equivalent to:
		this->replace (FeServer::current ()->makeEditionWith (position, value))"
		
	^FeEdition on: (myBeEdition with: position with: value carrier) with: myLabel!
*/
}
/**
 * Return a new FeEdition with the same contents and label as this Edition, except at a
 * specified set of positions, where the old values and labels, if there are any, are
 * superceded by the value argument.
 * Equivalent to:
 * this->replace (FeServer::current ()->makeEditionWithAll (positions, value))
 */
public FeEdition withAll(XnRegion positions, FeRangeElement value) {
	return FeEdition.on((myBeEdition.withAll(positions, value.carrier())), myLabel);
/*
udanax-top.st:20768:FeEdition methodsFor: 'operations'!
{FeEdition CLIENT} withAll: positions {XnRegion} with: value {FeRangeElement}
	"Return a new FeEdition with the same contents and label as this Edition, except at a specified set of positions, where the old values and labels, if there are any, are superceded by the value argument.
	Equivalent to:
		this->replace (FeServer::current ()->makeEditionWithAll (positions, value))"
		
	^FeEdition on: (myBeEdition withAll: positions with: value carrier) with: myLabel!
*/
}
/**
 * Return a new FeEdition with the same contents and label as this Edition, except at a
 * specified position, where the old value and label, if there is one, is removed.
 * Equivalent to:
 * this->copy (position->asRegion ()->complement ())
 */
public FeEdition without(Position position) {
	return FeEdition.on((myBeEdition.without(position)), myLabel);
/*
udanax-top.st:20775:FeEdition methodsFor: 'operations'!
{FeEdition CLIENT} without: position {Position}
	"Return a new FeEdition with the same contents and label as this Edition, except at a specified position, where the old value and label, if there is one, is removed.
	Equivalent to:
		this->copy (position->asRegion ()->complement ())"
		
	^FeEdition on: (myBeEdition without: position) with: myLabel!
*/
}
/**
 * Return a new FeEdition with the same contents and label as this Edition, except at a
 * specified set of positions, where the old values and labels, if there are any, are
 * removed.
 * Equivalent to
 * this->copy (positions->complement ())
 */
public FeEdition withoutAll(XnRegion positions) {
	return FeEdition.on((myBeEdition.withoutAll(positions)), myLabel);
/*
udanax-top.st:20782:FeEdition methodsFor: 'operations'!
{FeEdition CLIENT} withoutAll: positions {XnRegion}
	"Return a new FeEdition with the same contents and label as this Edition, except at a specified set of positions, where the old values and labels, if there are any, are removed.
	Equivalent to
		this->copy (positions->complement ())"
		
	^FeEdition on: (myBeEdition withoutAll: positions) with: myLabel!
*/
}
/**
 * Return the space in which the positions of this Edition are positions. Equivalent to
 * this->domain ()->coordinateSpace ()
 */
public CoordinateSpace coordinateSpace() {
	return myBeEdition.coordinateSpace();
/*
udanax-top.st:20791:FeEdition methodsFor: 'accessing'!
{CoordinateSpace CLIENT} coordinateSpace
	"Return the space in which the positions of this Edition are positions. Equivalent to
		this->domain ()->coordinateSpace ()"
	^myBeEdition coordinateSpace!
*/
}
/**
 * Essential. Retiurn how much space this Edition is taking up on the disk, in bytes (but the
 * precision may exceed the accuracy; it's simply a well-known unit). The method determines
 * how material shared with other Editions is treated: if omitShared, it is not counted at
 * all; if prorateShared, then it is divided evenly among the Editions sharing it; if
 * totalShared, its entire cost is counted. This figure is only approximate, and may vary
 * with time.
 * (No permissions are required to obtain this informiation, even though it exposes sharing
 * by Editions you can't read to traffic analysis.)
 */
public int cost(int method) {
	Someone.shouldImplement();
	return 0;
/*
udanax-top.st:20796:FeEdition methodsFor: 'accessing'!
{IntegerVar CLIENT} cost: method {Int32}
	"Essential. Retiurn how much space this Edition is taking up on the disk, in bytes (but the precision may exceed the accuracy; it's simply a well-known unit). The method determines how material shared with other Editions is treated: if omitShared, it is not counted at all; if prorateShared, then it is divided evenly among the Editions sharing it; if totalShared, its entire cost is counted. This figure is only approximate, and may vary with time.
	(No permissions are required to obtain this informiation, even though it exposes sharing by Editions you can't read to traffic analysis.)"
	
	Someone shouldImplement.
	^IntegerVarZero "fodder"!
*/
}
/**
 * Return the number of positions in this Edition. Blasts if infinite. Equivalent to
 * this->domain ()->count ()
 */
public int count() {
	return myBeEdition.count();
/*
udanax-top.st:20803:FeEdition methodsFor: 'accessing'!
{IntegerVar CLIENT} count
	"Return the number of positions in this Edition. Blasts if infinite. Equivalent to
		this->domain ()->count ()"
		
	^myBeEdition count!
*/
}
/**
 * Essential.  Return the region consisting of all the positions in this Edition. May be
 * infinite, or empty.
 * { k | <k,l,v> in self }
 */
public XnRegion domain() {
	return myBeEdition.domain();
/*
udanax-top.st:20809:FeEdition methodsFor: 'accessing'!
{XnRegion CLIENT} domain
	"Essential.  Return the region consisting of all the positions in this Edition. May be infinite, or empty.
	
	{ k | <k,l,v> in self }"
	
	^myBeEdition domain!
*/
}
/**
 * Return the value at the given position, or blast if there is no such position (i.e. if !!
 * this->domain ()->hasMember (position)).
 * v : <position,l,v> in self
 * requires: <position,l,v> in self
 */
public FeRangeElement get(Position position) {
	return myBeEdition.get(position);
/*
udanax-top.st:20816:FeEdition methodsFor: 'accessing'!
{FeRangeElement CLIENT} get: position {Position}
	"Return the value at the given position, or blast if there is no such position (i.e. if !! this->domain ()->hasMember (position)).
	
	v : <position,l,v> in self
	requires: <position,l,v> in self"
	
	^myBeEdition get: position!
*/
}
/**
 * Return whether the given position is in the Edition. Equivalent to
 * this->domain ()->hasMember (position)
 */
public boolean hasPosition(Position position) {
	Someone.thingToDo();
	/* rename Be protocol */
	return myBeEdition.includesKey(position);
/*
udanax-top.st:20824:FeEdition methodsFor: 'accessing'!
{BooleanVar CLIENT} hasPosition: position {Position}
	"Return whether the given position is in the Edition. Equivalent to
		this->domain ()->hasMember (position)"
	
	self thingToDo. "rename Be protocol"
	^myBeEdition includesKey: position!
*/
}
/**
 * Return whether there are any positions in this Edition. Equivalent to
 * this->domain ()->isEmpty ()
 */
public boolean isEmpty() {
	return myBeEdition.isEmpty();
/*
udanax-top.st:20831:FeEdition methodsFor: 'accessing'!
{BooleanVar CLIENT} isEmpty
	"Return whether there are any positions in this Edition. Equivalent to
		this->domain ()->isEmpty ()"
	
	^myBeEdition isEmpty!
*/
}
/**
 * Return whether there are a finite number of positions in this Edition. Equivalent to
 * this->domain ()->isFinite ()
 */
public boolean isFinite() {
	return myBeEdition.isFinite();
/*
udanax-top.st:20837:FeEdition methodsFor: 'accessing'!
{BooleanVar CLIENT} isFinite
	"Return whether there are a finite number of positions in this Edition. Equivalent to
		this->domain ()->isFinite ()"
	
	^myBeEdition isFinite!
*/
}
/**
 * Essential.  This is the fundamental retrieval operation.  Return a stepper of bundles.
 * Each bundle is an association between a region in the domain and the range elements
 * associated with that region.  Where the region is associated with data, for instance, the
 * bundle contains a PrimArray of the data elements.
 * If a region is given, only that subset of the Edition's contents will be returned.  If it
 * is not given, the entire content of the Edition will be returned.
 * if the ignoreTotalOrdering flag is set, then the operation can group non-contiguous
 * regions, and can supply the bundles in any order.
 * if the ignoreArrayOrdering flag is set, then ArrayBundles returned by the operation can be
 * ordered differently from the supplied order.
 * If an OrderSpec is not supplied, then the ordering will be the default order for the
 * coordinate space, if one exists, and if none exists the returned data will be completely
 * unordered and the Ordering flags will be ignored.
 */
public Stepper retrieve(XnRegion region, OrderSpec order, int flags) {
	Someone.thingToDo();
	/* The above comment is still horribly insufficient. */
	return myBeEdition.retrieve(region, order, flags);
/*
udanax-top.st:20843:FeEdition methodsFor: 'accessing'!
{(Stepper of: Bundle) CLIENT} retrieve: region {XnRegion default: NULL}
	with: order {OrderSpec default: NULL}
	with: flags {Int32 default: Int32Zero}
	"Essential.  This is the fundamental retrieval operation.  Return a stepper of bundles.  Each bundle is an association between a region in the domain and the range elements associated with that region.  Where the region is associated with data, for instance, the bundle contains a PrimArray of the data elements.
	If a region is given, only that subset of the Edition's contents will be returned.  If it is not given, the entire content of the Edition will be returned.
	if the ignoreTotalOrdering flag is set, then the operation can group non-contiguous regions, and can supply the bundles in any order.
	if the ignoreArrayOrdering flag is set, then ArrayBundles returned by the operation can be ordered differently from the supplied order.
	If an OrderSpec is not supplied, then the ordering will be the default order for the coordinate space, if one exists, and if none exists the returned data will be completely unordered and the Ordering flags will be ignored."
	
	self thingToDo.  "The above comment is still horribly insufficient."
	
	^myBeEdition retrieve: region with: order with: flags!
*/
}
/**
 * Return a stepper for iterating over the positions and RangeElements of this Edition. If a
 * region is specified, then it only iterates over the domain positions which are in the
 * given region. If no ordering is specified, then the default ascending full ordering of the
 * CoordinateSpace is used, or a random order chosen if there is no default.
 */
public TableStepper stepper(XnRegion region, OrderSpec ordering) {
	XnRegion theRegion;
	theRegion = domain();
	if (region != null) {
		theRegion = theRegion.intersect(region);
	}
	return new EditionStepper((theRegion.stepper(ordering)), this);
/*
udanax-top.st:20856:FeEdition methodsFor: 'accessing'!
{TableStepper CLIENT of: FeRangeElement} stepper: region {XnRegion default: NULL}
	with: ordering {OrderSpec default: NULL}
	"Return a stepper for iterating over the positions and RangeElements of this Edition. If a region is specified, then it only iterates over the domain positions which are in the given region. If no ordering is specified, then the default ascending full ordering of the CoordinateSpace is used, or a random order chosen if there is no default."
	
	| theRegion {XnRegion} |
	theRegion := self domain.
	region ~~ NULL
		ifTrue: [theRegion := theRegion intersect: region].
	^EditionStepper create: (theRegion stepper: ordering) with: self!
*/
}
/**
 * If this Edition has a single position, then return the RangeElement at that position; if
 * not, blasts. Equivalent to
 * this->get (this->domain ()->theOne ())
 */
public FeRangeElement theOne() {
	return myBeEdition.theOne();
/*
udanax-top.st:20866:FeEdition methodsFor: 'accessing'!
{FeRangeElement CLIENT} theOne
	"If this Edition has a single position, then return the RangeElement at that position; if not, blasts. Equivalent to
		this->get (this->domain ()->theOne ())"
		
	^myBeEdition theOne!
*/
}
/**
 * Whether the two Editions have the same domains, and each RangeElement isIdentical to the
 * corresponding RangeElement in the other Edition.
 */
public boolean isRangeIdentical(FeEdition other, XnRegion region) {
	Someone.shouldImplement();
	return false;
/*
udanax-top.st:20874:FeEdition methodsFor: 'comparing'!
{BooleanVar CLIENT} isRangeIdentical: other {FeEdition}
	with: region {XnRegion default: NULL}
	"Whether the two Editions have the same domains, and each RangeElement isIdentical to the corresponding RangeElement in the other Edition."
	
	Someone shouldImplement.
	^false "fodder"!
*/
}
/**
 * Return a mapping such that for each range element that appears in both editions, the
 * mapping maps each of its appearances in the argument edition to some appearance in this
 * one.  (Some of the appearances in this edition may be unmapped or mapped to multiple
 * appearances in the argument edition.)  Like 'mapSharedTo' except that the resulting
 * mapping is 'onto'.  This means that each range position of the resulting mapping inverse
 * maps to at most one domain position.  Such a mapping is suitable as an argument to
 * 'transformedBy', and represents the minimal transformation needed to make the shared part
 * of 'other' from self.  Note that there is no unique answer.
 * result = { <k1,k2> | <k1,l1,v1> in self and <k2,l2,v2> in other and v1 is same as v2
 * and not exists k11 : k11 !!= k1 and <k11,k2> in result }
 * Note that this is useful for optimization of FeBe communication and Frontend display
 * updating.
 */
public Mapping mapSharedOnto(FeEdition other) {
	Someone.shouldImplement();
	return null;
/*
udanax-top.st:20881:FeEdition methodsFor: 'comparing'!
{Mapping CLIENT} mapSharedOnto: other {FeEdition}
	"Return a mapping such that for each range element that appears in both editions, the mapping maps each of its appearances in the argument edition to some appearance in this one.  (Some of the appearances in this edition may be unmapped or mapped to multiple appearances in the argument edition.)  Like 'mapSharedTo' except that the resulting mapping is 'onto'.  This means that each range position of the resulting mapping inverse maps to at most one domain position.  Such a mapping is suitable as an argument to 'transformedBy', and represents the minimal transformation needed to make the shared part of 'other' from self.  Note that there is no unique answer.
	
	result = { <k1,k2> | <k1,l1,v1> in self and <k2,l2,v2> in other and v1 is same as v2
							and not exists k11 : k11 !!= k1 and <k11,k2> in result }
	
Note that this is useful for optimization of FeBe communication and Frontend display updating."
	Someone shouldImplement.
	^NULL "fodder"!
*/
}
/**
 * Essential.  Return a Mapping from each of the positions in this Edition to all of the
 * positions in the other Edition which have the same RangeElement.
 * { <k1,k2> | <k1,l1,v1> in self and <k2,l2,v2> in other and v1 is same as v2 }
 */
public Mapping mapSharedTo(FeEdition other) {
	return myBeEdition.mapSharedTo(other.beEdition());
/*
udanax-top.st:20892:FeEdition methodsFor: 'comparing'!
{Mapping CLIENT} mapSharedTo: other {FeEdition}
	"Essential.  Return a Mapping from each of the positions in this Edition to all of the positions in the other Edition which have the same RangeElement.
	
	{ <k1,k2> | <k1,l1,v1> in self and <k2,l2,v2> in other and v1 is same as v2 }"
	
	^myBeEdition mapSharedTo: other beEdition!
*/
}
/**
 * Return a new FeEdition containing exactly the subset of this Edition whose RangeElements
 * are not in the other Edition.
 * Equivalent to:
 * this->copy (this->sharedRegion (other)->complement ()).
 * { <k1,l1,v1> | <k1,l1,v1> in self and <k2,l2,v2> in other and v1 is same as v2 }
 * Note that this is useful for optimization of FeBe communication and Frontend display
 * updating.
 */
public FeEdition notSharedWith(FeEdition other, int flags) {
	return FeEdition.on((myBeEdition.notSharedWith(other.beEdition(), flags)), myLabel);
/*
udanax-top.st:20899:FeEdition methodsFor: 'comparing'!
{FeEdition CLIENT} notSharedWith: other {FeEdition}
	with: flags {Int32 default: Int32Zero}
	"Return a new FeEdition containing exactly the subset of this Edition whose RangeElements are not in the other Edition.
	Equivalent to:
		this->copy (this->sharedRegion (other)->complement ()).
		
	{ <k1,l1,v1> | <k1,l1,v1> in self and <k2,l2,v2> in other and v1 is same as v2 }
	
Note that this is useful for optimization of FeBe communication and Frontend display updating."
		
	^FeEdition on: (myBeEdition
			notSharedWith: other beEdition with: flags)
		with: myLabel!
*/
}
/**
 * Return the region consisting of all the positions in this Edition at which the given
 * RangeElement can be found.
 * Equivalent to:
 * this->sharedRegion (theServer ()->makeEditionWith (some position, value)).
 * { k | <k,l,v> in self and v is same as value }
 */
public XnRegion positionsOf(FeRangeElement value) {
	Someone.thingToDo();
	/* rename Be protocol */
	return myBeEdition.keysOf(value);
/*
udanax-top.st:20913:FeEdition methodsFor: 'comparing'!
{XnRegion CLIENT} positionsOf: value {FeRangeElement}
	"Return the region consisting of all the positions in this Edition at which the given RangeElement can be found.
	Equivalent to:
		this->sharedRegion (theServer ()->makeEditionWith (some position, value)).
		
	{ k | <k,l,v> in self and v is same as value }"
	
	self thingToDo. "rename Be protocol"
	^myBeEdition keysOf: value!
*/
}
/**
 * Essential.  Return a new FeEdition containing all Editions which can be read with the
 * authority of the CurrentKeyMaster, and which transclude RangeElements in this Edition.
 * Immediately returns with an Edition full of PlaceHolders, which will be filled in as
 * results appear; the lookup proceeds asynchronously.
 * The Server will attempt to avoid placing duplicate copies in the result, but it may still
 * happen.
 * If a Region is given, then the request only considers the subset at those positions (i.e.
 * equivalent to this->copy (positions)->rangeTransclusions (...))
 * If a directFilter is given, then the endorsements on the resulting Editions, unioned with
 * the endorsements on any Works directly on those Editions to which the CurrentKeyMaster has
 * read permission, must pass the filter.
 * If an indirectFilter is given, then the resulting Editions must be contained, directly or
 * indirectly, by an Edition whose endorsements (unioned with its readable Works
 * endorsements) pass the filter. (Giving a non-NULL indirectFilter will probably not be
 * supported in version 1.0.)
 * If the directContainersOnly flag is set, then the result only includes Editions which have
 * the material as RangeElements; otherwise, the result includes Editions which indirectly
 * contain the material through other Editions. (Setting this flag will probably not be
 * supported in version 1.0.)
 * If the fromTransitiveContents flag is set, then the result includes transclusions of
 * RangeElements of sub-Editions of this one, in addition to the RangeElements in this
 * Edition. (Setting ths flag will probably not be supported in version 1.0.)
 * If localPresentOnly flag is clear, a persistent request will be created, and the new
 * FeEdition will continue to be filled in in the future.  If it is set, only those Editions
 * which are currently known to transclude by this Backend are sure to be recorded into the
 * Trail.  (Some, but not all, Editions which come to transclude while this request is being
 * processed may be recorded.  If the request is followed by a
 * FeServer::waitForConsequences(), no Editions which come to transclude after the wait
 * completes will be recorded.)
 * If otherTranscluders is given, then the results will be recorded into it. (This may
 * increase the chance of the same Edition being recorded twice.)
 * (For convenience, you can attach a TransclusionDetector to the result Edition.  See
 * FeEdition::addFillRangeDetector()  See also FeServer::waitForConsequences().)
 */
public FeEdition rangeTranscluders(XnRegion positions, Filter directFilter, Filter indirectFilter, int flags, FeEdition otherTranscluders) {
	BeEdition theOther;
	Filter theDirectFilter;
	Filter theIndirectFilter;
	if (otherTranscluders == null) {
		theOther = null;
	}
	else {
		theOther = otherTranscluders.beEdition();
	}
	if (directFilter == null) {
		theDirectFilter = (Filter) FeServer.endorsementFilterSpace().fullRegion();
	}
	else {
		theDirectFilter = directFilter;
	}
	if (indirectFilter == null) {
		theIndirectFilter = (Filter) FeServer.endorsementFilterSpace().fullRegion();
	}
	else {
		theIndirectFilter = indirectFilter;
	}
	return FeEdition.on((myBeEdition.rangeTranscluders(positions, theDirectFilter, theIndirectFilter, flags, theOther)));
/*
udanax-top.st:20923:FeEdition methodsFor: 'comparing'!
{FeEdition CLIENT} rangeTranscluders: positions {XnRegion default: NULL}
	with: directFilter {Filter default: NULL}
	with: indirectFilter {Filter default: NULL}
	with: flags {Int32 default: Int32Zero}
	with: otherTranscluders {FeEdition default: NULL}
	"Essential.  Return a new FeEdition containing all Editions which can be read with the authority of the CurrentKeyMaster, and which transclude RangeElements in this Edition. Immediately returns with an Edition full of PlaceHolders, which will be filled in as results appear; the lookup proceeds asynchronously.
	The Server will attempt to avoid placing duplicate copies in the result, but it may still happen.
	If a Region is given, then the request only considers the subset at those positions (i.e. equivalent to this->copy (positions)->rangeTransclusions (...))
	If a directFilter is given, then the endorsements on the resulting Editions, unioned with the endorsements on any Works directly on those Editions to which the CurrentKeyMaster has read permission, must pass the filter.
	If an indirectFilter is given, then the resulting Editions must be contained, directly or indirectly, by an Edition whose endorsements (unioned with its readable Works endorsements) pass the filter. (Giving a non-NULL indirectFilter will probably not be supported in version 1.0.)
	If the directContainersOnly flag is set, then the result only includes Editions which have the material as RangeElements; otherwise, the result includes Editions which indirectly contain the material through other Editions. (Setting this flag will probably not be supported in version 1.0.)
	If the fromTransitiveContents flag is set, then the result includes transclusions of RangeElements of sub-Editions of this one, in addition to the RangeElements in this Edition. (Setting ths flag will probably not be supported in version 1.0.)
	If localPresentOnly flag is clear, a persistent request will be created, and the new FeEdition will continue to be filled in in the future.  If it is set, only those Editions which are currently known to transclude by this Backend are sure to be recorded into the Trail.  (Some, but not all, Editions which come to transclude while this request is being processed may be recorded.  If the request is followed by a FeServer::waitForConsequences(), no Editions which come to transclude after the wait completes will be recorded.)
	If otherTranscluders is given, then the results will be recorded into it. (This may increase the chance of the same Edition being recorded twice.)
	(For convenience, you can attach a TransclusionDetector to the result Edition.  See FeEdition::addFillRangeDetector()  See also FeServer::waitForConsequences().)"
	
	| theOther {BeEdition} theDirectFilter {Filter} theIndirectFilter {Filter} |
	otherTranscluders == NULL
		ifTrue: [theOther := NULL]
		ifFalse: [theOther := otherTranscluders beEdition].
	directFilter == NULL
		ifTrue: [theDirectFilter := FeServer endorsementFilterSpace fullRegion cast: Filter]
		ifFalse: [theDirectFilter := directFilter].
	indirectFilter == NULL
		ifTrue: [theIndirectFilter := FeServer endorsementFilterSpace fullRegion cast: Filter]
		ifFalse: [theIndirectFilter := indirectFilter].
	^FeEdition on: 
		(myBeEdition
			rangeTranscluders: positions
			with: theDirectFilter
			with: theIndirectFilter
			with: flags
			with: theOther)!
*/
}
/**
 * Essential.  Return a new FeEdition containing all Works which contain RangeElements of
 * this Edition and can be read by the CurrentKeyMaster. Returns an IDSpace Edition full of
 * PlaceHolders, which will be filled with Works as results come in.
 * If a filter is given, then only Works whose endorsements pass the Filter are returned.
 * If the localPresentOnly flag is clear, a persistent request will be created, and as new
 * Works come to be known to the Server, they will be filled into the resulting Edition.  If
 * it is set, only Works currently known to this Server are sure to be recorded into the
 * Trail.  (Some, but not all, Works which become known while this request is being processed
 * may be recorded.  If the request is followed by a FeServer::waitForConsequences(), no
 * Works which become known after the wait completes will be recorded.)
 * If the fromTransitiveContents flag is set, then the result includes Works which contain
 * RangeElements transitively contained in this Edition. (This may not be supported in 1.0)
 * If directContainersOnly is set, then only Works which are directly on Editions which are
 * RangeElements of this Edition are returned (and not Works which are on Editions which have
 * them as sub-Editions).
 * If otherTranscluders is given, this records works into that trail.
 * (For convenience, you can attach a TransclusionDetector to the result Edition.  See
 * FeEdition::addFillRangeDetector()  See also FeServer::waitForConsequences().)
 * { <k,l,w> | w's contains self, w passes filter}
 */
public FeEdition rangeWorks(XnRegion positions, Filter filter, int flags, FeEdition otherTranscluders) {
	BeEdition theOther;
	Filter theFilter;
	if (otherTranscluders == null) {
		theOther = null;
	}
	else {
		theOther = otherTranscluders.beEdition();
	}
	if (filter == null) {
		theFilter = (Filter) FeServer.endorsementFilterSpace().fullRegion();
	}
	else {
		theFilter = filter;
	}
	return FeEdition.on((myBeEdition.rangeWorks(positions, theFilter, flags, theOther)));
/*
udanax-top.st:20957:FeEdition methodsFor: 'comparing'!
{FeEdition CLIENT} rangeWorks: positions {XnRegion default: NULL}
	with: filter {Filter default: NULL}
	with: flags {Int32 default: Int32Zero}
	with: otherTranscluders {FeEdition default: NULL}
	"Essential.  Return a new FeEdition containing all Works which contain RangeElements of this Edition and can be read by the CurrentKeyMaster. Returns an IDSpace Edition full of PlaceHolders, which will be filled with Works as results come in.
	If a filter is given, then only Works whose endorsements pass the Filter are returned.
	If the localPresentOnly flag is clear, a persistent request will be created, and as new Works come to be known to the Server, they will be filled into the resulting Edition.  If it is set, only Works currently known to this Server are sure to be recorded into the Trail.  (Some, but not all, Works which become known while this request is being processed may be recorded.  If the request is followed by a FeServer::waitForConsequences(), no Works which become known after the wait completes will be recorded.)
	If the fromTransitiveContents flag is set, then the result includes Works which contain RangeElements transitively contained in this Edition. (This may not be supported in 1.0)
	If directContainersOnly is set, then only Works which are directly on Editions which are RangeElements of this Edition are returned (and not Works which are on Editions which have them as sub-Editions).
	If otherTranscluders is given, this records works into that trail.
	(For convenience, you can attach a TransclusionDetector to the result Edition.  See FeEdition::addFillRangeDetector()  See also FeServer::waitForConsequences().)
	
	{ <k,l,w> | w's contains self, w passes filter}"
		
	| theOther {BeEdition} theFilter {Filter} |
	otherTranscluders == NULL
		ifTrue: [theOther := NULL]
		ifFalse: [theOther := otherTranscluders beEdition].
	filter == NULL
		ifTrue: [theFilter := FeServer endorsementFilterSpace fullRegion cast: Filter]
		ifFalse: [theFilter := filter].
	^FeEdition on: 
		(myBeEdition
			rangeWorks: positions
			with: theFilter
			with: flags
			with: theOther)!
*/
}
/**
 * Return the subset of the positions of this Edition which  have RangeElements that are in
 * the other Edition.
 * If nestThis flag is set, then returns not only positions of RangeElements which are in the
 * other, but also positions of Editions which have RangeElements which are in the other, or
 * which have other such Editions, recursively.  (This searches down to, but not across, work
 * boundaries.)
 * If nestOther flag is set, then looks not only for RangeElements which are values of the
 * other Edition, but also those which are values of sub-Editions of the other Edition. (This
 * option will probably not be supported in version 1.0).
 * If both flags are false, then equivalent to:
 * this->mapSharedTo (other)->domain ()
 * { k1 | <k1,l1,v1> in self and <k2,l2,v2> in other and v1 is same as v2 }
 */
public XnRegion sharedRegion(FeEdition other, int flags) {
	return myBeEdition.sharedRegion(other.beEdition(), flags);
/*
udanax-top.st:20985:FeEdition methodsFor: 'comparing'!
{XnRegion CLIENT} sharedRegion: other {FeEdition}
	with: flags {Int32 default: Int32Zero}
	"Return the subset of the positions of this Edition which  have RangeElements that are in the other Edition.
	If nestThis flag is set, then returns not only positions of RangeElements which are in the other, but also positions of Editions which have RangeElements which are in the other, or which have other such Editions, recursively.  (This searches down to, but not across, work boundaries.)
	If nestOther flag is set, then looks not only for RangeElements which are values of the other Edition, but also those which are values of sub-Editions of the other Edition. (This option will probably not be supported in version 1.0).
	If both flags are false, then equivalent to:
		this->mapSharedTo (other)->domain ()
	
	{ k1 | <k1,l1,v1> in self and <k2,l2,v2> in other and v1 is same as v2 }"
	
	^myBeEdition sharedRegion: other beEdition with: flags!
*/
}
/**
 * Essential.  Return a new FeEdition consisting of the subset of this Edition whose
 * RangeElements are in the other Edition. If the same RangeElement is in this Edition at
 * several different positions, all positions will be in the result (provided the
 * RangeElement is also in the other Edition).
 * Equivalent to:
 * this->copy (this->sharedRegion (other, flags)).
 * { <k1,l1,v1> | <k1,l1,v1> in self and <k2,l2,v2> in other and v1 is same as v2 }
 */
public FeEdition sharedWith(FeEdition other, int flags) {
	return FeEdition.on((myBeEdition.sharedWith(other.beEdition(), flags)), myLabel);
/*
udanax-top.st:20997:FeEdition methodsFor: 'comparing'!
{FeEdition CLIENT} sharedWith: other {FeEdition}
	with: flags {Int32 default: Int32Zero}
	"Essential.  Return a new FeEdition consisting of the subset of this Edition whose RangeElements are in the other Edition. If the same RangeElement is in this Edition at several different positions, all positions will be in the result (provided the RangeElement is also in the other Edition).
	Equivalent to:
		this->copy (this->sharedRegion (other, flags)).
		
	{ <k1,l1,v1> | <k1,l1,v1> in self and <k2,l2,v2> in other and v1 is same as v2 }"
		
	^FeEdition on: (myBeEdition
			sharedWith: other beEdition with: flags)
		with: myLabel!
*/
}
/**
 * Essential.  Adds to the endorsements on this Edition.  The region of
 * additionalEndorsements must consist of a finite number of (club ID, token ID) pairs.
 * CurrentKeyMaster must hold the signature authority of all the Clubs used to endorse; the
 * request will blast and do nothing if any of the required authority is lacking.  (Redoing
 * an endorse() undoes a retract())
 */
public void endorse(CrossRegion additionalEndorsements) {
	FeRangeElement.validateEndorsement(additionalEndorsements, ((FeKeyMaster) CurrentKeyMaster.fluidGet()));
	myBeEdition.endorse(additionalEndorsements);
/*
udanax-top.st:21011:FeEdition methodsFor: 'endorsing'!
{void CLIENT} endorse: additionalEndorsements {CrossRegion}
	"Essential.  Adds to the endorsements on this Edition.  The region of additionalEndorsements must consist of a finite number of (club ID, token ID) pairs.  CurrentKeyMaster must hold the signature authority of all the Clubs used to endorse; the request will blast and do nothing if any of the required authority is lacking.  (Redoing an endorse() undoes a retract())"
	
	FeRangeElement validateEndorsement: additionalEndorsements with: CurrentKeyMaster fluidGet.
	myBeEdition endorse: additionalEndorsements!
*/
}
/**
 * Essential.  Return all of the endorsements which have been placed on this Edition and not
 * retracted.
 */
public CrossRegion endorsements() {
	return myBeEdition.endorsements();
/*
udanax-top.st:21017:FeEdition methodsFor: 'endorsing'!
{CrossRegion CLIENT} endorsements
	"Essential.  Return all of the endorsements which have been placed on this Edition and not retracted."
	
	^myBeEdition endorsements!
*/
}
/**
 * Essential.  Removes endorsements from this Edition.  This requires that the
 * CurrentKeyMaster hold signature authority for all of the Clubs whose endorsements are in
 * the list; will blast and do nothing if any of the required authority is lacking, even if
 * the endorsements weren't there to be retracted.  Ignores all endorsements which you could
 * have removed, but which don't happen to be there right now.
 * In the current release removed endorsements aren't preserved, so they vanish forever.
 * Beginning in some future release removed endorsements will become inactive, but it will be
 * possible to detect that they once had been present.  The intent is for a removed
 * endorsement to be analogous to a signature that has been struck out.  You can express that
 * you changed your mind, but you can't undo the past.
 */
public void retract(CrossRegion endorsements) {
	FeRangeElement.validateEndorsement(endorsements, ((FeKeyMaster) CurrentKeyMaster.fluidGet()));
	myBeEdition.retract(endorsements);
/*
udanax-top.st:21022:FeEdition methodsFor: 'endorsing'!
{void CLIENT} retract: endorsements {CrossRegion}
	"Essential.  Removes endorsements from this Edition.  This requires that the CurrentKeyMaster hold signature authority for all of the Clubs whose endorsements are in the list; will blast and do nothing if any of the required authority is lacking, even if the endorsements weren't there to be retracted.  Ignores all endorsements which you could have removed, but which don't happen to be there right now.
	
	In the current release removed endorsements aren't preserved, so they vanish forever.  Beginning in some future release removed endorsements will become inactive, but it will be possible to detect that they once had been present.  The intent is for a removed endorsement to be analogous to a signature that has been struck out.  You can express that you changed your mind, but you can't undo the past."
	
	FeRangeElement validateEndorsement: endorsements with: CurrentKeyMaster fluidGet.
	myBeEdition retract: endorsements!
*/
}
/**
 * Essential.  Return all the unretracted endorsements on this Edition along with those on
 * any Works directly on it which the CurrentKeyMaster has permission to read.
 */
public CrossRegion visibleEndorsements() {
	return myBeEdition.visibleEndorsements();
/*
udanax-top.st:21030:FeEdition methodsFor: 'endorsing'!
{CrossRegion CLIENT} visibleEndorsements
	"Essential.  Return all the unretracted endorsements on this Edition along with those on any Works directly on it which the CurrentKeyMaster has permission to read."
	
	^myBeEdition visibleEndorsements!
*/
}
/**
 * Essential.  Connect a FillRangeDetector to the underlying BeEdition so that when any of
 * the PlaceHolders in that Edition become any other kind of RangeElement, then the Detector
 * will be triggered with an Edition containing the new RangeElements (but not necessarily at
 * the same positions, or even in the same CoordinateSpace). If there already are
 * non-PlaceHolders, then the Detector is triggered immediately with those RangeElements.
 * See FillRangeDetector::allFilled (Edition * newIdentities).
 */
public void addFillRangeDetector(FeFillRangeDetector detector) {
	myBeEdition.addDetector(detector);
/*
udanax-top.st:21037:FeEdition methodsFor: 'becoming'!
{void} addFillRangeDetector: detector {FeFillRangeDetector}
	"Essential.  Connect a FillRangeDetector to the underlying BeEdition so that when any of the PlaceHolders in that Edition become any other kind of RangeElement, then the Detector will be triggered with an Edition containing the new RangeElements (but not necessarily at the same positions, or even in the same CoordinateSpace). If there already are non-PlaceHolders, then the Detector is triggered immediately with those RangeElements.
	See FillRangeDetector::allFilled (Edition * newIdentities)."
	
	myBeEdition addDetector: detector!
*/
}
/**
 * Essential.  Return the region consisting of all locations at which my RangeElements can
 * NOT be made identical to the corresponding RangeElements in the other Edition. (This seems
 * like the opposite of what you want, but in fact it makes it easy to check for success.)
 * Does not check whether you have permissions to do so, just whether it could be done by
 * someone with the appropriate permissions. See rangeOwners.
 */
public XnRegion canMakeRangeIdentical(FeEdition newIdentities, XnRegion positions) {
	Dean.shouldImplement();
	return null;
/*
udanax-top.st:21043:FeEdition methodsFor: 'becoming'!
{XnRegion CLIENT} canMakeRangeIdentical: newIdentities {FeEdition}
	with: positions {XnRegion default: NULL}
	"Essential.  Return the region consisting of all locations at which my RangeElements can NOT be made identical to the corresponding RangeElements in the other Edition. (This seems like the opposite of what you want, but in fact it makes it easy to check for success.)
	Does not check whether you have permissions to do so, just whether it could be done by someone with the appropriate permissions. See rangeOwners."
	
	Dean shouldImplement.
	^NULL "fodder"!
*/
}
/**
 * Essential.  Return a FillRangeDetector so that when any of the PlaceHolders in this
 * Edition become any other kind of RangeElement, then the Detector will be triggered with an
 * Edition containing the new RangeElements (but not necessarily at the same positions, or
 * even in the same CoordinateSpace). If there already are non-PlaceHolders, then the
 * Detector is triggered immediately with those RangeElements.
 * See FillRangeDetector::allFilled (Edition * newIdentities).
 */
public FeFillRangeDetector fillRangeDetector() {
	Dean.shouldImplement();
	addFillRangeDetector(null);
	return null;
/*
udanax-top.st:21051:FeEdition methodsFor: 'becoming'!
{FeFillRangeDetector CLIENT} fillRangeDetector
	"Essential.  Return a FillRangeDetector so that when any of the PlaceHolders in this Edition become any other kind of RangeElement, then the Detector will be triggered with an Edition containing the new RangeElements (but not necessarily at the same positions, or even in the same CoordinateSpace). If there already are non-PlaceHolders, then the Detector is triggered immediately with those RangeElements.
	See FillRangeDetector::allFilled (Edition * newIdentities)."
	
	Dean shouldImplement.
	self addFillRangeDetector: NULL.
	^NULL "fodder"!
*/
}
/**
 * Essential.  Try to change the identity of each RangeElements of this Edition which are in
 * the Region (or all if no Region supplied) to that of the RangeElement at the same position
 * in the other Edition. Returns the subset of this Edition which did not end up with the new
 * identities, because of
 * - lack of ownership authority
 * - different contents
 * - contents of other edition unreadable
 * - incompatible types
 * - no corresponding new identity
 * Note that the labels on the RangeElements need not match and will NOT be changed.
 */
public FeEdition makeRangeIdentical(FeEdition newIdentities, XnRegion positions) {
	BeEdition never;
	BeEdition maybe;
	BeEdition trial;
	Pair result;
	XnRegion theRegion;
	/* Keep trying the primitive routine until it says it can't do any more */
	Someone.knownBug();
	/* put loop into server loop */
	if ( ! (coordinateSpace().isEqual(newIdentities.coordinateSpace()))) {
		return this;
	}
	never = ((BeGrandMap) CurrentGrandMap.fluidGet()).newEmptyEdition(coordinateSpace());
	maybe = myBeEdition;
	theRegion = maybe.domain();
	if (positions != null) {
		theRegion = theRegion.intersect(positions);
	}
	trial = newIdentities.beEdition().copy(theRegion);
	while ((result = maybe.tryAllBecome(trial)).fetchRight() != null) {
		never = never.combine(((BeEdition) result.left()));
		maybe = (BeEdition) result.right();
		trial = trial.copy(maybe.domain());
	}
	return FeEdition.on(never, myLabel);
/*
udanax-top.st:21059:FeEdition methodsFor: 'becoming'!
{FeEdition CLIENT} makeRangeIdentical: newIdentities {FeEdition}
	with: positions {XnRegion default: NULL}
	"Essential.  Try to change the identity of each RangeElements of this Edition which are in the Region (or all if no Region supplied) to that of the RangeElement at the same position in the other Edition. Returns the subset of this Edition which did not end up with the new identities, because of
		- lack of ownership authority
		- different contents
		- contents of other edition unreadable
		- incompatible types
		- no corresponding new identity
		
Note that the labels on the RangeElements need not match and will NOT be changed."
	
	| never {BeEdition} maybe {BeEdition} trial {BeEdition} result {Pair of: BeEdition} theRegion {XnRegion} |
	"Keep trying the primitive routine until it says it can't do any more"
	self knownBug. "put loop into server loop"
	(self coordinateSpace isEqual: newIdentities coordinateSpace) ifFalse:
		[^self].
	never := CurrentGrandMap fluidGet newEmptyEdition: self coordinateSpace.
	maybe := myBeEdition.
	theRegion := maybe domain.
	positions ~~ NULL ifTrue:
		[theRegion := theRegion intersect: positions].
	trial := newIdentities beEdition copy: theRegion.
	[(result := maybe tryAllBecome: trial) fetchRight ~~ NULL]
		whileTrue:
			[never := never combine: (result left cast: BeEdition).
			maybe := result right cast: BeEdition.
			trial := trial copy: maybe domain].
	^FeEdition on: never with: myLabel!
*/
}
/**
 * The owners of all the RangeElements in the given Region, or in the entire
 * Edition if no Region is specified.
 */
public IDRegion rangeOwners(XnRegion positions) {
	return myBeEdition.rangeOwners(positions);
/*
udanax-top.st:21088:FeEdition methodsFor: 'becoming'!
{IDRegion CLIENT} rangeOwners: positions {XnRegion default: NULL} 
	"The owners of all the RangeElements in the given Region, or in the entire 
	Edition if no Region is specified."
	^myBeEdition rangeOwners: positions!
*/
}
/**
 * Essential.  Remove a Detector which had been added to this Edition. You should remove
 * every Detector you add, although they will go away automatically when a client session
 * terminates.
 */
public void removeFillRangeDetector(FeFillRangeDetector detector) {
	if ( ! (Heaper.isDestructed(myBeEdition))) {
		myBeEdition.removeDetector(detector);
	}
/*
udanax-top.st:21094:FeEdition methodsFor: 'becoming'!
{void} removeFillRangeDetector: detector {FeFillRangeDetector}
	"Essential.  Remove a Detector which had been added to this Edition. You should remove every Detector you add, although they will go away automatically when a client session terminates."
	
	(Heaper isDestructed: myBeEdition) ifFalse:
		[myBeEdition removeDetector: detector]!
*/
}
/**
 * Changes the owner of all RangeElements in the Edition (but not the Edition itself!!);
 * requires the authority of the current owner of each range element. If a Region is
 * supplied, then only sets those in the region.
 * Returns the subset of this Edition which is in the Region whose owners did not end up
 * being the new Owner because of lack of authority.
 */
public FeEdition setRangeOwners(ID newOwner, XnRegion region) {
	XnRegion theRegion;
	if (region == null) {
		theRegion = domain();
	}
	else {
		theRegion = region;
	}
	return FeEdition.on((myBeEdition.setRangeOwners(newOwner, theRegion)), myLabel);
/*
udanax-top.st:21100:FeEdition methodsFor: 'becoming'!
{FeEdition CLIENT} setRangeOwners: newOwner {ID} with: region {XnRegion default: NULL}
	"Changes the owner of all RangeElements in the Edition (but not the Edition itself!!); requires the authority of the current owner of each range element. If a Region is supplied, then only sets those in the region.
	Returns the subset of this Edition which is in the Region whose owners did not end up being the new Owner because of lack of authority."
	
	| theRegion {XnRegion} |
	region == NULL
		ifTrue: [theRegion := self domain]
		ifFalse: [theRegion := region].
	^FeEdition on: (myBeEdition setRangeOwners: newOwner with: theRegion) with: myLabel!
*/
}
public FeLabel label() {
	return myLabel;
/*
udanax-top.st:21112:FeEdition methodsFor: 'labelling'!
{FeLabel} label
	^myLabel!
*/
}
/**
 * Return a region consisting of exactly the positions in this Edition which are associated
 * with the given label.
 * { k | <k,label,v> in self }
 */
public XnRegion positionsLabelled(FeLabel label) {
	Someone.thingToDo();
	/* rename Be protocol */
	return myBeEdition.keysLabelled(((BeLabel) label.fetchBe()));
/*
udanax-top.st:21116:FeEdition methodsFor: 'labelling'!
{XnRegion CLIENT} positionsLabelled: label {FeLabel}
	"Return a region consisting of exactly the positions in this Edition which are associated with the given label.
	
	{ k | <k,label,v> in self }"
	
	self thingToDo. "rename Be protocol"
	^myBeEdition keysLabelled: (label fetchBe cast: BeLabel)!
*/
}
/**
 * Return a new FeEdition which is a copy of this Edition with the contained Edition at the
 * given position replaced by the given Edition, but with the Label at that position
 * unchanged.  Equivalent to
 * this->with (position, edition->relabelled (this->get (position)->label ())).
 * Note that rebind() is useless (and blasts) when a non-edition RangeElement is at the given
 * position.
 * { <k,l,v> | ((k isEqual: position) and (v is same as edition))
 * or (<k,l,v> in self and k !!= position) }
 */
public FeEdition rebind(Position position, FeEdition edition) {
	return FeEdition.fromOne(position, (edition.relabelled(((FeEdition) (get(position))).label())));
/*
udanax-top.st:21124:FeEdition methodsFor: 'labelling'!
{FeEdition CLIENT} rebind: position {Position} with: edition {FeEdition}
	"Return a new FeEdition which is a copy of this Edition with the contained Edition at the given position replaced by the given Edition, but with the Label at that position unchanged.  Equivalent to
		this->with (position, edition->relabelled (this->get (position)->label ())).
	Note that rebind() is useless (and blasts) when a non-edition RangeElement is at the given position.
		
	{ <k,l,v> | ((k isEqual: position) and (v is same as edition)) 
				or (<k,l,v> in self and k !!= position) }"
	
	^self class fromOne: position with: (edition relabelled: ((self get: position) cast: FeEdition) label)!
*/
}
public FeRangeElement relabelled(FeLabel label) {
	return FeEdition.on(myBeEdition, label);
/*
udanax-top.st:21135:FeEdition methodsFor: 'labelling'!
{FeRangeElement} relabelled: label {FeLabel}
	^FeEdition on: myBeEdition with: label!
*/
}
public BeEdition beEdition() {
	return myBeEdition;
/*
udanax-top.st:21141:FeEdition methodsFor: 'server accessing'!
{BeEdition} beEdition
	^myBeEdition!
*/
}
/**
 * Return an object that wraps up any run-time state that might be needed inside the Be
 * system.  Right now that means labels.
 */
public BeCarrier carrier() {
	return BeCarrier.make(((BeLabel) myLabel.getOrMakeBe()), myBeEdition);
/*
udanax-top.st:21144:FeEdition methodsFor: 'server accessing'!
{BeCarrier} carrier
	"Return an object that wraps up any run-time state that might be needed inside the Be system.  Right now that means labels."
	
	^BeCarrier make: (myLabel getOrMakeBe cast: BeLabel) with: myBeEdition!
*/
}
/**
 * The value at the position, or NULL if there is none
 */
public FeRangeElement fetch(Position position) {
	return myBeEdition.fetch(position);
/*
udanax-top.st:21149:FeEdition methodsFor: 'server accessing'!
{FeRangeElement} fetch: position {Position}
	"The value at the position, or NULL if there is none"
	^myBeEdition fetch: position!
*/
}
public BeRangeElement fetchBe() {
	return myBeEdition;
/*
udanax-top.st:21153:FeEdition methodsFor: 'server accessing'!
{BeRangeElement | NULL} fetchBe
	^myBeEdition!
*/
}
public BeRangeElement getOrMakeBe() {
	return myBeEdition;
/*
udanax-top.st:21157:FeEdition methodsFor: 'server accessing'!
{BeRangeElement} getOrMakeBe
	^myBeEdition!
*/
}
/**
 * These don't change as long as someone has a handle on them.
 */
public FeRangeElement again() {
	return this;
/*
udanax-top.st:21163:FeEdition methodsFor: 'client implementation'!
{FeRangeElement} again
	"These don't change as long as someone has a handle on them."
	
	^self!
*/
}
public boolean canMakeIdentical(FeRangeElement newIdentity) {
	if ( ! (isIdentical(newIdentity))) {
		throw new UnimplementedException();
	}
	return true;
/*
udanax-top.st:21168:FeEdition methodsFor: 'client implementation'!
{BooleanVar} canMakeIdentical: newIdentity {FeRangeElement}
	(self isIdentical: newIdentity) ifFalse:
		[self unimplemented].
	^true!
*/
}
public void makeIdentical(FeRangeElement newIdentity) {
	if ( ! (isIdentical(newIdentity))) {
		throw new UnimplementedException();
	}
/*
udanax-top.st:21174:FeEdition methodsFor: 'client implementation'!
{void} makeIdentical: newIdentity {FeRangeElement}
	(self isIdentical: newIdentity) ifFalse:
		[self unimplemented]!
*/
}
public FeEdition(BeEdition beEdition, FeLabel label) {
	super();
	myBeEdition = beEdition;
	myLabel = label;
/*
udanax-top.st:21181:FeEdition methodsFor: 'private: create'!
create: beEdition {BeEdition} with: label {FeLabel}
	super create.
	myBeEdition := beEdition.
	myLabel _ label.!
*/
}
public void printOn(PrintWriter oo) {
	String before;
	if (isEmpty()) {
		oo.print("Edition()");
		return ;
	}
	before = "Edition(";
	Stepper stomper = (retrieve(null, null, FeEdition.IGNOREUTOTALUORDERING()));
	for (; stomper.hasValue(); stomper.step()) {
		FeBundle bundle = (FeBundle) stomper.fetch();
		if (bundle == null) {
			continue ;
		}
		oo.print(before);
		oo.print(bundle.region());
		oo.print(" -> ");
		if (bundle instanceof FeArrayBundle) {
			FeArrayBundle array = (FeArrayBundle) bundle;
			oo.print(array.array());
		}
		else if (bundle instanceof FeElementBundle) {
			FeElementBundle range = (FeElementBundle) bundle;
			oo.print(range.element());
		}
		else if (bundle instanceof FePlaceHolderBundle) {
			FePlaceHolderBundle place = (FePlaceHolderBundle) bundle;
			oo.print("{...}");
		}
		before = ", ";
	}
	stomper.destroy();
	oo.print(")");
/*
udanax-top.st:21189:FeEdition methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	| before {char star} |
	self isEmpty ifTrue: [oo << 'Edition()'. ^VOID].
	before := 'Edition('.
	(self retrieve: NULL with: NULL with: FeEdition IGNORE.U.TOTAL.U.ORDERING) forEach:
		[ :bundle {FeBundle} |
		oo << before << bundle region << ' -> '.
		bundle cast: FeArrayBundle into:
			[ :array | oo << array array]
		cast: FeElementBundle into:
			[ :range | oo << range element]
		cast: FePlaceHolderBundle into:
			[ :place | oo << '{...}'].
		before := ', '].
	oo << ')'!
*/
}
public XnRegion canMakeRangeIdentical(FeEdition newIdentities) {
	return canMakeRangeIdentical(newIdentities, null);
/*
udanax-top.st:21208:FeEdition methodsFor: 'smalltalk: defaults'!
{XnRegion CLIENT} canMakeRangeIdentical: newIdentities {FeEdition}
	^self canMakeRangeIdentical: newIdentities with: NULL!
*/
}
public boolean isRangeIdentical(FeEdition other) {
	return isRangeIdentical(other, null);
/*
udanax-top.st:21212:FeEdition methodsFor: 'smalltalk: defaults'!
{BooleanVar CLIENT} isRangeIdentical: other {FeEdition}
	^self isRangeIdentical: other with: NULL!
*/
}
public FeEdition makeRangeIdentical(FeEdition newIdentities) {
	return makeRangeIdentical(newIdentities, null);
/*
udanax-top.st:21216:FeEdition methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} makeRangeIdentical: newIdentities {FeEdition}
	^self makeRangeIdentical: newIdentities with: NULL!
*/
}
public FeEdition notSharedWith(FeEdition other) {
	return notSharedWith(other, 0);
/*
udanax-top.st:21220:FeEdition methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} notSharedWith: other {FeEdition}
	^self notSharedWith: other with: 0!
*/
}
public FeEdition rangeTranscluders() {
	return rangeTranscluders(null, null, null, 0, null);
/*
udanax-top.st:21224:FeEdition methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} rangeTranscluders
	
	^self rangeTranscluders: NULL with: NULL with: NULL with: Int32Zero with: NULL!
*/
}
public FeEdition rangeTranscluders(XnRegion positions) {
	return rangeTranscluders(positions, null, null, 0, null);
/*
udanax-top.st:21228:FeEdition methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} rangeTranscluders: positions {XnRegion default: NULL}
	
	^self rangeTranscluders: positions with: NULL with: NULL with: Int32Zero with: NULL!
*/
}
public FeEdition rangeTranscluders(XnRegion positions, Filter filter) {
	return rangeTranscluders(positions, filter, null, 0, null);
/*
udanax-top.st:21232:FeEdition methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} rangeTranscluders: positions {XnRegion default: NULL}
	with: filter {Filter default: NULL}
	
	^self rangeTranscluders: positions with: filter with: NULL with: Int32Zero with: NULL!
*/
}
public FeEdition rangeTranscluders(XnRegion positions, Filter filter, Filter transitiveFilter) {
	return rangeTranscluders(positions, filter, transitiveFilter, 0, null);
/*
udanax-top.st:21237:FeEdition methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} rangeTranscluders: positions {XnRegion default: NULL}
	with: filter {Filter default: NULL}
	with: transitiveFilter {Filter default: NULL}
	
	^self rangeTranscluders: positions with: filter with: transitiveFilter with: Int32Zero with: NULL!
*/
}
public FeEdition rangeTranscluders(XnRegion positions, Filter filter, Filter transitiveFilter, int flags) {
	return rangeTranscluders(positions, filter, transitiveFilter, flags, null);
/*
udanax-top.st:21243:FeEdition methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} rangeTranscluders: positions {XnRegion default: NULL}
	with: filter {Filter default: NULL}
	with: transitiveFilter {Filter default: NULL}
	with: flags {Int32 default: Int32Zero}
	
	^self rangeTranscluders: positions with: filter with: transitiveFilter with: flags with: NULL!
*/
}
public FeEdition rangeWorks() {
	return rangeWorks(null, null, 0, null);
/*
udanax-top.st:21250:FeEdition methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} rangeWorks
	^self rangeWorks: NULL with: NULL with: 0 with: NULL!
*/
}
public FeEdition rangeWorks(XnRegion region) {
	return rangeWorks(region, null, 0, null);
/*
udanax-top.st:21253:FeEdition methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} rangeWorks: region {XnRegion default: NULL}
	^self rangeWorks: region with: NULL with: 0 with: NULL!
*/
}
public FeEdition rangeWorks(XnRegion region, Filter filter) {
	return rangeWorks(region, filter, 0, null);
/*
udanax-top.st:21256:FeEdition methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} rangeWorks: region {XnRegion default: NULL}
	with: filter {Filter default: NULL}
	^self rangeWorks: region with: filter with: 0 with: NULL!
*/
}
public FeEdition rangeWorks(XnRegion region, Filter filter, int flags) {
	return rangeWorks(region, filter, flags, null);
/*
udanax-top.st:21260:FeEdition methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} rangeWorks: region {XnRegion default: NULL}
	with: filter {Filter default: NULL}
	with: flags {Int32 default: Int32Zero}
	
	^self rangeWorks: region with: filter with: flags with: NULL!
*/
}
public Stepper retrieve() {
	return retrieve(null, null, 0);
/*
udanax-top.st:21266:FeEdition methodsFor: 'smalltalk: defaults'!
{(Stepper of: Bundle) CLIENT} retrieve
	^self retrieve: NULL with: NULL with: 0!
*/
}
public Stepper retrieve(XnRegion positions) {
	return retrieve(positions, null, 0);
/*
udanax-top.st:21270:FeEdition methodsFor: 'smalltalk: defaults'!
{(Stepper of: Bundle) CLIENT} retrieve: positions {XnRegion default: NULL}
	^self retrieve: positions with: NULL with: 0!
*/
}
public Stepper retrieve(XnRegion positions, OrderSpec order) {
	return retrieve(positions, order, 0);
/*
udanax-top.st:21274:FeEdition methodsFor: 'smalltalk: defaults'!
{(Stepper of: Bundle) CLIENT} retrieve: positions {XnRegion default: NULL} with: order {OrderSpec default: NULL}
	^self retrieve: positions with: order with: 0!
*/
}
public FeEdition setRangeOwners(ID newOwner) {
	return setRangeOwners(newOwner, null);
/*
udanax-top.st:21278:FeEdition methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} setRangeOwners: newOwner {ID}
	^self setRangeOwners: newOwner with: NULL!
*/
}
public XnRegion sharedRegion(FeEdition other) {
	return sharedRegion(other, 0);
/*
udanax-top.st:21282:FeEdition methodsFor: 'smalltalk: defaults'!
{XnRegion CLIENT} sharedRegion: other {FeEdition}
	^self sharedRegion: other with: 0!
*/
}
public FeEdition sharedWith(FeEdition other) {
	return sharedWith(other, 0);
/*
udanax-top.st:21285:FeEdition methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} sharedWith: other {FeEdition}
	^self sharedWith: other with: 0!
*/
}
public TableStepper stepper() {
	return stepper(null, null);
/*
udanax-top.st:21288:FeEdition methodsFor: 'smalltalk: defaults'!
{TableStepper CLIENT of: FeRangeElement} stepper
	^self stepper: NULL with: NULL!
*/
}
public TableStepper stepper(XnRegion region) {
	return stepper(region, null);
/*
udanax-top.st:21291:FeEdition methodsFor: 'smalltalk: defaults'!
{TableStepper CLIENT of: FeRangeElement} stepper: region {XnRegion default: NULL}
	^self stepper: region with: NULL!
*/
}
/**
 * @deprecated
 */
public void addCompletionDetector(FeCompletionDetector detector) {
	throw new PasseException();
/*
udanax-top.st:21296:FeEdition methodsFor: 'smalltalk: passe'!
{void} addCompletionDetector: detector {FeCompletionDetector}
	self passe!
*/
}
/**
 * @deprecated
 */
public void addFillInDetector(FeFillInDetector detector) {
	throw new PasseException();
/*
udanax-top.st:21299:FeEdition methodsFor: 'smalltalk: passe'!
{void} addFillInDetector: detector {FeFillInDetector}
	self passe!
*/
}
/**
 * @deprecated
 */
public FeEdition allBecome(FeEdition newIdentities, XnRegion positions) {
	throw new PasseException();
/*
udanax-top.st:21303:FeEdition methodsFor: 'smalltalk: passe'!
{FeEdition} allBecome: newIdentities {FeEdition} with: positions {XnRegion default: NULL}
	self passe "makeRangeIdentical"!
*/
}
/**
 * @deprecated
 */
public PrimArray asArray(XnRegion positions, OrderSpec ordering) {
	throw new PasseException();
/*
udanax-top.st:21307:FeEdition methodsFor: 'smalltalk: passe'!
{PrimArray} asArray: positions {XnRegion default: NULL}
	with: ordering {OrderSpec default: NULL}
	self passe "use retrieve"!
*/
}
/**
 * @deprecated
 */
public XnRegion keysLabelled(FeLabel label) {
	throw new PasseException();
/*
udanax-top.st:21312:FeEdition methodsFor: 'smalltalk: passe'!
{XnRegion} keysLabelled: label {FeLabel}
	self passe!
*/
}
/**
 * Some subset of this Edition, containing the given position, all with the same owner
 * { <k,l,v> | <k,l,v> in self and <position,l2,v2> in self and v.owner == v2.owner }
 * @deprecated
 */
public FeEdition parcelAt(Position position) {
	throw new PasseException();
/*
udanax-top.st:21316:FeEdition methodsFor: 'smalltalk: passe'!
{FeEdition} parcelAt: position {Position}
	"Some subset of this Edition, containing the given position, all with the same owner
	
	{ <k,l,v> | <k,l,v> in self and <position,l2,v2> in self and v.owner == v2.owner }"
	
self passe!
*/
}
/**
 * Divides this Edition into pieces each of whose RangeElements are all owned by a single
 * Club.
 * { <k1, { <k2,v2> } > | <k2,v2> in self and k1 == v2's owner }
 * @deprecated
 */
public FeEdition parcels() {
	throw new PasseException();
/*
udanax-top.st:21323:FeEdition methodsFor: 'smalltalk: passe'!
{FeEdition of: ID and: FeEdition} parcels
	"Divides this Edition into pieces each of whose RangeElements are all owned by a single Club.
	
	{ <k1, { <k2,v2> } > | <k2,v2> in self and k1 == v2's owner }"
	
	self passe!
*/
}
/**
 * @deprecated
 */
public void removeCompletionDetector(FeCompletionDetector detector) {
	throw new PasseException();
/*
udanax-top.st:21330:FeEdition methodsFor: 'smalltalk: passe'!
{void} removeCompletionDetector: detector {FeCompletionDetector}
self passe!
*/
}
/**
 * @deprecated
 */
public void removeFillInDetector(FeFillInDetector detector) {
	throw new PasseException();
/*
udanax-top.st:21333:FeEdition methodsFor: 'smalltalk: passe'!
{void} removeFillInDetector: detector {FeFillInDetector}
	self passe!
*/
}
/**
 * Rearrange the positions of this Edition to lie in the given region, with the given
 * ordering. Equivalent to server->makeEdition (this->asArray (oldRegion, oldOrder),
 * newRegion, newOrder, NULL), except that it doesn't require everything to be in the same
 * zone (and is of course more efficient).
 * This message is tentative and may be removed from the protocol.
 * @deprecated
 */
public FeEdition reorganize(XnRegion oldRegion, OrderSpec oldOrder, XnRegion newRegion, OrderSpec newOrder) {
	throw new PasseException();
/*
udanax-top.st:21337:FeEdition methodsFor: 'smalltalk: passe'!
{FeEdition} reorganize: oldRegion {XnRegion | NULL}
	with: oldOrder {OrderSpec | NULL}
	with: newRegion {XnRegion | NULL}
	with: newOrder {OrderSpec | NULL}
	"Rearrange the positions of this Edition to lie in the given region, with the given ordering. Equivalent to server->makeEdition (this->asArray (oldRegion, oldOrder), newRegion, newOrder, NULL), except that it doesn't require everything to be in the same zone (and is of course more efficient).
	
	This message is tentative and may be removed from the protocol."
	
	^FeEdition on: (myBeEdition
			reorganize: oldRegion with: oldOrder with: newRegion with: newOrder) with: myLabel!
*/
}
/**
 * @deprecated
 */
public FeEdition setAllOwners(ID newOwner, XnRegion region) {
	throw new PasseException();
/*
udanax-top.st:21348:FeEdition methodsFor: 'smalltalk: passe'!
{FeEdition} setAllOwners: newOwner {ID} with: region {XnRegion default: NULL}
	self passe "setRangeOwners"!
*/
}
/**
 * @deprecated
 */
public FeEdition transclusions(XnRegion positions, Filter directFilter, Filter indirectFilter, int flags, FeEdition otherTransclusions) {
	throw new PasseException();
/*
udanax-top.st:21352:FeEdition methodsFor: 'smalltalk: passe'!
{FeEdition} transclusions: positions {XnRegion default: NULL}
	with: directFilter {Filter default: NULL}
	with: indirectFilter {Filter default: NULL}
	with: flags {Int32 default: Int32Zero}
	with: otherTransclusions {FeEdition default: NULL}
	self passe "rangeTranscluders"!
*/
}
/**
 * @deprecated
 */
public void unendorse(CrossRegion endorsements) {
	throw new PasseException();
/*
udanax-top.st:21360:FeEdition methodsFor: 'smalltalk: passe'!
{void} unendorse: endorsements {CrossRegion}
	self passe "retract"!
*/
}
/**
 * Essential.  A zone containing the given position, all with the same kind of RangeElements.
 * @deprecated
 */
public Pair zoneAt(Position position) {
	throw new PasseException();
/*
udanax-top.st:21364:FeEdition methodsFor: 'smalltalk: passe'!
{Pair of: PrimSpec and: FeEdition} zoneAt: position {Position}
	"Essential.  A zone containing the given position, all with the same kind of RangeElements."
	
	| result {Pair} |
	self passe.
	self thingToDo. "get rid of BeEdition protocol"
	result := myBeEdition zoneAt: position.
	^Pair make: result left
		with: (FeEdition on: (result left cast: BeEdition) with: myLabel)!
*/
}
/**
 * @deprecated
 */
public FeEdition zoneOf(PrimSpec values) {
	throw new PasseException();
/*
udanax-top.st:21374:FeEdition methodsFor: 'smalltalk: passe'!
{FeEdition} zoneOf: values {PrimSpec}
	self passe!
*/
}
/**
 * Divides this Edition up into pieces all of whose RangeElements have the same PrimSpec. If
 * no ordering is given, then uses the default full ordering for this CoordinateSpace.
 * @deprecated
 */
public TwoStepper zones(OrderSpec ordering) {
	throw new PasseException();
/*
udanax-top.st:21378:FeEdition methodsFor: 'smalltalk: passe'!
{TwoStepper of: PrimSpec and: FeEdition} zones: ordering {OrderSpec default: NULL}
	"Divides this Edition up into pieces all of whose RangeElements have the same PrimSpec. If no ordering is given, then uses the default full ordering for this CoordinateSpace."
	
	self passe!
*/
}
/**
 * Whether the given position is in the Edition. Equivalent to
 * this->domain ()->hasMember (position)
 */
public boolean includesKey(Position position) {
	return myBeEdition.includesKey(position);
/*
udanax-top.st:21385:FeEdition methodsFor: 'obsolete:'!
{BooleanVar} includesKey: position {Position}
	"Whether the given position is in the Edition. Equivalent to
		this->domain ()->hasMember (position)"
	
	^myBeEdition includesKey: position!
*/
}
/**
 * All of the keys in this Edition at which the given RangeElement can be found. Equivalent
 * to
 * this->sharedRegion (theServer ()->makeEditionWith (some position, value)).
 * { k | <k,l,v> in self and v is same as value }
 */
public XnRegion keysOf(FeRangeElement value) {
	return myBeEdition.keysOf(value);
/*
udanax-top.st:21391:FeEdition methodsFor: 'obsolete:'!
{XnRegion} keysOf: value {FeRangeElement}
	"All of the keys in this Edition at which the given RangeElement can be found. Equivalent to
		this->sharedRegion (theServer ()->makeEditionWith (some position, value)).
		
	{ k | <k,l,v> in self and v is same as value }"
	
	^myBeEdition keysOf: value!
*/
}
public void destruct() {
	myBeEdition.removeFeRangeElement(this);
	super.destruct();
/*
udanax-top.st:21401:FeEdition methodsFor: 'destruct'!
{void} destruct
	myBeEdition removeFeRangeElement: self.
	super destruct.!
*/
}
/**
 * Essential.  Creates an Edition mapping from a Region of keys to the values in an array.
 * The ordering specifies the correspondance between  the keys and the indices in the array.
 * If a Region is given, then it must have the same count as the array.  If no Region is
 * given, then it is taken to be the IntegerRegion from 0  to the size of the array. If no
 * OrderSpec is given, then it is the default ascending full ordering for that
 * CoordinateSpace.
 */
public static FeEdition fromArray(PrimArray values) {
	return fromArray(values, null, null);
/*
udanax-top.st:21415:FeEdition class methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} fromArray: values {PrimArray of: FeRangeElement}
	"Essential.  Creates an Edition mapping from a Region of keys to the values in an array. The ordering specifies the correspondance between  the keys and the indices in the array.
	If a Region is given, then it must have the same count as the array.  If no Region is given, then it is taken to be the IntegerRegion from 0  to the size of the array. If no OrderSpec is given, then it is the default ascending full ordering for that CoordinateSpace."
			
	^self fromArray: values with: NULL with: NULL!
*/
}
/**
 * Essential.  Creates an Edition mapping from a Region of keys to the values in an array.
 * The ordering specifies the correspondance between  the keys and the indices in the array.
 * If a Region is given, then it must have the same count as the array.  If no Region is
 * given, then it is taken to be the IntegerRegion from 0  to the size of the array. If no
 * OrderSpec is given, then it is the default ascending full ordering for that
 * CoordinateSpace.
 */
public static FeEdition fromArray(PrimArray values, XnRegion keys) {
	return fromArray(values, keys, null);
/*
udanax-top.st:21421:FeEdition class methodsFor: 'smalltalk: defaults'!
{FeEdition CLIENT} fromArray: values {PrimArray of: FeRangeElement}
	with: keys {XnRegion default: NULL}
	"Essential.  Creates an Edition mapping from a Region of keys to the values in an array. The ordering specifies the correspondance between  the keys and the indices in the array.
	If a Region is given, then it must have the same count as the array.  If no Region is given, then it is taken to be the IntegerRegion from 0  to the size of the array. If no OrderSpec is given, then it is the default ascending full ordering for that CoordinateSpace."
			
	^self fromArray: values with: keys with: NULL!
*/
}
/**
 * An empty Edition, with the given CoordinateSpace but no contents.
 */
public static FeEdition empty(CoordinateSpace keySpace) {
	return FeEdition.on((((BeGrandMap) CurrentGrandMap.fluidGet()).newEmptyEdition(keySpace)));
/*
udanax-top.st:21430:FeEdition class methodsFor: 'creation'!
{FeEdition CLIENT} empty: keySpace {CoordinateSpace}
	"An empty Edition, with the given CoordinateSpace but no contents."
	^FeEdition on: (CurrentGrandMap fluidGet newEmptyEdition: keySpace)!
*/
}
/**
 * Essential.  A singleton Edition mapping from a Region of keys (potentially infinite) to a
 * single value.
 */
public static FeEdition fromAll(XnRegion keys, FeRangeElement value) {
	return FeEdition.on((((BeGrandMap) CurrentGrandMap.fluidGet()).newEditionWithAll(keys, value.carrier())));
/*
udanax-top.st:21435:FeEdition class methodsFor: 'creation'!
{FeEdition CLIENT} fromAll: keys {XnRegion} with: value {FeRangeElement}
	"Essential.  A singleton Edition mapping from a Region of keys (potentially infinite) to a single value."
	^FeEdition on: 
		(CurrentGrandMap fluidGet
			newEditionWithAll: keys
			with: value carrier)!
*/
}
/**
 * Essential.  Creates an Edition mapping from a Region of keys to the values in an array.
 * The ordering specifies the correspondance between  the keys and the indices in the array.
 * If a Region is given, then it must have the same count as the array.  If no Region is
 * given, then it is taken to be the IntegerRegion from 0  to the size of the array. If no
 * OrderSpec is given, then it is the default ascending full ordering for that
 * CoordinateSpace.
 */
public static FeEdition fromArray(PrimArray values, XnRegion keys, OrderSpec ordering) {
	XnRegion theKeys;
	OrderSpec theOrdering;
	if (keys == null) {
		theKeys = IntegerRegion.make(0, values.count());
	}
	else {
		theKeys = keys;
	}
	if (ordering == null) {
		theOrdering = theKeys.coordinateSpace().getAscending();
	}
	else {
		theOrdering = ordering;
	}
	if (values instanceof PrimDataArray) {
		PrimDataArray data = (PrimDataArray) values;
		return FeEdition.on((((BeGrandMap) CurrentGrandMap.fluidGet()).newDataEdition(data, theKeys, theOrdering)));
	}
	else if (values instanceof PtrArray) {
		PtrArray ptr = (PtrArray) values;
		return FeEdition.on((((BeGrandMap) CurrentGrandMap.fluidGet()).newValueEdition(ptr, theKeys, theOrdering)));
	}
	return null;
/*
udanax-top.st:21443:FeEdition class methodsFor: 'creation'!
{FeEdition CLIENT} fromArray: values {PrimArray of: FeRangeElement}
	with: keys {XnRegion default: NULL}
	with: ordering {OrderSpec default: NULL}
	"Essential.  Creates an Edition mapping from a Region of keys to the values in an array. The ordering specifies the correspondance between  the keys and the indices in the array.
	If a Region is given, then it must have the same count as the array.  If no Region is given, then it is taken to be the IntegerRegion from 0  to the size of the array. If no OrderSpec is given, then it is the default ascending full ordering for that CoordinateSpace."
			
	| theKeys {XnRegion} theOrdering {OrderSpec} |
	keys == NULL
		ifTrue: [theKeys := IntegerRegion make: IntegerVar0 with: values count]
		ifFalse: [theKeys := keys].
	ordering == NULL
		ifTrue: [theOrdering := theKeys coordinateSpace getAscending]
		ifFalse: [theOrdering := ordering].
	values
		cast: PrimDataArray into: 
			[ :data |
			^FeEdition on: 
				(CurrentGrandMap fluidGet
					newDataEdition: data
					with: theKeys 
					with: theOrdering )]
		cast: PtrArray into: 
			[ :ptr | 
			^FeEdition on: 
				(CurrentGrandMap fluidGet
					newValueEdition: ptr 
					with: theKeys 
					with: theOrdering)].
	^NULL "fodder"!
*/
}
/**
 * A singleton Edition mapping from a single key to a single value.
 */
public static FeEdition fromOne(Position key, FeRangeElement value) {
	return FeEdition.on((((BeGrandMap) CurrentGrandMap.fluidGet()).newEditionWith(key, value.carrier())));
/*
udanax-top.st:21473:FeEdition class methodsFor: 'creation'!
{FeEdition CLIENT} fromOne: key {Position} with: value {FeRangeElement}
	"A singleton Edition mapping from a single key to a single value."
	
	^FeEdition on: (CurrentGrandMap fluidGet newEditionWith: key with: value carrier)!
*/
}
public static FeEdition on(BeEdition be) {
	FeEdition result;
	result = new FeEdition(be, FeLabel.fake());
	be.addFeRangeElement(result);
	return result;
/*
udanax-top.st:21478:FeEdition class methodsFor: 'creation'!
{FeEdition} on: be {BeEdition} 
	| result {FeEdition} |
	result := self create: be with: FeLabel fake.
	be addFeRangeElement: result.
	^result!
*/
}
public static FeEdition on(BeEdition be, FeLabel label) {
	FeEdition result;
	result = new FeEdition(be, label);
	be.addFeRangeElement(result);
	return result;
/*
udanax-top.st:21485:FeEdition class methodsFor: 'creation'!
{FeEdition} on: be {BeEdition} with: label {FeLabel}
	| result {FeEdition} |
	result := self create: be with: label.
	be addFeRangeElement: result.
	^result!
*/
}
/**
 * Essential.  Create a new Edition mapping from each key in the Region to a new, unique
 * PlaceHolder. The owner will have the capability to make them become something else.
 */
public static FeEdition placeHolders(XnRegion keys) {
	return FeEdition.on((((BeGrandMap) CurrentGrandMap.fluidGet()).newPlaceHolders(keys)));
/*
udanax-top.st:21492:FeEdition class methodsFor: 'creation'!
{FeEdition CLIENT} placeHolders: keys {XnRegion} 
	"Essential.  Create a new Edition mapping from each key in the Region to a new, unique PlaceHolder. The owner will have the capability to make them become something else."
	
	^FeEdition on: (CurrentGrandMap fluidGet newPlaceHolders: keys)!
*/
}
/**
 * For transcluders and works queries - only return objects which directly contain the
 * sources of the query (i.e. excludes those which only contain it transitively through
 * intermediate Editions)
 */
public static int DIRECTUCONTAINERSUONLY() {
	return 4;
/*
udanax-top.st:21499:FeEdition class methodsFor: 'constants'!
{Int32 constFn INLINE CLIENT} DIRECT.U.CONTAINERS.U.ONLY
	"For transcluders and works queries - only return objects which directly contain the sources of the query (i.e. excludes those which only contain it transitively through intermediate Editions)"
	^4!
*/
}
/**
 * For sharedWith/sharedRegion/notSharedWith - look for RangeElements contained transitively
 * within the other Edition
 */
public static int FROMUOTHERUTRANSITIVEUCONTENTS() {
	return 8;
/*
udanax-top.st:21503:FeEdition class methodsFor: 'constants'!
{Int32 constFn INLINE CLIENT} FROM.U.OTHER.U.TRANSITIVE.U.CONTENTS
	"For sharedWith/sharedRegion/notSharedWith - look for RangeElements contained transitively within the other Edition"
	^8!
*/
}
/**
 * For transcluders, and works queries - consider RangeElements contained transitively inside
 * the Edition, as well as just its immediate RangeElements
 */
public static int FROMUTRANSITIVEUCONTENTS() {
	return 2;
/*
udanax-top.st:21508:FeEdition class methodsFor: 'constants'!
{Int32 constFn INLINE CLIENT} FROM.U.TRANSITIVE.U.CONTENTS
	"For transcluders, and works queries - consider RangeElements contained transitively inside the Edition, as well as just its immediate RangeElements"
	^2!
*/
}
/**
 * Used for retrieve.  Allow the ArrayBundles in retrieve to be organized according to a
 * different ordering.
 */
public static int IGNOREUARRAYUORDERING() {
	return 2;
/*
udanax-top.st:21512:FeEdition class methodsFor: 'constants'!
{Int32 constFn INLINE CLIENT} IGNORE.U.ARRAY.U.ORDERING
	"Used for retrieve.  Allow the ArrayBundles in retrieve to be organized according to a different ordering."
	
	^2!
*/
}
/**
 * Used for retrieve.  Allow non-contiguous chunks to be grouped together on retrieve, and
 * allow the bundles to be presented in any order.
 */
public static int IGNOREUTOTALUORDERING() {
	return 1;
/*
udanax-top.st:21517:FeEdition class methodsFor: 'constants'!
{Int32 constFn INLINE CLIENT} IGNORE.U.TOTAL.U.ORDERING
	"Used for retrieve.  Allow non-contiguous chunks to be grouped together on retrieve, and allow the bundles to be presented in any order."
	
	^1!
*/
}
/**
 * For transcluders and works queries - only guarantee to return items which are currently
 * known to this server
 */
public static int LOCALUPRESENTUONLY() {
	return 1;
/*
udanax-top.st:21522:FeEdition class methodsFor: 'constants'!
{Int32 constFn INLINE CLIENT} LOCAL.U.PRESENT.U.ONLY
	"For transcluders and works queries - only guarantee to return items which are currently known to this server"
	^1!
*/
}
/**
 * For cost - omit the cost of shared material
 */
public static int OMITUSHARED() {
	return 1;
/*
udanax-top.st:21526:FeEdition class methodsFor: 'constants'!
{Int32 constFn INLINE CLIENT} OMIT.U.SHARED
	"For cost - omit the cost of shared material"
	
	^1!
*/
}
/**
 * For sharedWith/sharedRegion/notSharedWith
 */
public static int otherTransitiveContents() {
	return 2;
/*
udanax-top.st:21531:FeEdition class methodsFor: 'constants'!
{Int32 constFn INLINE CLIENT} otherTransitiveContents
	"For sharedWith/sharedRegion/notSharedWith"
	^2!
*/
}
/**
 * For cost - prorate the cost of shared material among Editions sharing it
 */
public static int PRORATEUSHARED() {
	return 2;
/*
udanax-top.st:21536:FeEdition class methodsFor: 'constants'!
{Int32 constFn INLINE CLIENT} PRORATE.U.SHARED
	"For cost - prorate the cost of shared material among Editions sharing it"
	
	^2!
*/
}
/**
 * For retrieve - ensure that each Bundle in a retrieve has a single owner
 */
public static int SEPARATEUOWNERS() {
	return 32;
/*
udanax-top.st:21541:FeEdition class methodsFor: 'constants'!
{Int32 constFn INLINE CLIENT} SEPARATE.U.OWNERS
	"For retrieve - ensure that each Bundle in a retrieve has a single owner"
	
	^32!
*/
}
/**
 * Used for version comparison.
 */
public static int thisTransitiveContents() {
	return 1;
/*
udanax-top.st:21546:FeEdition class methodsFor: 'constants'!
{Int32 constFn INLINE CLIENT} thisTransitiveContents
	"Used for version comparison."
	^1!
*/
}
/**
 * For sharedRegion, sharedWith, notSharedWith queries - look down towards transitively
 * contained material
 */
public static int TOUTRANSITIVEUCONTENTS() {
	return 2;
/*
udanax-top.st:21551:FeEdition class methodsFor: 'constants'!
{Int32 constFn INLINE CLIENT} TO.U.TRANSITIVE.U.CONTENTS
	"For sharedRegion, sharedWith, notSharedWith queries - look down towards transitively contained material"
	^2!
*/
}
/**
 * For cost - count the entire cost of shared material
 */
public static int TOTALUSHARED() {
	return 3;
/*
udanax-top.st:21555:FeEdition class methodsFor: 'constants'!
{Int32 constFn INLINE CLIENT} TOTAL.U.SHARED
	"For cost - count the entire cost of shared material"
	
	^3!
*/
}
/**
 * {Int32 constFn INLINE CLIENT} DIRECT.U.CONTAINERS.U.ONLY
 * {Int32 constFn INLINE CLIENT} FROM.U.OTHER.U.TRANSITIVE.U.CONTENTS
 * {Int32 constFn INLINE CLIENT} FROM.U.TRANSITIVE.U.CONTENTS
 * {Int32 constFn INLINE CLIENT} IGNORE.U.ARRAY.U.ORDERING
 * {Int32 constFn INLINE CLIENT} IGNORE.U.TOTAL.U.ORDERING
 * {Int32 constFn INLINE CLIENT} LOCAL.U.PRESENT.U.ONLY
 * {Int32 constFn INLINE CLIENT} OMIT.U.SHARED
 * {Int32 constFn INLINE CLIENT} PRORATE.U.SHARED
 * {Int32 constFn INLINE CLIENT} SEPARATE.U.OWNERS
 * {Int32 constFn INLINE CLIENT} TO.U.TRANSITIVE.U.CONTENTS
 * {Int32 constFn INLINE CLIENT} TOTAL.U.SHARED
 * {void CLIENT} addFillRangeDetector: detector {PrFillRangeDetector}
 * {XuRegion CLIENT} canMakeRangeIdentical: newIdentities {FeEdition} with: positions
 * {XuRegion default: NULL}
 * {FeEdition CLIENT} combine: other {FeEdition}
 * {CoordinateSpace CLIENT} coordinateSpace
 * {FeEdition CLIENT} copy: positions {XuRegion}
 * {IntegerVar CLIENT} cost: method {Int32}
 * {IntegerVar CLIENT} count
 * {XuRegion CLIENT} domain
 * {void CLIENT} endorse: endorsements {CrossRegion}
 * {CrossRegion CLIENT} endorsements
 * {FeRangeElement CLIENT} get: position {Position}
 * {BooleanVar CLIENT} hasPosition: position {Position}
 * {BooleanVar CLIENT} isEmpty
 * {BooleanVar CLIENT} isFinite
 * {BooleanVar CLIENT} isRangeIdentical: other {FeEdition}
 * {BooleanVar CLIENT} isRangeIdentical: other {FeEdition} with: region {XuRegion default:
 * NULL}
 * {FeEdition CLIENT} makeRangeIdentical: newIdentities {FeEdition} with: positions {XuRegion
 * default: NULL}
 * {Mapping CLIENT} mapSharedOnto: other {FeEdition}
 * {Mapping CLIENT} mapSharedTo: other {FeEdition}
 * {FeEdition CLIENT} notSharedWith: other {FeEdition} with: flags {Int32 default: Int32Zero}
 * {XuRegion CLIENT} positionsLabelled: label {FeLabel}
 * {XuRegion CLIENT} positionsOf: value {FeRangeElement}
 * {IDRegion CLIENT} rangeOwners: positions {XuRegion default: NULL}
 * {FeEdition CLIENT} rangeTranscluders: positions {XuRegion default: NULL} with:
 * directFilter {Filter default: NULL} with: indirectFilter {Filter default: NULL} with:
 * flags {Int32 default: Int32Zero} with: otherTrail {FeEdition default: NULL}
 * {FeEdition CLIENT} rangeWorks: positions {XuRegion default: NULL} with: filter {Filter
 * default: NULL} with: flags {Int32 default: Int32Zero} with: otherTrail {FeEdition default:
 * NULL}
 * {FeEdition CLIENT} rebind: position {Position} with: edition {FeEdition}
 * {void CLIENT} removeFillRangeDetector: detector {PrFillRangeDetector}
 * {FeEdition CLIENT} replace: other {FeEdition}
 * {void CLIENT} retract: endorsements {CrossRegion}
 * {(Stepper of: Bundle) CLIENT} retrieve: positions {XuRegion default: NULL} with: order
 * {OrderSpec default: NULL} with: flags {Int32 default: Int32Zero}
 * {FeEdition CLIENT} setRangeOwners: newOwner {ID} with: positions {XuRegion default: NULL}
 * {XuRegion CLIENT} sharedRegion: other {FeEdition} with: flags {Int32 default: Int32Zero}
 * {FeEdition CLIENT} sharedWith: other {FeEdition} with: flags {Int32 default: Int32Zero}
 * {TableStepper CLIENT of: FeRangeElement} stepper: region {XuRegion default: NULL} with:
 * order {OrderSpec default: NULL}
 * {FeRangeElement CLIENT} theOne
 * {FeEdition CLIENT} transformedBy: mapping {Mapping}
 * {CrossRegion CLIENT} visibleEndorsements
 * {FeEdition CLIENT} with: position {Position} with: value {FeRangeElement}
 * {FeEdition CLIENT} withAll: positions {XuRegion} with: value {FeRangeElement}
 * {FeEdition CLIENT} without: position {Position}
 * {FeEdition CLIENT} withoutAll: positions {XuRegion}
 */
public static void infostProtocol() {
/*
udanax-top.st:21562:FeEdition class methodsFor: 'smalltalk: system'!
info.stProtocol
"{Int32 constFn INLINE CLIENT} DIRECT.U.CONTAINERS.U.ONLY
{Int32 constFn INLINE CLIENT} FROM.U.OTHER.U.TRANSITIVE.U.CONTENTS
{Int32 constFn INLINE CLIENT} FROM.U.TRANSITIVE.U.CONTENTS
{Int32 constFn INLINE CLIENT} IGNORE.U.ARRAY.U.ORDERING
{Int32 constFn INLINE CLIENT} IGNORE.U.TOTAL.U.ORDERING
{Int32 constFn INLINE CLIENT} LOCAL.U.PRESENT.U.ONLY
{Int32 constFn INLINE CLIENT} OMIT.U.SHARED
{Int32 constFn INLINE CLIENT} PRORATE.U.SHARED
{Int32 constFn INLINE CLIENT} SEPARATE.U.OWNERS
{Int32 constFn INLINE CLIENT} TO.U.TRANSITIVE.U.CONTENTS
{Int32 constFn INLINE CLIENT} TOTAL.U.SHARED
{void CLIENT} addFillRangeDetector: detector {PrFillRangeDetector}
{XuRegion CLIENT} canMakeRangeIdentical: newIdentities {FeEdition} with: positions {XuRegion default: NULL}
{FeEdition CLIENT} combine: other {FeEdition}
{CoordinateSpace CLIENT} coordinateSpace
{FeEdition CLIENT} copy: positions {XuRegion}
{IntegerVar CLIENT} cost: method {Int32}
{IntegerVar CLIENT} count
{XuRegion CLIENT} domain
{void CLIENT} endorse: endorsements {CrossRegion}
{CrossRegion CLIENT} endorsements
{FeRangeElement CLIENT} get: position {Position}
{BooleanVar CLIENT} hasPosition: position {Position}
{BooleanVar CLIENT} isEmpty
{BooleanVar CLIENT} isFinite
{BooleanVar CLIENT} isRangeIdentical: other {FeEdition}
{BooleanVar CLIENT} isRangeIdentical: other {FeEdition} with: region {XuRegion default: NULL}
{FeEdition CLIENT} makeRangeIdentical: newIdentities {FeEdition} with: positions {XuRegion default: NULL}
{Mapping CLIENT} mapSharedOnto: other {FeEdition}
{Mapping CLIENT} mapSharedTo: other {FeEdition}
{FeEdition CLIENT} notSharedWith: other {FeEdition} with: flags {Int32 default: Int32Zero}
{XuRegion CLIENT} positionsLabelled: label {FeLabel}
{XuRegion CLIENT} positionsOf: value {FeRangeElement}
{IDRegion CLIENT} rangeOwners: positions {XuRegion default: NULL}
{FeEdition CLIENT} rangeTranscluders: positions {XuRegion default: NULL} with: directFilter {Filter default: NULL} with: indirectFilter {Filter default: NULL} with: flags {Int32 default: Int32Zero} with: otherTrail {FeEdition default: NULL}
{FeEdition CLIENT} rangeWorks: positions {XuRegion default: NULL} with: filter {Filter default: NULL} with: flags {Int32 default: Int32Zero} with: otherTrail {FeEdition default: NULL}
{FeEdition CLIENT} rebind: position {Position} with: edition {FeEdition}
{void CLIENT} removeFillRangeDetector: detector {PrFillRangeDetector}
{FeEdition CLIENT} replace: other {FeEdition}
{void CLIENT} retract: endorsements {CrossRegion}
{(Stepper of: Bundle) CLIENT} retrieve: positions {XuRegion default: NULL} with: order {OrderSpec default: NULL} with: flags {Int32 default: Int32Zero}
{FeEdition CLIENT} setRangeOwners: newOwner {ID} with: positions {XuRegion default: NULL}
{XuRegion CLIENT} sharedRegion: other {FeEdition} with: flags {Int32 default: Int32Zero}
{FeEdition CLIENT} sharedWith: other {FeEdition} with: flags {Int32 default: Int32Zero}
{TableStepper CLIENT of: FeRangeElement} stepper: region {XuRegion default: NULL} with: order {OrderSpec default: NULL}
{FeRangeElement CLIENT} theOne
{FeEdition CLIENT} transformedBy: mapping {Mapping}
{CrossRegion CLIENT} visibleEndorsements
{FeEdition CLIENT} with: position {Position} with: value {FeRangeElement}
{FeEdition CLIENT} withAll: positions {XuRegion} with: value {FeRangeElement}
{FeEdition CLIENT} without: position {Position}
{FeEdition CLIENT} withoutAll: positions {XuRegion}
"!
*/
}
public FeEdition() {
/*

Generated during transformation
*/
}
public FeEdition(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
