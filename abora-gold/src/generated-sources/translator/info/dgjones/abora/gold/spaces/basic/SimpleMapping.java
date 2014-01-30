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
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.EmptyMapping;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.SimpleMapping;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class SimpleMapping extends Mapping {

	protected XnRegion myRegion;
	protected Mapping myMapping;
/*
udanax-top.st:30292:
Mapping subclass: #SimpleMapping
	instanceVariableNames: '
		myRegion {XnRegion}
		myMapping {Mapping}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Basic'!
*/
/*
udanax-top.st:30298:
(SimpleMapping getOrMakeCxxClassDescription)
	friends:
'/- friends for class SimpleMapping -/
friend SPTR(Mapping) restrictTo (XnRegion*, Mapping*);
';
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
/*
udanax-top.st:30463:
SimpleMapping class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:30466:
(SimpleMapping getOrMakeCxxClassDescription)
	friends:
'/- friends for class SimpleMapping -/
friend SPTR(Mapping) restrictTo (XnRegion*, Mapping*);
';
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SimpleMapping.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public Mapping appliedAfter(Dsp dsp) {
	return SimpleMapping.restrictTo((dsp.inverseOfAll(myRegion)), (myMapping.appliedAfter(dsp)));
/*
udanax-top.st:30307:SimpleMapping methodsFor: 'accessing'!
{Mapping} appliedAfter: dsp {Dsp}
	^SimpleMapping restrictTo: (dsp inverseOfAll: myRegion)
		with: (myMapping appliedAfter: dsp)!
*/
}
public CoordinateSpace coordinateSpace() {
	return myRegion.coordinateSpace();
/*
udanax-top.st:30312:SimpleMapping methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^myRegion coordinateSpace!
*/
}
public XnRegion domain() {
	return myRegion;
/*
udanax-top.st:30316:SimpleMapping methodsFor: 'accessing'!
{XnRegion} domain
	^ myRegion!
*/
}
public Dsp fetchDsp() {
	return myMapping.fetchDsp();
/*
udanax-top.st:30319:SimpleMapping methodsFor: 'accessing'!
{Dsp | NULL} fetchDsp
	^ myMapping fetchDsp!
*/
}
public boolean isComplete() {
	return myMapping.isComplete();
/*
udanax-top.st:30322:SimpleMapping methodsFor: 'accessing'!
{BooleanVar} isComplete
	^myMapping isComplete!
*/
}
public boolean isIdentity() {
	return false;
/*
udanax-top.st:30326:SimpleMapping methodsFor: 'accessing'!
{BooleanVar} isIdentity
	^false!
*/
}
public Mapping preCompose(Dsp dsp) {
	return SimpleMapping.restrictTo(myRegion, (myMapping.preCompose(dsp)));
/*
udanax-top.st:30330:SimpleMapping methodsFor: 'accessing'!
{Mapping} preCompose: dsp {Dsp}
	^SimpleMapping restrictTo: myRegion with: (myMapping preCompose: dsp)!
*/
}
public XnRegion range() {
	return myMapping.ofAll(myRegion);
/*
udanax-top.st:30334:SimpleMapping methodsFor: 'accessing'!
{XnRegion} range
	^ myMapping ofAll: myRegion!
*/
}
public CoordinateSpace rangeSpace() {
	return myMapping.rangeSpace();
/*
udanax-top.st:30337:SimpleMapping methodsFor: 'accessing'!
{CoordinateSpace} rangeSpace
	
	^ myMapping rangeSpace!
*/
}
public ImmuSet simpleMappings() {
	return ImmuSet.make().with(this);
/*
udanax-top.st:30341:SimpleMapping methodsFor: 'accessing'!
{ImmuSet of: Mapping} simpleMappings
	^ ImmuSet make with: self!
*/
}
public ImmuSet simpleRegionMappings() {
	if (myMapping.domain().isSimple()) {
		return ImmuSet.make().with(myMapping);
	}
	else {
		MuSet simpleMappings;
		simpleMappings = MuSet.make();
		Stepper stomper = myMapping.domain().simpleRegions();
		for (; stomper.hasValue(); stomper.step()) {
			XnRegion simpleRegion = (XnRegion) stomper.fetch();
			if (simpleRegion == null) {
				continue ;
			}
			simpleMappings.store((myMapping.restrict(simpleRegion)));
		}
		stomper.destroy();
		return ImmuSet.makeMuSet(simpleMappings);
	}
/*
udanax-top.st:30345:SimpleMapping methodsFor: 'accessing'!
{ImmuSet of: Mapping} simpleRegionMappings
	myMapping domain isSimple 
		ifTrue:
			[^ImmuSet make with: myMapping]
		ifFalse:
			[ | simpleMappings {MuSet} | 
			simpleMappings _ MuSet make.
			myMapping domain simpleRegions forEach: [:simpleRegion {XnRegion} |
				simpleMappings store: (myMapping restrict: simpleRegion)].
			^ImmuSet make.MuSet: simpleMappings]!
*/
}
public Mapping transformedBy(Dsp dsp) {
	return SimpleMapping.restrictTo(myRegion, (myMapping.transformedBy(dsp)));
/*
udanax-top.st:30357:SimpleMapping methodsFor: 'accessing'!
{Mapping} transformedBy: dsp {Dsp}
	^SimpleMapping restrictTo: myRegion
		with: (myMapping transformedBy: dsp)!
*/
}
public Position inverseOf(Position pos) {
	Position result;
	result = myMapping.inverseOf(pos);
	if (myRegion.hasMember(result)) {
		return result;
	}
	else {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_RANGE);
	}
/*
udanax-top.st:30364:SimpleMapping methodsFor: 'transforming'!
{Position} inverseOf: pos {Position}
	| result {Position} |
	result _ myMapping inverseOf: pos.
	(myRegion hasMember: result)
		ifTrue: [^result]
		ifFalse: [Heaper BLAST: #NotInRange].
	^NULL "fodder"!
*/
}
public XnRegion inverseOfAll(XnRegion reg) {
	return (myMapping.inverseOfAll(reg)).intersect(myRegion);
/*
udanax-top.st:30373:SimpleMapping methodsFor: 'transforming'!
{XnRegion} inverseOfAll: reg {XnRegion}
	^(myMapping inverseOfAll: reg) intersect: myRegion!
*/
}
public Position of(Position pos) {
	if (domain().hasMember(pos)) {
		return myMapping.of(pos);
	}
	else {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_DOMAIN);
	}
/*
udanax-top.st:30377:SimpleMapping methodsFor: 'transforming'!
{Position} of: pos {Position}
	(self domain hasMember: pos)
		ifTrue: [^ myMapping of: pos]
		ifFalse: [Heaper BLAST: #NotInDomain].
	^NULL "fodder"!
*/
}
public XnRegion ofAll(XnRegion reg) {
	return myMapping.ofAll((domain().intersect(reg)));
/*
udanax-top.st:30384:SimpleMapping methodsFor: 'transforming'!
{XnRegion} ofAll: reg {XnRegion}
	^myMapping ofAll: (self domain intersect: reg)!
*/
}
public Mapping inverse() {
	return myMapping.inverse().restrictRange(myRegion);
/*
udanax-top.st:30390:SimpleMapping methodsFor: 'operations'!
{Mapping} inverse
	^myMapping inverse restrictRange: myRegion!
*/
}
public Mapping restrict(XnRegion region) {
	return SimpleMapping.restrictTo((myRegion.intersect(region)), myMapping);
/*
udanax-top.st:30394:SimpleMapping methodsFor: 'operations'!
{Mapping} restrict: region {XnRegion}
	
	^SimpleMapping restrictTo: (myRegion intersect: region) with: myMapping!
*/
}
public Mapping restrictRange(XnRegion region) {
	return SimpleMapping.restrictTo(myRegion, (myMapping.restrictRange(region)));
/*
udanax-top.st:30398:SimpleMapping methodsFor: 'operations'!
{Mapping} restrictRange: region {XnRegion}
	
	^SimpleMapping restrictTo: myRegion with: (myMapping restrictRange: region)!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(myMapping);
	oo.print(" on ");
	oo.print(myRegion);
/*
udanax-top.st:30404:SimpleMapping methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << myMapping << ' on ' << myRegion!
*/
}
public SimpleMapping(XnRegion region, Mapping mapping) {
	super();
	myRegion = region;
	myMapping = mapping;
/*
udanax-top.st:30410:SimpleMapping methodsFor: 'private: private creation'!
create: region {XnRegion} with: mapping {Mapping}
	super create.
	myRegion _ region.
	myMapping _ mapping.!
*/
}
public int actualHashForEqual() {
	return myRegion.hashForEqual() + myMapping.hashForEqual();
/*
udanax-top.st:30417:SimpleMapping methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^myRegion hashForEqual + myMapping hashForEqual!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof SimpleMapping) {
		SimpleMapping sm = (SimpleMapping) other;
		return (sm.domain().isEqual(myRegion)) && (sm.mapping().isEqual(myMapping));
	}
	else {
		return false;
	}
/*
udanax-top.st:30420:SimpleMapping methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: SimpleMapping into: [:sm |
			^(sm domain isEqual: myRegion)
			 and: [sm mapping isEqual: myMapping]]
		others: [^false].
	^false "fodder"!
*/
}
public Mapping mapping() {
	return myMapping;
/*
udanax-top.st:30431:SimpleMapping methodsFor: 'private: private'!
{Mapping} mapping
	^myMapping!
*/
}
public Mapping fetchCombine(Mapping mapping) {
	if (mapping.isEqual(myMapping)) {
		return mapping;
	}
	if (mapping instanceof SimpleMapping) {
		SimpleMapping other = (SimpleMapping) mapping;
		Mapping both;
		if (other.mapping().isEqual(myMapping)) {
			return SimpleMapping.restrictTo((other.domain().unionWith(myRegion)), myMapping);
		}
		else {
			if ((other.domain().isEqual(myRegion)) && ((both = myMapping.fetchCombine(other.mapping())) != null)) {
				return SimpleMapping.restrictTo(myRegion, both);
			}
		}
	}
	return null;
/*
udanax-top.st:30436:SimpleMapping methodsFor: 'protected'!
{Mapping} fetchCombine: mapping {Mapping}
	(mapping isEqual: myMapping) ifTrue: [^mapping].
	mapping
		cast: SimpleMapping into: [:other |
			| both {Mapping} |
			(other mapping isEqual: myMapping) ifTrue:
				[^SimpleMapping restrictTo: (other domain unionWith: myRegion) with: myMapping]
			ifFalse: [((other domain isEqual: myRegion)
					and: [(both _ myMapping fetchCombine: other mapping) ~~ NULL]) ifTrue:
				[^SimpleMapping restrictTo: myRegion with: both]]]
		others: [].
	^NULL!
*/
}
public SimpleMapping(Rcvr receiver) {
	super(receiver);
	myRegion = (XnRegion) receiver.receiveHeaper();
	myMapping = (Mapping) receiver.receiveHeaper();
/*
udanax-top.st:30452:SimpleMapping methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myRegion _ receiver receiveHeaper.
	myMapping _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myRegion);
	xmtr.sendHeaper(myMapping);
/*
udanax-top.st:30457:SimpleMapping methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myRegion.
	xmtr sendHeaper: myMapping.!
*/
}
public static Mapping restrictTo(XnRegion region, Mapping mapping) {
	if (region.isEmpty()) {
		return EmptyMapping.make(mapping.domainSpace(), mapping.rangeSpace());
	}
	else {
		return new SimpleMapping(region, mapping);
	}
/*
udanax-top.st:30475:SimpleMapping class methodsFor: 'pseudo constructors'!
{Mapping} restrictTo: region {XnRegion} with: mapping {Mapping}
	region isEmpty
		ifTrue: [^EmptyMapping make: mapping domainSpace
								 with: mapping rangeSpace]
		ifFalse: [^SimpleMapping create: region with: mapping]!
*/
}
public SimpleMapping() {
/*

Generated during transformation
*/
}
}
