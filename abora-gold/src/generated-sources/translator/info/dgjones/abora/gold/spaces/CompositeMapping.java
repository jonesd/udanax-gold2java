/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces;

import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.SetAccumulator;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.CompositeMapping;
import info.dgjones.abora.gold.spaces.EmptyMapping;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class CompositeMapping extends Mapping {

	protected CoordinateSpace myCS;
	protected CoordinateSpace myRS;
	protected ImmuSet myMappings;
/*
udanax-top.st:28588:
Mapping subclass: #CompositeMapping
	instanceVariableNames: '
		myCS {CoordinateSpace}
		myRS {CoordinateSpace}
		myMappings {ImmuSet of: Mapping}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces'!
*/
/*
udanax-top.st:28595:
(CompositeMapping getOrMakeCxxClassDescription)
	friends:
'/- friends for class CompositeMapping -/
friend SPTR(Mapping) mapping(Mapping*, Mapping*);
friend SPTR(Mapping)  privateMakeMapping (CoordinateSpace *, CoordinateSpace *, ImmuSet OF1(Mapping) *);';
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
/*
udanax-top.st:28831:
CompositeMapping class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:28834:
(CompositeMapping getOrMakeCxxClassDescription)
	friends:
'/- friends for class CompositeMapping -/
friend SPTR(Mapping) mapping(Mapping*, Mapping*);
friend SPTR(Mapping)  privateMakeMapping (CoordinateSpace *, CoordinateSpace *, ImmuSet OF1(Mapping) *);';
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CompositeMapping.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public Mapping appliedAfter(Dsp dsp) {
	SetAccumulator result;
	result = SetAccumulator.make();
	Stepper stomper = myMappings.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Mapping each = (Mapping) stomper.fetch();
		if (each == null) {
			continue ;
		}
		result.step((each.appliedAfter(dsp)));
	}
	stomper.destroy();
	return CompositeMapping.privateMakeMapping(coordinateSpace(), rangeSpace(), ((ImmuSet) result.value()));
/*
udanax-top.st:28604:CompositeMapping methodsFor: 'operations'!
{Mapping} appliedAfter: dsp {Dsp}
	| result {SetAccumulator of: Mapping} |
	result _ SetAccumulator make.
	myMappings stepper forEach: [ :each {Mapping} |
		result step: (each appliedAfter: dsp)].
	^CompositeMapping
		privateMakeMapping: self coordinateSpace
		with: self rangeSpace
		with: (result value cast: ImmuSet)!
*/
}
public Mapping inverse() {
	Mapping result;
	Ravi.thingToDo();
	/* can this be done more efficiently by taking advantage of invariants? */
	result = Mapping.makeCoordinateSpace(rangeSpace(), domainSpace());
	Stepper stomper = myMappings.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Mapping sub = (Mapping) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		result = result.combine(sub.inverse());
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:28615:CompositeMapping methodsFor: 'operations'!
{Mapping} inverse
	| result {Mapping} |
	Ravi thingToDo. "can this be done more efficiently by taking advantage of invariants?"
	result := Mapping make.CoordinateSpace: self rangeSpace with: self domainSpace.
	myMappings stepper forEach: [ :sub {Mapping} |
		result := result combine: sub inverse].
	^result!
*/
}
public Mapping preCompose(Dsp dsp) {
	SetAccumulator result;
	result = SetAccumulator.make();
	Stepper stomper = myMappings.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Mapping each = (Mapping) stomper.fetch();
		if (each == null) {
			continue ;
		}
		result.step((each.preCompose(dsp)));
	}
	stomper.destroy();
	return CompositeMapping.privateMakeMapping(coordinateSpace(), rangeSpace(), ((ImmuSet) result.value()));
/*
udanax-top.st:28624:CompositeMapping methodsFor: 'operations'!
{Mapping} preCompose: dsp {Dsp}
	| result {SetAccumulator of: Mapping} |
	result _ SetAccumulator make.
	myMappings stepper forEach: [ :each {Mapping} |
		result step: (each preCompose: dsp)].
	^CompositeMapping
		privateMakeMapping: self coordinateSpace
		with: self rangeSpace
		with: (result value cast: ImmuSet)!
*/
}
public Mapping restrict(XnRegion region) {
	MuSet result;
	result = MuSet.make();
	Stepper stomper = myMappings.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Mapping each = (Mapping) stomper.fetch();
		if (each == null) {
			continue ;
		}
		Mapping restricted;
		restricted = each.restrict(region);
		if ( ! (restricted.domain().isEmpty())) {
			result.store(restricted);
		}
	}
	stomper.destroy();
	return CompositeMapping.privateMakeMapping(coordinateSpace(), rangeSpace(), result.asImmuSet());
/*
udanax-top.st:28635:CompositeMapping methodsFor: 'operations'!
{Mapping} restrict: region {XnRegion}
	| result {MuSet of: Mapping} |
	result _ MuSet make.
	myMappings stepper forEach: [ :each {Mapping} | 
		| restricted {Mapping} |
		restricted _ each restrict: region.
		restricted domain isEmpty ifFalse:
			[result store: restricted]].
	^CompositeMapping
		privateMakeMapping: self coordinateSpace
		with: self rangeSpace
		with: result asImmuSet!
*/
}
public Mapping restrictRange(XnRegion region) {
	MuSet result;
	result = MuSet.make();
	Stepper stomper = myMappings.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Mapping each = (Mapping) stomper.fetch();
		if (each == null) {
			continue ;
		}
		Mapping restricted;
		restricted = each.restrictRange(region);
		if ( ! (restricted.domain().isEmpty())) {
			result.store(restricted);
		}
	}
	stomper.destroy();
	return CompositeMapping.privateMakeMapping(coordinateSpace(), rangeSpace(), result.asImmuSet());
/*
udanax-top.st:28649:CompositeMapping methodsFor: 'operations'!
{Mapping} restrictRange: region {XnRegion}
	| result {MuSet of: Mapping} |
	result _ MuSet make.
	myMappings stepper forEach: [ :each {Mapping} | 
		| restricted {Mapping} |
		restricted _ each restrictRange: region.
		restricted domain isEmpty ifFalse:
			[result store: restricted]].
	^CompositeMapping
		privateMakeMapping: self coordinateSpace
		with: self rangeSpace
		with: result asImmuSet!
*/
}
public Mapping transformedBy(Dsp dsp) {
	SetAccumulator result;
	result = SetAccumulator.make();
	Stepper stomper = myMappings.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Mapping each = (Mapping) stomper.fetch();
		if (each == null) {
			continue ;
		}
		result.step((each.transformedBy(dsp)));
	}
	stomper.destroy();
	return CompositeMapping.privateMakeMapping(coordinateSpace(), rangeSpace(), ((ImmuSet) result.value()));
/*
udanax-top.st:28663:CompositeMapping methodsFor: 'operations'!
{Mapping} transformedBy: dsp {Dsp}
	| result {SetAccumulator of: Mapping} |
	result _ SetAccumulator make.
	myMappings stepper forEach: [ :each {Mapping} |
		result step: (each transformedBy: dsp)].
	^CompositeMapping
		privateMakeMapping: self coordinateSpace
		with: self rangeSpace
		with: (result value cast: ImmuSet)!
*/
}
public CoordinateSpace coordinateSpace() {
	return myCS;
/*
udanax-top.st:28676:CompositeMapping methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^myCS!
*/
}
public XnRegion domain() {
	XnRegion result;
	result = coordinateSpace().emptyRegion();
	Stepper stomper = myMappings.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Mapping each = (Mapping) stomper.fetch();
		if (each == null) {
			continue ;
		}
		result = result.unionWith(each.domain());
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:28680:CompositeMapping methodsFor: 'accessing'!
{XnRegion} domain
	
	| result {XnRegion} |
	result _ self coordinateSpace emptyRegion.
	myMappings stepper forEach: [ :each {Mapping} |
		result _ result unionWith: each domain].
	^result!
*/
}
public Dsp fetchDsp() {
	return null;
/*
udanax-top.st:28688:CompositeMapping methodsFor: 'accessing'!
{Dsp | NULL} fetchDsp
	^NULL!
*/
}
public boolean isComplete() {
	return false;
/*
udanax-top.st:28691:CompositeMapping methodsFor: 'accessing'!
{BooleanVar} isComplete
	^false "blast?"!
*/
}
public boolean isIdentity() {
	return false;
/*
udanax-top.st:28695:CompositeMapping methodsFor: 'accessing'!
{BooleanVar} isIdentity
	^false!
*/
}
public XnRegion range() {
	XnRegion result;
	result = rangeSpace().emptyRegion();
	Stepper stomper = myMappings.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Mapping each = (Mapping) stomper.fetch();
		if (each == null) {
			continue ;
		}
		result = result.unionWith(each.range());
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:28699:CompositeMapping methodsFor: 'accessing'!
{XnRegion} range
	| result {XnRegion} |
	result _ self rangeSpace emptyRegion.
	myMappings stepper forEach: [ :each {Mapping} |
		result _ result unionWith: each range].
	^result!
*/
}
public CoordinateSpace rangeSpace() {
	return myRS;
/*
udanax-top.st:28706:CompositeMapping methodsFor: 'accessing'!
{CoordinateSpace} rangeSpace
	^myRS!
*/
}
public ImmuSet simpleMappings() {
	return myMappings;
/*
udanax-top.st:28710:CompositeMapping methodsFor: 'accessing'!
{ImmuSet of: Mapping} simpleMappings
	^myMappings!
*/
}
public ImmuSet simpleRegionMappings() {
	MuSet simpleMappings;
	Mapping eachSimple;
	simpleMappings = MuSet.make();
	Stepper stomper = myMappings.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Mapping each = (Mapping) stomper.fetch();
		if (each == null) {
			continue ;
		}
		if (each.domain().isSimple()) {
			simpleMappings.store(each);
		}
		else {
			Stepper stomper2 = each.domain().simpleRegions();
			for (; stomper2.hasValue(); stomper2.step()) {
				XnRegion simpleRegion = (XnRegion) stomper2.fetch();
				if (simpleRegion == null) {
					continue ;
				}
				eachSimple = each.restrict(simpleRegion);
				simpleMappings.store(eachSimple);
			}
			stomper2.destroy();
		}
	}
	stomper.destroy();
	return (ImmuSet.makeMuSet(simpleMappings));
/*
udanax-top.st:28713:CompositeMapping methodsFor: 'accessing'!
{ImmuSet of: Mapping} simpleRegionMappings
	| simpleMappings {MuSet of: Mapping} eachSimple {Mapping} |
	
	simpleMappings _ MuSet make.
	myMappings stepper forEach: [ :each {Mapping} | 
		each domain isSimple 
			ifTrue:
				[simpleMappings store: each]
			ifFalse:
				[each domain simpleRegions forEach: [:simpleRegion {XnRegion} |
					eachSimple _ each restrict: simpleRegion.
					simpleMappings store: eachSimple]]].
	^(ImmuSet make.MuSet: simpleMappings)!
*/
}
public Position inverseOf(Position pos) {
	Position result;
	result = null;
	Stepper stomper = myMappings.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Mapping each = (Mapping) stomper.fetch();
		if (each == null) {
			continue ;
		}
		if (each.range().hasMember(pos)) {
			if (result == null) {
				result = each.inverseOf(pos);
			}
			else {
				throw new AboraRuntimeException(AboraRuntimeException.MULTIPLE_PRE_IMAGES);
			}
		}
	}
	stomper.destroy();
	if (result == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_RANGE);
	}
	return result;
/*
udanax-top.st:28730:CompositeMapping methodsFor: 'transforming'!
{Position} inverseOf: pos {Position}
	| result {Position} |
	result _ NULL.
	myMappings stepper forEach: [ :each {Mapping} |
		(each range hasMember: pos) ifTrue:
			[result == NULL
				ifTrue: [result _ each inverseOf: pos]
				ifFalse: [Heaper BLAST: #MultiplePreImages]]].
	result == NULL
		ifTrue: [Heaper BLAST: #NotInRange].
	^result!
*/
}
public XnRegion inverseOfAll(XnRegion reg) {
	XnRegion result;
	result = coordinateSpace().emptyRegion();
	Stepper stomper = myMappings.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Mapping each = (Mapping) stomper.fetch();
		if (each == null) {
			continue ;
		}
		result = result.unionWith((each.inverseOfAll(reg)));
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:28743:CompositeMapping methodsFor: 'transforming'!
{XnRegion} inverseOfAll: reg {XnRegion}
	| result {XnRegion} |
	result _ self coordinateSpace emptyRegion.
	myMappings stepper forEach: [ :each {Mapping} |
		result _ result unionWith: (each inverseOfAll: reg)].
	^result!
*/
}
public Position of(Position pos) {
	Position result;
	result = null;
	Stepper stomper = myMappings.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Mapping each = (Mapping) stomper.fetch();
		if (each == null) {
			continue ;
		}
		if (each.domain().hasMember(pos)) {
			if (result == null) {
				result = each.of(pos);
			}
			else {
				throw new AboraRuntimeException(AboraRuntimeException.MULTIPLE_IMAGES);
			}
		}
	}
	stomper.destroy();
	if (result == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_DOMAIN);
	}
	return result;
/*
udanax-top.st:28751:CompositeMapping methodsFor: 'transforming'!
{Position} of: pos {Position}
	| result {Position} |
	result _ NULL.
	myMappings stepper forEach: [ :each {Mapping} |
		(each domain hasMember: pos) ifTrue:
			[result == NULL
				ifTrue: [result _ each of: pos]
				ifFalse: [Heaper BLAST: #MultipleImages]]].
	result == NULL
		ifTrue: [Heaper BLAST: #NotInDomain].
	^result!
*/
}
public XnRegion ofAll(XnRegion reg) {
	XnRegion result;
	result = rangeSpace().emptyRegion();
	Stepper stomper = myMappings.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Mapping each = (Mapping) stomper.fetch();
		if (each == null) {
			continue ;
		}
		result = result.unionWith((each.ofAll(reg)));
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:28764:CompositeMapping methodsFor: 'transforming'!
{XnRegion} ofAll: reg {XnRegion}
	| result {XnRegion} |
	result _ self rangeSpace emptyRegion.
	myMappings stepper forEach: [ :each {Mapping} |
		result _ result unionWith: (each ofAll: reg)].
	^result!
*/
}
public void printOn(PrintWriter stream) {
	stream.print(getAboraClass().name());
	myMappings.printOnWithSimpleSyntax(stream, "(", ", ", ")");
/*
udanax-top.st:28774:CompositeMapping methodsFor: 'printing'!
{void} printOn: stream {ostream reference}
	stream << self getCategory name.
	myMappings printOnWithSimpleSyntax: stream with: '(' with: ', ' with: ')'!
*/
}
public CompositeMapping(CoordinateSpace cs, CoordinateSpace rs, ImmuSet mappings) {
	super();
	myCS = cs;
	myRS = rs;
	myMappings = mappings;
/*
udanax-top.st:28781:CompositeMapping methodsFor: 'private: private creation'!
create: cs {CoordinateSpace} with: rs {CoordinateSpace} with: mappings {ImmuSet of: Mapping}
	super create.
	myCS _ cs.
	myRS _ rs.
	myMappings _ mappings!
*/
}
public int actualHashForEqual() {
	return HashHelper.hashForEqual(this.getClass()) ^ myMappings.hashForEqual();
/*
udanax-top.st:28789:CompositeMapping methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^#cat.U.CompositeMapping hashForEqual bitXor: myMappings hashForEqual!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof CompositeMapping) {
		CompositeMapping cm = (CompositeMapping) other;
		return cm.simpleMappings().isEqual(myMappings);
	}
	else {
		return false;
	}
/*
udanax-top.st:28792:CompositeMapping methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: CompositeMapping into: [:cm |
			^cm simpleMappings isEqual: myMappings]
		others: [^false].
	^false "fodder"!
*/
}
public Mapping fetchCombine(Mapping mapping) {
	if (mapping instanceof EmptyMapping) {
		return this;
	}
	else {
		MuSet result;
		result = myMappings.asMuSet();
		if (mapping instanceof CompositeMapping) {
			Stepper stomper = mapping.simpleMappings().stepper();
			for (; stomper.hasValue(); stomper.step()) {
				Mapping each = (Mapping) stomper.fetch();
				if (each == null) {
					continue ;
				}
				CompositeMapping.storeMapping(each, result);
			}
			stomper.destroy();
		}
		else {
			CompositeMapping.storeMapping(mapping, result);
		}
		return CompositeMapping.privateMakeMapping(myCS, myRS, result.asImmuSet());
	}
/*
udanax-top.st:28802:CompositeMapping methodsFor: 'protected: protected'!
{Mapping} fetchCombine: mapping {Mapping}
	(mapping isKindOf: EmptyMapping)
		ifTrue: [  ^ self ]
		ifFalse:
			[| result {MuSet of: Mapping} |
			result _ myMappings asMuSet.
			(mapping isKindOf: CompositeMapping) ifTrue:
				[mapping simpleMappings stepper forEach: [ :each {Mapping} |
					CompositeMapping storeMapping: each with: result]]
				ifFalse:
					[CompositeMapping storeMapping: mapping with: result].
			^CompositeMapping privateMakeMapping: myCS with: myRS with: result asImmuSet]!
*/
}
public CompositeMapping(Rcvr receiver) {
	super(receiver);
	myCS = (CoordinateSpace) receiver.receiveHeaper();
	myRS = (CoordinateSpace) receiver.receiveHeaper();
	myMappings = (ImmuSet) receiver.receiveHeaper();
/*
udanax-top.st:28818:CompositeMapping methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCS _ receiver receiveHeaper.
	myRS _ receiver receiveHeaper.
	myMappings _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myCS);
	xmtr.sendHeaper(myRS);
	xmtr.sendHeaper(myMappings);
/*
udanax-top.st:28824:CompositeMapping methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCS.
	xmtr sendHeaper: myRS.
	xmtr sendHeaper: myMappings.!
*/
}
public static Mapping privateMakeMapping(CoordinateSpace cs, CoordinateSpace rs, ImmuSet mappings) {
	if (mappings.isEmpty()) {
		return EmptyMapping.make(cs, rs);
	}
	else {
		if (mappings.count() == 1) {
			return (Mapping) mappings.theOne();
		}
		else {
			return new CompositeMapping(cs, rs, mappings);
		}
	}
/*
udanax-top.st:28843:CompositeMapping class methodsFor: 'functions'!
{Mapping} privateMakeMapping: cs {CoordinateSpace} 
	with: rs {CoordinateSpace} 
	with: mappings {ImmuSet of: Mapping}
	
	mappings isEmpty
		ifTrue: [^EmptyMapping make: cs with: rs]
		ifFalse:
			[mappings count = 1
				ifTrue: [^mappings theOne cast: Mapping]
				ifFalse: [^CompositeMapping create: cs with: rs with: mappings]]!
*/
}
/**
 * store a map into the set, checking to see if it can be combined with another
 */
public static void storeMapping(Mapping map, MuSet maps) {
	Stepper stomper = maps.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Mapping each = (Mapping) stomper.fetch();
		if (each == null) {
			continue ;
		}
		Mapping combined;
		combined = map.fetchCombine(each);
		if (combined != null) {
			combined = each.fetchCombine(map);
		}
		if (combined != null) {
			maps.remove(each);
			maps.introduce(combined);
			return ;
		}
	}
	stomper.destroy();
	maps.introduce(map);
/*
udanax-top.st:28854:CompositeMapping class methodsFor: 'functions'!
{void} storeMapping: map {Mapping} with: maps {MuSet of: Mapping}
	"store a map into the set, checking to see if it can be combined with another"
	maps stepper forEach: [ :each {Mapping} | | combined {Mapping} |
		combined _ map fetchCombine: each.
		combined ~~ NULL ifTrue:
			[combined _ each fetchCombine: map].
		combined ~~ NULL ifTrue:
			[maps remove: each.
			maps introduce: combined.
			^VOID]].
	maps introduce: map!
*/
}
public CompositeMapping() {
/*

Generated during transformation
*/
}
}
