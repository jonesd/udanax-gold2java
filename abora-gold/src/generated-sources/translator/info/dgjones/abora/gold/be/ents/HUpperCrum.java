/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.ents;

import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.ents.HUpperCrum;
import info.dgjones.abora.gold.be.ents.HistoryCrum;
import info.dgjones.abora.gold.be.ents.OPart;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class HUpperCrum extends HistoryCrum {

	protected TracePosition hcut;
	protected MuSet hcrums;
	protected BertCrum myBertCrum;
/*
udanax-top.st:27492:
HistoryCrum subclass: #HUpperCrum
	instanceVariableNames: '
		hcut {TracePosition}
		hcrums {MuSet of: OPart}
		myBertCrum {BertCrum}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:27499:
(HUpperCrum getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
/*
udanax-top.st:27664:
HUpperCrum class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:27667:
(HUpperCrum getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HUpperCrum.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Return true if the receiver can backfollow to trace.
 */
public boolean inTrace(TracePosition trace) {
	/* This chase up the htree could terminate early if the trace equalled 
	the trace in the receiver. This would be correct except that 
	oplanes can be created with a particular trace, only part of which 
	actually get included in the real orgl with that trace. */
	if (hcut.isLE(trace)) {
		Stepper stomper = hcrums.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			OPart oc = (OPart) stomper.fetch();
			if (oc == null) {
				continue ;
			}
			if (oc.hCrum().inTrace(trace)) {
				return true;
			}
		}
		stomper.destroy();
	}
	return false;
/*
udanax-top.st:27504:HUpperCrum methodsFor: 'testing'!
{BooleanVar} inTrace: trace {TracePosition} 
	"Return true if the receiver can backfollow to trace."
	"This chase up the htree could terminate early if the trace equalled 
	the trace in the receiver. This would be correct except that 
	oplanes can be created with a particular trace, only part of which 
	actually get included in the real orgl with that trace."
	(hcut isLE: trace) ifTrue: 
		[hcrums stepper forEach: 
			[:oc {OPart} |
			(oc hCrum inTrace: trace) ifTrue: [^true]]]. 
	^false!
*/
}
/**
 * Return true if their are no upward pointers.  This is used
 * by OParts to determine if they can be forgotten.
 */
public boolean isEmpty() {
	return hcrums.isEmpty();
/*
udanax-top.st:27517:HUpperCrum methodsFor: 'testing'!
{BooleanVar} isEmpty
	"Return true if their are no upward pointers.  This is used
	 by OParts to determine if they can be forgotten."
	
	^hcrums isEmpty!
*/
}
/**
 * If bertCrum is leafward of newBCrum then change it and return true,
 * otherwise return false.
 */
public boolean propagateBCrum(BertCrum newBCrum) {
	if (myBertCrum.isLE(newBCrum)) {
		return false;
	}
	else {
		if ( ! (newBCrum.isLE(myBertCrum))) {
			throw new AboraAssertionException("Unrelated bertsCrums!  Call dean.");
		}
		myBertCrum = newBCrum;
		return true;
	}
/*
udanax-top.st:27523:HUpperCrum methodsFor: 'testing'!
{Boolean} propagateBCrum: newBCrum {BertCrum} 
	"If bertCrum is leafward of newBCrum then change it and return true, 
	otherwise return false."
	(myBertCrum isLE: newBCrum)
		ifTrue: [^false]
		ifFalse: 
			[[(newBCrum isLE: myBertCrum) assert: 'Unrelated bertsCrums!!  Call dean.'] smalltalkOnly.
			myBertCrum _ newBCrum.
			^true]!
*/
}
/**
 * find the canopyCrum that goes with this hCrum.
 */
public BertCrum bertCrum() {
	return myBertCrum;
/*
udanax-top.st:27536:HUpperCrum methodsFor: 'accessing'!
{BertCrum} bertCrum
	"find the canopyCrum that goes with this hCrum."
	^myBertCrum!
*/
}
public TracePosition hCut() {
	return hcut;
/*
udanax-top.st:27541:HUpperCrum methodsFor: 'accessing'!
{TracePosition} hCut
	^hcut!
*/
}
/**
 * return the mapping into the domain space of the given trace
 */
public Mapping mappingTo(TracePosition trace, Mapping initial) {
	Mapping result;
	result = Mapping.make(initial.coordinateSpace(), initial.rangeSpace());
	if (inTrace(trace)) {
		Stepper stomper = hcrums.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			OPart each = (OPart) stomper.fetch();
			if (each == null) {
				continue ;
			}
			result = result.combine((each.mappingTo(trace, initial)));
		}
		stomper.destroy();
	}
	return result;
/*
udanax-top.st:27544:HUpperCrum methodsFor: 'accessing'!
{Mapping} mappingTo: trace {TracePosition} with: initial {Mapping}
	"return the mapping into the domain space of the given trace"
	| result {Mapping} |
	result _ Mapping make: initial coordinateSpace with: initial rangeSpace.
	(self inTrace: trace) ifTrue:
		[hcrums stepper forEach: [ :each {OPart} |
			result _ result combine: (each mappingTo: trace with: initial)]].
	^result!
*/
}
public ImmuSet oParents() {
	return hcrums.asImmuSet();
/*
udanax-top.st:27553:HUpperCrum methodsFor: 'accessing'!
{ImmuSet of: OPart} oParents
	^hcrums asImmuSet!
*/
}
/**
 * If this hcrum represents a fork, then it must get its own canopy crum.
 */
public void addOParent(OPart newCrum) {
	/* This routine could be drastically improved for orgl creation. */
	Someone.hack();
	/* Removed smalltalkOnly */
	updateBertCanopy(newCrum.hCrum().bertCrum());
	hcrums.store(newCrum);
/*
udanax-top.st:27558:HUpperCrum methodsFor: 'updating'!
{void} addOParent: newCrum {OPart} 
	"If this hcrum represents a fork, then it must get its own canopy crum."
	"This routine could be drastically improved for orgl creation."
	self hack.
	[newCrum testHChild: self] smalltalkOnly.
	self updateBertCanopy: newCrum hCrum bertCrum.
	hcrums store: newCrum!
*/
}
/**
 * Make a history crum with no upward pointers.
 */
public void removeOParent(OPart newCrum) {
	hcrums.remove(newCrum);
/*
udanax-top.st:27567:HUpperCrum methodsFor: 'updating'!
{void} removeOParent: newCrum {OPart} 
	"Make a history crum with no upward pointers."
	hcrums remove: newCrum.!
*/
}
/**
 * Apply filter on canopy
 */
public void actualDelayedStoreBackfollow(PropFinder finder, RecorderFossil fossil, ResultRecorder recorder, HashSetCache hCrumCache) {
	PropFinder newFinder;
	/* Simplify finder (to cut out no longer reachable tests). */
	newFinder = finder.pass(myBertCrum);
	/* If things are still findable, recur on each child. */
	if ( ! (newFinder.isEmpty())) {
		Stepper stomper = hcrums.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			OPart loaf = (OPart) stomper.fetch();
			if (loaf == null) {
				continue ;
			}
			loaf.hCrum().delayedStoreBackfollow(newFinder, fossil, recorder, hCrumCache);
		}
		stomper.destroy();
	}
/*
udanax-top.st:27574:HUpperCrum methodsFor: 'filtering'!
{void} actualDelayedStoreBackfollow: finder {PropFinder} 
	with: fossil {RecorderFossil}
	with: recorder {ResultRecorder}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	"Apply filter on canopy"
	
	| newFinder {PropFinder} |
	
	"Simplify finder (to cut out no longer reachable tests)."
	newFinder _ finder pass: myBertCrum.
	
	"If things are still findable, recur on each child."
	newFinder isEmpty ifFalse:
		[hcrums stepper forEach: [:loaf {OPart} |
			loaf hCrum delayedStoreBackfollow: newFinder
				with: fossil
				with: recorder
				with: hCrumCache]]!
*/
}
public boolean anyPasses(PropFinder finder) {
	if (finder.doesPass(myBertCrum)) {
		Stepper stomper = hcrums.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			OPart loaf = (OPart) stomper.fetch();
			if (loaf == null) {
				continue ;
			}
			if (loaf.hCrum().anyPasses(finder)) {
				return true;
			}
		}
		stomper.destroy();
	}
	return false;
/*
udanax-top.st:27594:HUpperCrum methodsFor: 'filtering'!
{BooleanVar} anyPasses: finder {PropFinder}
	(finder doesPass: myBertCrum) 
		ifTrue: [hcrums stepper forEach: 
					[:loaf {OPart} |
					(loaf hCrum anyPasses: finder)
						ifTrue: [^true]]].
	^false!
*/
}
public void ringDetectors(FeEdition edition) {
	if (bertCrum().isSensorWaiting()) {
		Stepper stomper = oParents().stepper();
		for (; stomper.hasValue(); stomper.step()) {
			OPart o = (OPart) stomper.fetch();
			if (o == null) {
				continue ;
			}
			o.hCrum().ringDetectors(edition);
		}
		stomper.destroy();
	}
/*
udanax-top.st:27602:HUpperCrum methodsFor: 'filtering'!
{void} ringDetectors: edition {FeEdition}
	self bertCrum isSensorWaiting ifTrue:
		[self oParents stepper forEach: [ :o {OPart} |
			o hCrum ringDetectors: edition]]!
*/
}
/**
 * Make my bertCrum the join of its current value and bCrum.
 */
public void updateBertCanopy(BertCrum bCrum) {
	if ( ! (myBertCrum.isLE(bCrum))) {
		BertCrum oldBCrum;
		oldBCrum = myBertCrum;
		myBertCrum = (BertCrum) (myBertCrum.computeJoin(bCrum));
		if ((myBertCrum) != (oldBCrum)) {
			myBertCrum.addPointer(this);
			oldBCrum.removePointer(this);
		}
	}
/*
udanax-top.st:27610:HUpperCrum methodsFor: 'private:'!
{void} updateBertCanopy: bCrum {BertCrum} 
	"Make my bertCrum the join of its current value and bCrum."
	(myBertCrum isLE: bCrum) ifFalse: 
			[| oldBCrum {BertCrum} |
			oldBCrum _ myBertCrum.
			myBertCrum _ (myBertCrum computeJoin: bCrum) cast: BertCrum.
			(myBertCrum basicCast: BertCrum) ~~ (oldBCrum basicCast: BertCrum) ifTrue:
				[myBertCrum addPointer: self.
				oldBCrum removePointer: self]]!
*/
}
public HUpperCrum(TracePosition trace, BertCrum canopy) {
	super();
	hcut = trace;
	myBertCrum = canopy;
	myBertCrum.addPointer(this);
	hcrums = MuSet.make();
/*
udanax-top.st:27622:HUpperCrum methodsFor: 'create'!
create: trace {TracePosition} with: canopy {BertCrum}
	super create.
	hcut _ trace.
	myBertCrum _ canopy.
	myBertCrum addPointer: self.
	hcrums _ MuSet make!
*/
}
public HUpperCrum(OPart first, OPart second, TracePosition trace) {
	super();
	MuSet set;
	hcut = trace;
	/* self halt. */
	set = MuSet.makeIntegerVar(2);
	set.introduce(first);
	set.introduce(second);
	hcrums = set;
	myBertCrum = first.hCrum().bertCrum();
	updateBertCanopy(second.hCrum().bertCrum());
	myBertCrum.addPointer(this);
/*
udanax-top.st:27629:HUpperCrum methodsFor: 'create'!
create: first {OPart} with: second {OPart} with: trace {TracePosition} 
	| set {MuSet} |
	super create.
	hcut _ trace.
	"self halt."
	set _ MuSet make: 2.
	set introduce: first.
	set introduce: second.
	hcrums _ set.
	myBertCrum _ first hCrum bertCrum.
	self updateBertCanopy: second hCrum bertCrum.
	myBertCrum addPointer: self!
*/
}
/*
udanax-top.st:27644:HUpperCrum methodsFor: 'smalltalk:'!
inspectOrgls
	hcrums count == 1
		ifTrue: [hcrums stepper get inspect]
		ifFalse: [hcrums asOrderedCollection inspect]!
*/
public HUpperCrum(Rcvr receiver) {
	super(receiver);
	hcut = (TracePosition) receiver.receiveHeaper();
	hcrums = (MuSet) receiver.receiveHeaper();
	myBertCrum = (BertCrum) receiver.receiveHeaper();
/*
udanax-top.st:27651:HUpperCrum methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	hcut _ receiver receiveHeaper.
	hcrums _ receiver receiveHeaper.
	myBertCrum _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(hcut);
	xmtr.sendHeaper(hcrums);
	xmtr.sendHeaper(myBertCrum);
/*
udanax-top.st:27657:HUpperCrum methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: hcut.
	xmtr sendHeaper: hcrums.
	xmtr sendHeaper: myBertCrum.!
*/
}
public static HUpperCrum make(Heaper something) {
	if (something instanceof HUpperCrum) {
		return makeHUpperCrum((HUpperCrum) something);
	}
	if (something instanceof BertCrum) {
		return makeBertCrum((BertCrum) something);
	}
	throw new AboraRuntimeException(AboraRuntimeException.FATAL_ERROR);
/*
udanax-top.st:27672:HUpperCrum class methodsFor: 'smalltalk: create'!
make: something {Heaper}
	(something isKindOf: HUpperCrum) ifTrue: [^self make.HUpperCrum: something].
	(something isKindOf: BertCrum) ifTrue: [^self make.BertCrum: something].
	Heaper BLAST: #FatalError!
*/
}
public static HUpperCrum make() {
	AboraBlockSupport.enterConsistent();
	try {
		return new HUpperCrum(((TracePosition) CurrentTrace.fluidGet()), ((BertCrum) CurrentBertCrum.fluidGet()));
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:27679:HUpperCrum class methodsFor: 'instance creation'!
make
	[Ent] USES.
	DiskManager consistent: [
	^HUpperCrum create: CurrentTrace fluidGet with: CurrentBertCrum fluidGet].
	^ NULL "Compiler fodder"!
*/
}
public static HUpperCrum makeBertCrum(BertCrum bertCrum) {
	return new HUpperCrum(((TracePosition) CurrentTrace.fluidGet()), bertCrum);
/*
udanax-top.st:27685:HUpperCrum class methodsFor: 'instance creation'!
make.BertCrum: bertCrum {BertCrum}
	^HUpperCrum create: CurrentTrace fluidGet with: bertCrum!
*/
}
public static HUpperCrum makeHUpperCrum(HUpperCrum hcrum) {
	return new HUpperCrum(hcrum.hCut(), hcrum.bertCrum());
/*
udanax-top.st:27688:HUpperCrum class methodsFor: 'instance creation'!
make.HUpperCrum: hcrum {HUpperCrum}
	^HUpperCrum create: hcrum hCut with: hcrum bertCrum!
*/
}
public HUpperCrum() {
/*

Generated during transformation
*/
}
}
