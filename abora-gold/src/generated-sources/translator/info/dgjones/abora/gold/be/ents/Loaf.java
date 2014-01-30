/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.ents;

import info.dgjones.abora.gold.arrange.Arrangement;
import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.be.basic.BeCarrier;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.ents.ActualOrglRoot;
import info.dgjones.abora.gold.be.ents.HUpperCrum;
import info.dgjones.abora.gold.be.ents.HistoryCrum;
import info.dgjones.abora.gold.be.ents.InnerLoaf;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.OExpandingLoaf;
import info.dgjones.abora.gold.be.ents.OPart;
import info.dgjones.abora.gold.be.ents.OPartialLoaf;
import info.dgjones.abora.gold.be.ents.OVirtualLoaf;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.be.ents.RegionLoaf;
import info.dgjones.abora.gold.be.ents.SharedData;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.PrimDataArray;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.HRoot;
import info.dgjones.abora.gold.java.missing.XnSensor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class Loaf extends OPart {

	protected HUpperCrum myHCrum;
/*
udanax-top.st:7375:
OPart subclass: #Loaf
	instanceVariableNames: 'myHCrum {HUpperCrum}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:7379:
(Loaf getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #DEFERRED; add: #DEFERRED.LOCKED; yourself)!
*/
/*
udanax-top.st:7649:
Loaf class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:7652:
(Loaf getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #DEFERRED; add: #DEFERRED.LOCKED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Loaf.class).setAttributes( new Set().add("SHEPHERDPATRIARCH").add("COPY").add("DEFERRED").add("DEFERREDLOCKED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * return a mapping from my data to corresponding stuff in the given trace
 */
public Mapping compare(TracePosition trace, XnRegion region) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7384:Loaf methodsFor: 'accessing'!
{Mapping} compare: trace {TracePosition} with: region {XnRegion}
	"return a mapping from my data to corresponding stuff in the given trace"
	self subclassResponsibility!
*/
}
public int count() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7388:Loaf methodsFor: 'accessing'!
{IntegerVar} count
	self subclassResponsibility!
*/
}
public XnRegion domain() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7391:Loaf methodsFor: 'accessing'!
{XnRegion} domain
	self subclassResponsibility!
*/
}
/**
 * Look up the range element for the key.  If it is embedded within a virtual
 * structure, then make a virtual range element using the edition and globalKey.
 */
public FeRangeElement fetch(Position key, BeEdition edition, Position globalKey) {
	Someone.thingToDo();
	/* This should softSplay the position up. */
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7394:Loaf methodsFor: 'accessing'!
{FeRangeElement | NULL} fetch: key {Position} with: edition {BeEdition} with: globalKey {Position}
	"Look up the range element for the key.  If it is embedded within a virtual
	 structure, then make a virtual range element using the edition and globalKey."
	
	self thingToDo.  "This should softSplay the position up."
	self subclassResponsibility!
*/
}
/**
 * Return the bottom-most Loaf.  Used to get the owner and such of a position.
 */
public OExpandingLoaf fetchBottomAt(Position key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7401:Loaf methodsFor: 'accessing'!
{OExpandingLoaf} fetchBottomAt: key {Position}
	"Return the bottom-most Loaf.  Used to get the owner and such of a position."
	self subclassResponsibility!
*/
}
/**
 * Fill an array with my contents
 */
public void fill(XnRegion keys, Arrangement toArrange, PrimArray toArray, Dsp globalDsp, BeEdition edition) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7406:Loaf methodsFor: 'accessing'!
{void} fill: keys {XnRegion}
	with: toArrange {Arrangement}
	with: toArray {PrimArray}
	with: globalDsp {Dsp}
	with: edition {BeEdition}
	"Fill an array with my contents"
	
	self subclassResponsibility!
*/
}
/**
 * Get or Make the BeRangeElement at the location.
 */
public BeRangeElement getBe(Position key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7415:Loaf methodsFor: 'accessing'!
{BeRangeElement} getBe: key {Position}
	"Get or Make the BeRangeElement at the location."
		
	self subclassResponsibility!
*/
}
public XnRegion rangeOwners(XnRegion positions) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7420:Loaf methodsFor: 'accessing'!
{XnRegion} rangeOwners: positions {XnRegion | NULL} 
	
	self subclassResponsibility!
*/
}
/**
 * Recur assigning owners.  Return the portion of the o-tree that
 * couldn't be assigned, or NULL if it was all assigned.
 */
public OrglRoot setAllOwners(ID owner) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7424:Loaf methodsFor: 'accessing'!
{OrglRoot} setAllOwners: owner {ID}
	"Recur assigning owners.  Return the portion of the o-tree that
	 couldn't be assigned, or NULL if it was all assigned."
		
	self subclassResponsibility!
*/
}
public XnRegion usedDomain() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7430:Loaf methodsFor: 'accessing'!
{XnRegion} usedDomain
	self subclassResponsibility!
*/
}
/**
 * Return a stepper of bundles according to the order.
 */
public Stepper bundleStepper(XnRegion region, OrderSpec order, Dsp globalDsp) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7435:Loaf methodsFor: 'operations'!
{Stepper} bundleStepper: region {XnRegion} with: order {OrderSpec} with: globalDsp {Dsp}
	"Return a stepper of bundles according to the order."
	
	self subclassResponsibility!
*/
}
public OrglRoot combine(ActualOrglRoot another, XnRegion limitRegion, Dsp globalDsp) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7440:Loaf methodsFor: 'operations'!
{OrglRoot} combine: another {ActualOrglRoot} with: limitRegion {XnRegion} with: globalDsp {Dsp}
	self subclassResponsibility!
*/
}
/**
 * Just search for now.
 */
public XnRegion keysLabelled(BeLabel label) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7443:Loaf methodsFor: 'operations'!
{XnRegion} keysLabelled: label {BeLabel}
	"Just search for now."
	self subclassResponsibility!
*/
}
/**
 * Return a region describing the stuff that can backfollow to trace.
 */
public XnRegion sharedRegion(TracePosition trace, XnRegion limitRegion) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7448:Loaf methodsFor: 'operations'!
{XnRegion} sharedRegion: trace {TracePosition} with: limitRegion {XnRegion}
	"Return a region describing the stuff that can backfollow to trace."
	self subclassResponsibility!
*/
}
/**
 * Return a copy with externalDsp added to the receiver's dsp.
 */
public Loaf transformedBy(Dsp externalDsp) {
	if (externalDsp.isIdentity()) {
		return this;
	}
	else {
		return InnerLoaf.make(this, externalDsp);
	}
/*
udanax-top.st:7452:Loaf methodsFor: 'operations'!
{Loaf} transformedBy: externalDsp {Dsp}
	"Return a copy with externalDsp added to the receiver's dsp."
	externalDsp isIdentity 
		ifTrue: [^self]
		ifFalse: [^InnerLoaf make: self with: externalDsp]!
*/
}
/**
 * Return a copy with globalDsp removed from the receiver's dsp.
 */
public Loaf unTransformedBy(Dsp globalDsp) {
	if (globalDsp.isIdentity()) {
		return this;
	}
	else {
		return InnerLoaf.make(this, ((Dsp) globalDsp.inverse()));
	}
/*
udanax-top.st:7459:Loaf methodsFor: 'operations'!
{Loaf} unTransformedBy: globalDsp {Dsp}
	"Return a copy with globalDsp removed from the receiver's dsp."
	globalDsp isIdentity 
		ifTrue: [^self]
		ifFalse: [^InnerLoaf make: self with: (globalDsp inverse cast: Dsp)]!
*/
}
/**
 * Make each child completely contained or completely outside
 * the region. Return the number of children completely in the region.
 * Full containment cases can be handled generically.
 */
public int splay(XnRegion region, XnRegion limitRegion) {
	if (limitRegion.isSubsetOf(region)) {
		return 2;
	}
	else {
		if (limitRegion.intersects(region)) {
			return actualSplay(region, limitRegion);
		}
		else {
			return 0;
		}
	}
/*
udanax-top.st:7468:Loaf methodsFor: 'splay'!
{UInt8} splay: region {XnRegion} with: limitRegion {XnRegion} 
	"Make each child completely contained or completely outside 
	the region. Return the number of children completely in the region. 
	Full containment cases can be handled generically."
	(limitRegion isSubsetOf: region)
		ifTrue: [^2]
		ifFalse: [(limitRegion intersects: region)
				ifTrue: [^self actualSplay: region with: limitRegion]
				ifFalse: [^Int0]]!
*/
}
/**
 * Speciall handle the splay cases in which the region partially intersects
 * with limitedRegion.  These require rotations and splitting.
 */
public int actualSplay(XnRegion region, XnRegion limitRegion) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7481:Loaf methodsFor: 'protected: splay'!
{Int8} actualSplay: region {XnRegion} with: limitRegion {XnRegion} 
	"Speciall handle the splay cases in which the region partially intersects
	 with limitedRegion.  These require rotations and splitting."
	 
	 self subclassResponsibility!
*/
}
/**
 * This should probably take a bertCanopyCrum argument, as well.
 */
public void addOParent(OPart oParent) {
	/* add oParent to the set of upward pointers. */
	myHCrum.addOParent(oParent);
	remember();
	diskUpdate();
/*
udanax-top.st:7489:Loaf methodsFor: 'backfollow'!
{void} addOParent: oParent {OPart}
	"This should probably take a bertCanopyCrum argument, as well."
	"add oParent to the set of upward pointers."
	myHCrum addOParent: oParent.
	self remember.
	self diskUpdate!
*/
}
public XnRegion attachTrailBlazer(TrailBlazer blazer) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7497:Loaf methodsFor: 'backfollow'!
{XnRegion} attachTrailBlazer: blazer {TrailBlazer}
	
	self subclassResponsibility!
*/
}
/**
 * send checkRecorders to all children
 */
public void checkChildRecorders(PropFinder finder) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7501:Loaf methodsFor: 'backfollow'!
{void} checkChildRecorders: finder {PropFinder}
	"send checkRecorders to all children"
	self subclassResponsibility!
*/
}
/**
 * check any recorders that might be triggered by a change in the edition.
 * Walk leafward on O-plane, filtered by sensor canopy, ringing recorders.
 * Not in a consistent block:  It spawns unbounded work.
 */
public void checkRecorders(PropFinder finder, SensorCrum scrum) {
	PropFinder newFinder;
	/* Shrink finder to just what may be on this branch of O-tree.
	 If there might be something on this branch
	 	Check the children using the simplified finder. */
	newFinder = sensorCrum().checkRecorders(finder, scrum);
	if ( ! (newFinder.isEmpty())) {
		checkChildRecorders(newFinder);
	}
/*
udanax-top.st:7505:Loaf methodsFor: 'backfollow'!
{void} checkRecorders: finder {PropFinder} 
	with: scrum {SensorCrum | NULL}
	"check any recorders that might be triggered by a change in the edition.
	 Walk leafward on O-plane, filtered by sensor canopy, ringing recorders.
	 
	 Not in a consistent block:  It spawns unbounded work. "
	
	| newFinder {PropFinder} |
	
	"Shrink finder to just what may be on this branch of O-tree.
	 If there might be something on this branch
	 	Check the children using the simplified finder."
	 	
	newFinder _ self sensorCrum checkRecorders: finder with: scrum.
	newFinder isEmpty ifFalse:
		[self checkChildRecorders: newFinder]!
*/
}
public void checkTrailBlazer(TrailBlazer blazer) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7522:Loaf methodsFor: 'backfollow'!
{void} checkTrailBlazer: blazer {TrailBlazer}
	
	self subclassResponsibility!
*/
}
/**
 * One step of walk south on the O-tree during the 'now' part of a backfollow.
 */
public void delayedStoreMatching(PropFinder finder, RecorderFossil fossil, ResultRecorder recorder, HashSetCache hCrumCache) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7526:Loaf methodsFor: 'backfollow'!
{void} delayedStoreMatching: finder {PropFinder} 
	with: fossil {RecorderFossil} 
	with: recorder {ResultRecorder}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	"One step of walk south on the O-tree during the 'now' part of a backfollow."
	
	self subclassResponsibility!
*/
}
public TrailBlazer fetchTrailBlazer() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7535:Loaf methodsFor: 'backfollow'!
{TrailBlazer | NULL} fetchTrailBlazer
	
	self subclassResponsibility!
*/
}
public HistoryCrum hCrum() {
	return myHCrum;
/*
udanax-top.st:7539:Loaf methodsFor: 'backfollow'!
{HistoryCrum} hCrum
	^myHCrum!
*/
}
/**
 * remove oparent from the set of upward pointers.
 */
public void removeOParent(OPart oparent) {
	myHCrum.removeOParent(oparent);
	if (myHCrum.isEmpty()) {
		/* Now we get into the risky part of deletion.  There are
			 no more upward pointers, so destroy the receiver. */
		destroy();
	}
	else {
		diskUpdate();
	}
/*
udanax-top.st:7542:Loaf methodsFor: 'backfollow'!
{void} removeOParent: oparent {OPart}
	"remove oparent from the set of upward pointers."
	myHCrum removeOParent: oparent.
	myHCrum isEmpty 
		ifTrue:
			["Now we get into the risky part of deletion.  There are
			 no more upward pointers, so destroy the receiver."
			self destroy]
		ifFalse: [self diskUpdate]!
*/
}
/**
 * Go ahead and actually store the recorder in the sensor canopy.  However, instead of
 * propogating the props immediately, accumulate all those agenda items into the 'agenda'
 * parameter.  This is done instead of scheduling them directly because our client needs to
 * schedule something else following all the prop propogation.
 */
public void storeRecordingAgents(RecorderFossil recorder, Agenda agenda) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7553:Loaf methodsFor: 'backfollow'!
{void} storeRecordingAgents: recorder {RecorderFossil}
	with: agenda {Agenda}
	"Go ahead and actually store the recorder in the sensor canopy.  However, instead of propogating the props immediately, accumulate all those agenda items into the 'agenda' parameter.  This is done instead of scheduling them directly because our client needs to schedule something else following all the prop propogation."
	
	self subclassResponsibility!
*/
}
/**
 * A Detector has been added to my parent. Walk down and trigger it on all non-partial stuff
 */
public void triggerDetector(FeFillRangeDetector detect) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7559:Loaf methodsFor: 'backfollow'!
{void} triggerDetector: detect {FeFillRangeDetector}
	"A Detector has been added to my parent. Walk down and trigger it on all non-partial stuff"
	
	self subclassResponsibility!
*/
}
/**
 * Ensure the my bertCrum is not be leafward of newBCrum.
 */
public boolean updateBCrumTo(BertCrum newBCrum) {
	if (myHCrum.propagateBCrum(newBCrum)) {
		diskUpdate();
		return true;
	}
	return false;
/*
udanax-top.st:7564:Loaf methodsFor: 'backfollow'!
{BooleanVar} updateBCrumTo: newBCrum {BertCrum} 
	"Ensure the my bertCrum is not be leafward of newBCrum."
	(myHCrum propagateBCrum: newBCrum)
		ifTrue:
			[self diskUpdate.
			^true].
	^false!
*/
}
/**
 * Make a FeEdition out of myself. Used for triggering Detectors
 */
public FeEdition asFeEdition() {
	Object currentTraceOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentTrace, hCrum().hCut());
	try {
		Object currentBertCrumOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentBertCrum, hCrum().bertCrum());
		try {
			return FeEdition.on((BeEdition.make((ActualOrglRoot.make(this, domain())))));
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(CurrentBertCrum, currentBertCrumOldValue);
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentTrace, currentTraceOldValue);
	}
/*
udanax-top.st:7575:Loaf methodsFor: 'protected:'!
{FeEdition} asFeEdition
	"Make a FeEdition out of myself. Used for triggering Detectors"
	
	CurrentTrace fluidBind: self hCrum hCut during:
	[CurrentBertCrum fluidBind: self hCrum bertCrum during:
		[^FeEdition on: (BeEdition make: (ActualOrglRoot make: self with: self domain))]]!
*/
}
public void dismantle() {
	AboraBlockSupport.enterInsistent(2);
	try {
		super.dismantle();
		myHCrum = null;
	}
	finally {
		AboraBlockSupport.exitInsistent();
	}
/*
udanax-top.st:7582:Loaf methodsFor: 'protected:'!
{void} dismantle
	DiskManager insistent: 2 with:
		[super dismantle.
		myHCrum _ NULL]!
*/
}
public Loaf(HUpperCrum hcrum, SensorCrum scrum) {
	super(scrum);
	if (hcrum == null) {
		myHCrum = HUpperCrum.make();
	}
	else {
		myHCrum = hcrum;
	}
/*
udanax-top.st:7589:Loaf methodsFor: 'create'!
create: hcrum {HUpperCrum | NULL} with: scrum {SensorCrum | NULL} 
	super create: scrum.
	hcrum == NULL
		ifTrue: [myHCrum _ HUpperCrum make]
		ifFalse: [myHCrum _ hcrum]!
*/
}
public Loaf(int hash, HUpperCrum hcrum, SensorCrum scrum) {
	super(hash, scrum);
	if (hcrum == null) {
		myHCrum = HUpperCrum.make();
	}
	else {
		myHCrum = hcrum;
	}
/*
udanax-top.st:7596:Loaf methodsFor: 'create'!
create: hash {UInt32} with: hcrum {HUpperCrum | NULL} with: scrum {SensorCrum | NULL} 
	super create: hash with: scrum.
	hcrum == NULL
		ifTrue: [myHCrum _ HUpperCrum make]
		ifFalse: [myHCrum _ hcrum]!
*/
}
public int contentsHash() {
	return super.contentsHash() ^ myHCrum.hashForEqual();
/*
udanax-top.st:7604:Loaf methodsFor: 'testing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: myHCrum hashForEqual!
*/
}
/**
 * @deprecated
 */
public void checkChildRecorders(BeEdition stamp, PropFinder finder) {
	throw new PasseException();
/*
udanax-top.st:7611:Loaf methodsFor: 'smalltalk: passe'!
{void} checkChildRecorders: stamp {BeEdition} with: finder {PropFinder}
self passe "fewer args"!
*/
}
/**
 * @deprecated
 */
public void checkRecorders(BeEdition edition, PropFinder finder, SensorCrum scrum) {
	throw new PasseException();
/*
udanax-top.st:7614:Loaf methodsFor: 'smalltalk: passe'!
{void} checkRecorders: edition {BeEdition} 
	with: finder {PropFinder} 
	with: scrum {SensorCrum | NULL}
	self passe "fewer args"!
*/
}
/**
 * @deprecated
 */
public void delayedStoreMatching(PropFinder finder, RecorderFossil recorder, HashSetCache hCrumCache) {
	throw new PasseException();
/*
udanax-top.st:7619:Loaf methodsFor: 'smalltalk: passe'!
{void} delayedStoreMatching: finder {PropFinder} 
	with: recorder {RecorderFossil}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	self passe "extra argument"!
*/
}
/**
 * inform a piece of partiality
 * @deprecated
 */
public void inform(Position key, HRoot value, TracePosition trace) {
	throw new PasseException();
/*
udanax-top.st:7624:Loaf methodsFor: 'smalltalk: passe'!
{void} inform: key {Position} with: value {HRoot} with: trace {TracePosition}
	"inform a piece of partiality"
	
	self passe!
*/
}
/**
 * @deprecated
 */
public void storeMatching(PropFinder finder, MuTable table, HashSetCache hCrumCache) {
	throw new PasseException();
/*
udanax-top.st:7629:Loaf methodsFor: 'smalltalk: passe'!
{void} storeMatching: finder {PropFinder} 
	with: table {MuTable of: ID and: BeEdition} 
	with: hCrumCache {HashSetCache of: HistoryCrum}
	self passe!
*/
}
/**
 * @deprecated
 */
public void wait(XnSensor sensor) {
	throw new PasseException();
/*
udanax-top.st:7634:Loaf methodsFor: 'smalltalk: passe'!
{void} wait: sensor {XnSensor}
	
	self passe!
*/
}
public Loaf(Rcvr receiver) {
	super(receiver);
	myHCrum = (HUpperCrum) receiver.receiveHeaper();
/*
udanax-top.st:7640:Loaf methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myHCrum _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myHCrum);
/*
udanax-top.st:7644:Loaf methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myHCrum.!
*/
}
public static Loaf makeRegion(XnRegion region, BeCarrier element) {
	AboraBlockSupport.enterConsistent(7);
	try {
		return new RegionLoaf(region, element.fetchLabel(), element.rangeElement(), null);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:7657:Loaf class methodsFor: 'create'!
{Loaf} make.Region: region {XnRegion} with: element {BeCarrier}
	
	DiskManager consistent: 7 with: 
		[^RegionLoaf create: region with: element fetchLabel with: element rangeElement with: NULL]!
*/
}
public static Loaf makeXnRegion(XnRegion region) {
	AboraBlockSupport.enterConsistent(3);
	try {
		return new OPartialLoaf(region, null, SensorCrum.partial());
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:7662:Loaf class methodsFor: 'create'!
make.XnRegion: region {XnRegion}
	
	DiskManager consistent: 3 with:
		[^OPartialLoaf create: region
			with: NULL
			with: SensorCrum partial]!
*/
}
public static Loaf make(PrimDataArray values, Arrangement arrangement) {
	AboraBlockSupport.enterConsistent(4);
	try {
		SharedData tmp;
		tmp = new SharedData(values, arrangement);
		return new OVirtualLoaf(arrangement.region(), tmp);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:7669:Loaf class methodsFor: 'create'!
make: values {PrimDataArray} with: arrangement {Arrangement}
	
	DiskManager consistent: 4 with:
		[| tmp {SharedData} |
		tmp _ SharedData create: values with: arrangement.
		^OVirtualLoaf create: arrangement region with: tmp]!
*/
}
public Loaf() {
/*

Generated during transformation
*/
}
}
