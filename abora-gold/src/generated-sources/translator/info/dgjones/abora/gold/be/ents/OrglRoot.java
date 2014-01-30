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
import info.dgjones.abora.gold.be.ents.EmptyOrglRoot;
import info.dgjones.abora.gold.be.ents.HBottomCrum;
import info.dgjones.abora.gold.be.ents.HistoryCrum;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.OPart;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.collection.basic.PrimDataArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.HRoot;
import info.dgjones.abora.gold.java.missing.XnSensor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class OrglRoot extends OPart {

	protected HBottomCrum myHCrum;
/*
udanax-top.st:9563:
OPart subclass: #OrglRoot
	instanceVariableNames: 'myHCrum {HBottomCrum}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:9567:
(OrglRoot getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #DEFERRED; add: #DEFERRED.LOCKED; yourself)!
*/
/*
udanax-top.st:9816:
OrglRoot class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:9819:
(OrglRoot getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #DEFERRED; add: #DEFERRED.LOCKED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OrglRoot.class).setAttributes( new Set().add("SHEPHERDPATRIARCH").add("COPY").add("DEFERRED").add("DEFERREDLOCKED"));
/*

Generated during transformation: AddMethod
*/
}
public XnRegion attachTrailBlazer(TrailBlazer blazer) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9572:OrglRoot methodsFor: 'backfollow'!
{XnRegion} attachTrailBlazer: blazer {TrailBlazer}
	
	self subclassResponsibility!
*/
}
/**
 * check any recorders that might be triggered by a change in the stamp
 */
public void checkRecorders(PropFinder finder, SensorCrum scrum) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9576:OrglRoot methodsFor: 'backfollow'!
{void} checkRecorders: finder {PropFinder} 
	with: scrum {SensorCrum | NULL}
	"check any recorders that might be triggered by a change in the stamp"
	
	self subclassResponsibility!
*/
}
public void checkTrailBlazer(TrailBlazer blazer) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9582:OrglRoot methodsFor: 'backfollow'!
{void} checkTrailBlazer: blazer {TrailBlazer}
	
	self subclassResponsibility!
*/
}
public TrailBlazer fetchTrailBlazer() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9586:OrglRoot methodsFor: 'backfollow'!
{TrailBlazer | NULL} fetchTrailBlazer
	
	self subclassResponsibility!
*/
}
/**
 * NOTE: The AgendaItem returned is not yet scheduled.  Doing so is up to my caller.
 */
public AgendaItem propChanger(PropChange change) {
	return myHCrum.propChanger(change);
/*
udanax-top.st:9590:OrglRoot methodsFor: 'backfollow'!
{AgendaItem} propChanger: change {PropChange}
	"NOTE: The AgendaItem returned is not yet scheduled.  Doing so is up to my caller."
	^myHCrum propChanger: change!
*/
}
/**
 * A Detector has been added to my parent. Walk down and trigger it on all non-partial stuff
 */
public void triggerDetector(FeFillRangeDetector detect) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9595:OrglRoot methodsFor: 'backfollow'!
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
udanax-top.st:9600:OrglRoot methodsFor: 'backfollow'!
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
 * the kind of domain elements allowed
 */
public CoordinateSpace coordinateSpace() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9611:OrglRoot methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	"the kind of domain elements allowed"
	self subclassResponsibility!
*/
}
public int count() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9616:OrglRoot methodsFor: 'accessing'!
{IntegerVar} count
	self subclassResponsibility!
*/
}
public XnRegion domain() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9619:OrglRoot methodsFor: 'accessing'!
{XnRegion} domain
	self subclassResponsibility!
*/
}
/**
 * get an individual element
 */
public FeRangeElement fetch(Position key, BeEdition edition) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9622:OrglRoot methodsFor: 'accessing'!
{FeRangeElement | NULL} fetch: key {Position} with: edition {BeEdition}
	"get an individual element"
	self subclassResponsibility!
*/
}
/**
 * Get or Make the BeRangeElement at the location.
 */
public BeRangeElement getBe(Position key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9627:OrglRoot methodsFor: 'accessing'!
{BeRangeElement} getBe: key {Position}
	"Get or Make the BeRangeElement at the location."
	self subclassResponsibility!
*/
}
public HistoryCrum hCrum() {
	return myHCrum;
/*
udanax-top.st:9632:OrglRoot methodsFor: 'accessing'!
{HistoryCrum} hCrum
	^myHCrum!
*/
}
/**
 * This is primarily for the example routines.
 */
public TracePosition hCut() {
	return myHCrum.hCut();
/*
udanax-top.st:9635:OrglRoot methodsFor: 'accessing'!
{TracePosition} hCut
	"This is primarily for the example routines."
	^myHCrum hCut!
*/
}
public void introduceEdition(BeEdition edition) {
	myHCrum.introduceEdition(edition);
	remember();
	diskUpdate();
/*
udanax-top.st:9640:OrglRoot methodsFor: 'accessing'!
{void} introduceEdition: edition {BeEdition}
	myHCrum introduceEdition: edition.
	self remember.
	self diskUpdate!
*/
}
public boolean isEmpty() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9646:OrglRoot methodsFor: 'accessing'!
{BooleanVar} isEmpty
	self subclassResponsibility!
*/
}
/**
 * Just search for now.
 */
public XnRegion keysLabelled(BeLabel label) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9649:OrglRoot methodsFor: 'accessing'!
{XnRegion} keysLabelled: label {BeLabel}
	"Just search for now."
	self subclassResponsibility!
*/
}
/**
 * return a mapping from my data to corresponding stuff in the given trace
 */
public Mapping mapSharedTo(TracePosition trace) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9654:OrglRoot methodsFor: 'accessing'!
{Mapping} mapSharedTo: trace {TracePosition}
	"return a mapping from my data to corresponding stuff in the given trace"
	self subclassResponsibility!
*/
}
/**
 * Return the owner for the given position in the receiver.
 */
public ID ownerAt(Position key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9658:OrglRoot methodsFor: 'accessing'!
{ID} ownerAt: key {Position}
	"Return the owner for the given position in the receiver."
	
	self subclassResponsibility!
*/
}
public XnRegion rangeOwners(XnRegion positions) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9663:OrglRoot methodsFor: 'accessing'!
{XnRegion} rangeOwners: positions {XnRegion | NULL} 
	
	self subclassResponsibility!
*/
}
public void removeEdition(BeEdition stamp) {
	myHCrum.removeEdition(stamp);
	if (myHCrum.isEmpty()) {
		/* Now we get into the risky part of deletion.  Only Editions can keep OrglRoots around, so destroy the receiver. */
		destroy();
	}
	else {
		diskUpdate();
	}
/*
udanax-top.st:9667:OrglRoot methodsFor: 'accessing'!
{void} removeEdition: stamp {BeEdition}
	myHCrum removeEdition: stamp.
	myHCrum isEmpty
		ifTrue: 
			["Now we get into the risky part of deletion.  Only Editions can keep OrglRoots around, so destroy the receiver."
			self destroy]
		ifFalse: [self diskUpdate]!
*/
}
/**
 * Return the portiong whose owner couldn't be changed.
 */
public OrglRoot setAllOwners(ID owner) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9676:OrglRoot methodsFor: 'accessing'!
{OrglRoot} setAllOwners: owner {ID}
	"Return the portiong whose owner couldn't be changed."
		
	self subclassResponsibility!
*/
}
/**
 * Return a region for all the stuff in this orgl that can backfollow to trace.
 */
public XnRegion sharedRegion(TracePosition trace) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9681:OrglRoot methodsFor: 'accessing'!
{XnRegion} sharedRegion: trace {TracePosition}
	"Return a region for all the stuff in this orgl that can backfollow to trace."
	self subclassResponsibility!
*/
}
/**
 * Return a simple region that encloses the domain of the receiver.
 */
public XnRegion simpleDomain() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9686:OrglRoot methodsFor: 'accessing'!
{XnRegion} simpleDomain
	"Return a simple region that encloses the domain of the receiver."
	
	self subclassResponsibility!
*/
}
/**
 * Return the owner for the given position in the receiver.
 */
public PrimSpec specAt(Position key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9691:OrglRoot methodsFor: 'accessing'!
{PrimSpec} specAt: key {Position}
	"Return the owner for the given position in the receiver."
	
	self subclassResponsibility!
*/
}
public XnRegion usedDomain() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9696:OrglRoot methodsFor: 'accessing'!
{XnRegion} usedDomain
	self subclassResponsibility!
*/
}
/**
 * Return a stepper of bundles according to the order.
 */
public Stepper bundleStepper(XnRegion region, OrderSpec order) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9701:OrglRoot methodsFor: 'operations'!
{Stepper} bundleStepper: region {XnRegion} with: order {OrderSpec}
	"Return a stepper of bundles according to the order."
	
	self subclassResponsibility!
*/
}
public OrglRoot combine(OrglRoot orgl) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9706:OrglRoot methodsFor: 'operations'!
{OrglRoot} combine: orgl {OrglRoot} 
	self subclassResponsibility!
*/
}
public OrglRoot copy(XnRegion externalRegion) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9710:OrglRoot methodsFor: 'operations'!
{OrglRoot} copy: externalRegion {XnRegion} 
	self subclassResponsibility!
*/
}
/**
 * This does the 'now' part of setting up a recorder, once the 'later' part has been set up.
 * It does a walk south on the O-tree, then walks back north on all the H-trees, filtered by
 * the Bert canopy.
 */
public void delayedFindMatching(PropFinder finder, RecorderFossil fossil, ResultRecorder recorder) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9714:OrglRoot methodsFor: 'operations'!
{void} delayedFindMatching: finder {PropFinder}
	with: fossil {RecorderFossil}
	with: recorder {ResultRecorder}
	
	"This does the 'now' part of setting up a recorder, once the 'later' part has been set up.
	 It does a walk south on the O-tree, then walks back north on all the H-trees, filtered by the Bert canopy."
	
	self subclassResponsibility!
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
udanax-top.st:9723:OrglRoot methodsFor: 'operations'!
{void} storeRecordingAgents: recorder {RecorderFossil}
	with: agenda {Agenda}
	"Go ahead and actually store the recorder in the sensor canopy.  However, instead of propogating the props immediately, accumulate all those agenda items into the 'agenda' parameter.  This is done instead of scheduling them directly because our client needs to schedule something else following all the prop propogation."
	
	self subclassResponsibility!
*/
}
/**
 * Return a copy with externalDsp added to the receiver's dsp.
 */
public OrglRoot transformedBy(Dsp externalDsp) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9729:OrglRoot methodsFor: 'operations'!
{OrglRoot} transformedBy: externalDsp {Dsp}
	"Return a copy with externalDsp added to the receiver's dsp."
	self subclassResponsibility!
*/
}
/**
 * Return a copy with externalDsp removed from the receiver's dsp.
 */
public OrglRoot unTransformedBy(Dsp externalDsp) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:9734:OrglRoot methodsFor: 'operations'!
{OrglRoot} unTransformedBy: externalDsp {Dsp}
	"Return a copy with externalDsp removed from the receiver's dsp."
	self subclassResponsibility!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(3);
	try {
		super.dismantle();
		myHCrum = null;
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:9741:OrglRoot methodsFor: 'protected:'!
{void} dismantle
	DiskManager consistent: 3 with:
		[super dismantle.
		myHCrum _ NULL]!
*/
}
public OrglRoot(SensorCrum scrum) {
	super(scrum);
	myHCrum = HBottomCrum.make();
/*
udanax-top.st:9748:OrglRoot methodsFor: 'create'!
create: scrum {SensorCrum | NULL}
	super create: scrum.
	myHCrum _ HBottomCrum make.!
*/
}
public int contentsHash() {
	return super.contentsHash() ^ myHCrum.hashForEqual();
/*
udanax-top.st:9754:OrglRoot methodsFor: 'testing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: myHCrum hashForEqual!
*/
}
/**
 * @deprecated
 */
public ScruTable asDataTable() {
	throw new PasseException();
/*
udanax-top.st:9761:OrglRoot methodsFor: 'smalltalk: passe'!
{ScruTable} asDataTable
	self passe!
*/
}
/**
 * @deprecated
 */
public ScruTable asTable() {
	throw new PasseException();
/*
udanax-top.st:9765:OrglRoot methodsFor: 'smalltalk: passe'!
{ScruTable} asTable
	self passe!
*/
}
/**
 * @deprecated
 */
public void checkRecorders(BeEdition edition, PropFinder finder, SensorCrum scrum) {
	throw new PasseException();
/*
udanax-top.st:9769:OrglRoot methodsFor: 'smalltalk: passe'!
{void} checkRecorders: edition {BeEdition} 
	with: finder {PropFinder} 
	with: scrum {SensorCrum | NULL}
self passe "fewer args"!
*/
}
/**
 * @deprecated
 */
public void delayedFindMatching(PropFinder finder, RecorderFossil recorder) {
	throw new PasseException();
/*
udanax-top.st:9774:OrglRoot methodsFor: 'smalltalk: passe'!
{void} delayedFindMatching: finder {PropFinder}
	with: recorder {RecorderFossil}
	
	self passe "extra argument"!
*/
}
/**
 * @deprecated
 */
public FeRangeElement fetch(Position key) {
	throw new PasseException();
/*
udanax-top.st:9779:OrglRoot methodsFor: 'smalltalk: passe'!
{FeRangeElement | NULL} fetch: key {Position}
	self passe!
*/
}
/**
 * @deprecated
 */
public ScruTable findMatching(PropFinder finder) {
	throw new PasseException();
/*
udanax-top.st:9783:OrglRoot methodsFor: 'smalltalk: passe'!
{ScruTable of: ID and: BeEdition} findMatching: finder {PropFinder}
	self passe!
*/
}
/**
 * @deprecated
 */
public void inform(Position key, HRoot value) {
	throw new PasseException();
/*
udanax-top.st:9787:OrglRoot methodsFor: 'smalltalk: passe'!
{void} inform: key {Position} with: value {HRoot}
	
	self passe!
*/
}
/**
 * @deprecated
 */
public void introduceStamp(BeEdition stamp) {
	throw new PasseException();
/*
udanax-top.st:9791:OrglRoot methodsFor: 'smalltalk: passe'!
{void} introduceStamp: stamp {BeEdition}
	self passe.!
*/
}
/**
 * @deprecated
 */
public void propChanged(PropChange change) {
	throw new PasseException();
/*
udanax-top.st:9794:OrglRoot methodsFor: 'smalltalk: passe'!
{void} propChanged: change {PropChange}
	self passe!
*/
}
/**
 * @deprecated
 */
public void removeStamp(BeEdition stamp) {
	throw new PasseException();
/*
udanax-top.st:9798:OrglRoot methodsFor: 'smalltalk: passe'!
{void} removeStamp: stamp {BeEdition}
	self passe.!
*/
}
/**
 * @deprecated
 */
public void wait(XnSensor sensor) {
	throw new PasseException();
/*
udanax-top.st:9801:OrglRoot methodsFor: 'smalltalk: passe'!
{void} wait: sensor {XnSensor}
	
	self passe!
*/
}
public OrglRoot(Rcvr receiver) {
	super(receiver);
	myHCrum = (HBottomCrum) receiver.receiveHeaper();
/*
udanax-top.st:9807:OrglRoot methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myHCrum _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myHCrum);
/*
udanax-top.st:9811:OrglRoot methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myHCrum.!
*/
}
/**
 * create a new orgl root
 */
public static OrglRoot makeCoordinateSpace(CoordinateSpace cs) {
	/* This should definitely be cached!!  We make them all the time probably. */
	Someone.thingToDo();
	AboraBlockSupport.enterConsistent(4);
	try {
		return new EmptyOrglRoot(cs);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:9824:OrglRoot class methodsFor: 'creation'!
make.CoordinateSpace: cs {CoordinateSpace}
	"create a new orgl root"
	"This should definitely be cached!!  We make them all the time probably."
	self thingToDo.
	DiskManager consistent: 4 with:
		[^EmptyOrglRoot create: cs]!
*/
}
public static OrglRoot makeXnRegion(XnRegion region) {
	if (region.isEmpty()) {
		return OrglRoot.make(region.coordinateSpace());
	}
	return ActualOrglRoot.make((Loaf.makeXnRegion(region)), region);
/*
udanax-top.st:9832:OrglRoot class methodsFor: 'creation'!
make.XnRegion: region {XnRegion}
	region isEmpty ifTrue:
		[^OrglRoot make: region coordinateSpace].
	^ActualOrglRoot make: (Loaf make.XnRegion: region) with: region!
*/
}
public static OrglRoot make(XnRegion keys, OrderSpec ordering, PtrArray values) {
	Stepper stepper;
	OrglRoot result;
	int i;
	result = OrglRoot.makeCoordinateSpace(ordering.coordinateSpace());
	Someone.hack();
	/* This should make a balanced tree directly. */
	i = 0;
	stepper = keys.stepper(ordering);
	Stepper stomper = stepper;
	for (; stomper.hasValue(); stomper.step()) {
		Position key = (Position) stomper.fetch();
		if (key == null) {
			continue ;
		}
		BeCarrier element;
		XnRegion region;
		FeRangeElement fe = (FeRangeElement) (values.fetch(i));
		if (fe != null ) {
			element = fe.carrier();
		}
		else {
			throw new AboraRuntimeException(AboraRuntimeException.MUST_NOT_HAVE_NULL_ELEMENTS);
		}
		region = key.asRegion();
		result = result.combine((ActualOrglRoot.make((Loaf.makeRegion(region, element)), region)));
		i = i + 1;
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:9838:OrglRoot class methodsFor: 'creation'!
{OrglRoot} make: keys {XnRegion}
	with: ordering {OrderSpec}
	with: values {PtrArray of: FeRangeElement}
	| stepper {Stepper} result {OrglRoot} i {Int32} |
	result _ OrglRoot make.CoordinateSpace: ordering coordinateSpace.
	self hack.   "This should make a balanced tree directly."
	i _ Int32Zero.
	stepper _ keys stepper: ordering.
	stepper forEach: 
		[:key {Position} |
		| element {BeCarrier} region {XnRegion} |
		(values fetch: i) 
			notNULL: [:fe {FeRangeElement} | element _ fe carrier]
			else: [Heaper BLAST: #MustNotHaveNullElements].
		region _ key asRegion.
		result _ result combine: 
					(ActualOrglRoot 
						make: (Loaf make.Region: region with: element)
						with: region).
		i _ i + 1].
	^result!
*/
}
/**
 * Make an Orgl from a bunch of Data. The data is
 * guaranteed to be of a reasonable size.
 */
public static OrglRoot makeData(PrimDataArray values, Arrangement arrangement) {
	return ActualOrglRoot.make((Loaf.make(values, arrangement)), arrangement.region());
/*
udanax-top.st:9861:OrglRoot class methodsFor: 'creation'!
{OrglRoot} makeData: values {PrimDataArray} with: arrangement {Arrangement}
	"Make an Orgl from a bunch of Data. The data is 
	guaranteed to be of a reasonable size."
	^ActualOrglRoot 
		make: (Loaf make: values with: arrangement)
		with: arrangement region!
*/
}
/**
 * Make an Orgl from a bunch of Data. The data is
 * guaranteed to be of a reasonable size.
 */
public static OrglRoot makeData(XnRegion keys, OrderSpec ordering, PrimDataArray values) {
	return ActualOrglRoot.make((Loaf.make(values, (ordering.arrange(keys)))), keys);
/*
udanax-top.st:9869:OrglRoot class methodsFor: 'creation'!
{OrglRoot} makeData: keys {XnRegion} with: ordering {OrderSpec} with: values {PrimDataArray} 
	"Make an Orgl from a bunch of Data. The data is 
	guaranteed to be of a reasonable size."
	^ActualOrglRoot 
		make: (Loaf make: values with: (ordering arrange: keys))
		with: keys!
*/
}
/**
 * create a new orgl root
 */
public static OrglRoot make(Heaper it) {
	if (it instanceof CoordinateSpace) {
		return makeCoordinateSpace((CoordinateSpace) it);
	}
	if (it instanceof XnRegion) {
		return makeXnRegion((XnRegion) it);
	}
	throw new UnsupportedOperationException();
/*
udanax-top.st:9879:OrglRoot class methodsFor: 'smalltalk:'!
{OrglRoot} make: it {Heaper}
	"create a new orgl root"
	(it isKindOf: CoordinateSpace) ifTrue: [^self make.CoordinateSpace: it].
	(it isKindOf: XnRegion) ifTrue: [^self make.XnRegion: it].
	^self make.ScruTable: (it cast: ScruTable)!
*/
}
public OrglRoot() {
/*

Generated during transformation
*/
}
}
