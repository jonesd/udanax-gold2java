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
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.ents.ActualOrglRoot;
import info.dgjones.abora.gold.be.ents.HUpperCrum;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.OExpandingLoaf;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.XnSensor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

/**
 * NOT.A.TYPE
 */
public class OExpandingLoaf extends Loaf {

	protected XnRegion myRegion;
/*
udanax-top.st:8555:
Loaf subclass: #OExpandingLoaf
	instanceVariableNames: 'myRegion {XnRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:8559:
OExpandingLoaf comment:
' NOT.A.TYPE'!
*/
/*
udanax-top.st:8561:
(OExpandingLoaf getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #COPY; add: #DEFERRED; add: #SHEPHERD.ANCESTOR; add: #DEFERRED.LOCKED; add: #(MAY.BECOME SplitLoaf ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OExpandingLoaf.class).setAttributes( new Set().add("COPY").add("DEFERRED").add("SHEPHERDANCESTOR").add("DEFERREDLOCKED").add( new String[]
	{"MAYBECOME", "SplitLoaf"}));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Return a stepper of bundles according to the order.
 */
public Stepper bundleStepper(XnRegion region, OrderSpec order, Dsp globalDsp) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:8566:OExpandingLoaf methodsFor: 'operations'!
{Stepper} bundleStepper: region {XnRegion} with: order {OrderSpec} with: globalDsp {Dsp}
	"Return a stepper of bundles according to the order."
	
	self subclassResponsibility!
*/
}
/**
 * Accumulate dsp downward.
 */
public OrglRoot combine(ActualOrglRoot another, XnRegion limitRegion, Dsp globalDsp) {
	XnRegion myGlobalRegion;
	ActualOrglRoot result;
	OrglRoot him;
	myGlobalRegion = (globalDsp.ofAll(myRegion));
	if ( ! ((another.copy(myGlobalRegion)).isEmpty())) {
		throw new AboraRuntimeException(AboraRuntimeException.INTERSECTING_COMBINE);
	}
	result = ActualOrglRoot.make((transformedBy(globalDsp)), myGlobalRegion);
	him = another;
	Stepper stomper = myGlobalRegion.distinctions().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion split = (XnRegion) stomper.fetch();
		if (split == null) {
			continue ;
		}
		OrglRoot hisOut;
		hisOut = him.copy(split.complement());
		if ( ! (hisOut.isEmpty())) {
			result = result.makeNew(split, result, ((ActualOrglRoot) hisOut));
			him = another.copy(split);
		}
	}
	stomper.destroy();
	if ( ! (him.isEmpty())) {
		throw new AboraRuntimeException(AboraRuntimeException.COMBINE_LOOP_FAILED);
	}
	return result;
/*
udanax-top.st:8571:OExpandingLoaf methodsFor: 'operations'!
{OrglRoot} combine: another {ActualOrglRoot} with: limitRegion {XnRegion unused} with: globalDsp {Dsp}
	"Accumulate dsp downward."
	
	| myGlobalRegion {XnRegion} result {ActualOrglRoot} him {OrglRoot} |
	myGlobalRegion _ (globalDsp ofAll: myRegion).
	(another copy: myGlobalRegion) isEmpty ifFalse: [Heaper BLAST: #IntersectingCombine].
	result _ ActualOrglRoot make: (self transformedBy: globalDsp) with: myGlobalRegion.
	him _ another.
	[ScruSet] USES.
	myGlobalRegion distinctions stepper forEach: 
		[:split {XnRegion} |
		| hisOut {OrglRoot} |
		hisOut _ him copy: split complement.
		hisOut isEmpty ifFalse: 
			[result _ result makeNew: split with: result with: (hisOut cast: ActualOrglRoot). 
			him _ another copy: split]].
	him isEmpty ifFalse: [Heaper BLAST: #CombineLoopFailed].
	^result!
*/
}
public void informTo(OrglRoot orgl) {
	throw new UnimplementedException();
/*
udanax-top.st:8590:OExpandingLoaf methodsFor: 'operations'!
{void} informTo: orgl {OrglRoot unused}
	self unimplemented!
*/
}
public boolean isPartial() {
	return false;
/*
udanax-top.st:8593:OExpandingLoaf methodsFor: 'operations'!
{Boolean} isPartial
	^false!
*/
}
/**
 * Make each child completely contained or completely outside
 * the region. Return the number of children completely in the region.
 * Handle the containment cases using myRegion.
 */
public int splay(XnRegion region, XnRegion limitRegion) {
	if (myRegion.isSubsetOf(region)) {
		return 2;
	}
	else {
		if (myRegion.intersects(region)) {
			return actualSplay(region, limitRegion);
		}
		else {
			return 0;
		}
	}
/*
udanax-top.st:8596:OExpandingLoaf methodsFor: 'operations'!
{UInt8} splay: region {XnRegion} with: limitRegion {XnRegion} 
	"Make each child completely contained or completely outside 
	the region. Return the number of children completely in the region. 
	Handle the containment cases using myRegion."
	(myRegion isSubsetOf: region)
		ifTrue: [^2]
		ifFalse: [(myRegion intersects: region)
				ifTrue: [^self actualSplay: region with: limitRegion]
				ifFalse: [^Int0]]!
*/
}
public XnRegion attachTrailBlazer(TrailBlazer blazer) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:8609:OExpandingLoaf methodsFor: 'backfollow'!
{XnRegion} attachTrailBlazer: blazer {TrailBlazer}
	
	self subclassResponsibility!
*/
}
/**
 * send checkRecorders to all children
 */
public void checkChildRecorders(PropFinder finder) {
/*
udanax-top.st:8613:OExpandingLoaf methodsFor: 'backfollow'!
{void} checkChildRecorders: finder {PropFinder}
	"send checkRecorders to all children"!
*/
}
public void checkTrailBlazer(TrailBlazer blazer) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:8616:OExpandingLoaf methodsFor: 'backfollow'!
{void} checkTrailBlazer: blazer {TrailBlazer}
	
	self subclassResponsibility!
*/
}
/**
 * Default south-to-north turnaround point during 'now' part of backfollow (which is
 * leafward, then rootward, in the H-tree, filtered by the Bert canopy).  (Sometimes
 * overridden).
 * (OExpandingLoaf is the supercalss of all O-tree leaf types.)
 */
public void delayedStoreMatching(PropFinder finder, RecorderFossil fossil, ResultRecorder recorder, HashSetCache hCrumCache) {
	hCrum().delayedStoreBackfollow(finder, fossil, recorder, hCrumCache);
/*
udanax-top.st:8620:OExpandingLoaf methodsFor: 'backfollow'!
{void} delayedStoreMatching: finder {PropFinder} 
	with: fossil {RecorderFossil} 
	with: recorder {ResultRecorder}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	"Default south-to-north turnaround point during 'now' part of backfollow (which is leafward, then rootward, in the H-tree, filtered by the Bert canopy).  (Sometimes overridden).
	(OExpandingLoaf is the supercalss of all O-tree leaf types.)"
	
	self hCrum delayedStoreBackfollow: finder with: fossil with: recorder with: hCrumCache!
*/
}
public TrailBlazer fetchTrailBlazer() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:8630:OExpandingLoaf methodsFor: 'backfollow'!
{TrailBlazer | NULL} fetchTrailBlazer
	
	self subclassResponsibility!
*/
}
public void storeRecordingAgents(RecorderFossil recorder, Agenda agenda) {
	agenda.registerItem((sensorCrum().recordingAgent(recorder)));
/*
udanax-top.st:8634:OExpandingLoaf methodsFor: 'backfollow'!
{void} storeRecordingAgents: recorder {RecorderFossil}
	with: agenda {Agenda}  
	
	agenda registerItem: (self sensorCrum recordingAgent: recorder)!
*/
}
public void triggerDetector(FeFillRangeDetector detect) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:8639:OExpandingLoaf methodsFor: 'backfollow'!
{void} triggerDetector: detect {FeFillRangeDetector}
	
	self subclassResponsibility!
*/
}
/**
 * return a mapping from my data to corresponding stuff in the given trace
 */
public Mapping compare(TracePosition trace, XnRegion region) {
	return hCrum().mappingTo(trace, (region.coordinateSpace().identityDsp().restrict(region)));
/*
udanax-top.st:8645:OExpandingLoaf methodsFor: 'accessing'!
{Mapping} compare: trace {TracePosition} with: region {XnRegion}
	"return a mapping from my data to corresponding stuff in the given trace"
	^self hCrum mappingTo: trace with: (region coordinateSpace identityDsp restrict: region)!
*/
}
public int count() {
	return myRegion.count();
/*
udanax-top.st:8649:OExpandingLoaf methodsFor: 'accessing'!
{IntegerVar} count
	^myRegion count!
*/
}
public XnRegion domain() {
	return myRegion;
/*
udanax-top.st:8652:OExpandingLoaf methodsFor: 'accessing'!
{XnRegion} domain
	^myRegion!
*/
}
public FeRangeElement fetch(Position key, BeEdition edition, Position globalKey) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:8655:OExpandingLoaf methodsFor: 'accessing'!
{FeRangeElement | NULL} fetch: key {Position} with: edition {BeEdition} with: globalKey {Position}
	self subclassResponsibility!
*/
}
/**
 * I'm at the bottom.
 */
public OExpandingLoaf fetchBottomAt(Position key) {
	return this;
/*
udanax-top.st:8658:OExpandingLoaf methodsFor: 'accessing'!
{OExpandingLoaf} fetchBottomAt: key {Position}
	"I'm at the bottom."
	^self!
*/
}
/**
 * Fill an array with my contents
 */
public void fill(XnRegion keys, Arrangement toArrange, PrimArray toArray, Dsp globalDsp, BeEdition edition) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:8663:OExpandingLoaf methodsFor: 'accessing'!
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
udanax-top.st:8672:OExpandingLoaf methodsFor: 'accessing'!
{BeRangeElement} getBe: key {Position}
	"Get or Make the BeRangeElement at the location."
		
	self subclassResponsibility!
*/
}
/**
 * This gets overridden by RegionLoaf.
 */
public XnRegion keysLabelled(BeLabel label) {
	return domain().coordinateSpace().emptyRegion();
/*
udanax-top.st:8677:OExpandingLoaf methodsFor: 'accessing'!
{XnRegion} keysLabelled: label {BeLabel}
	"This gets overridden by RegionLoaf."
	
	^self domain coordinateSpace emptyRegion!
*/
}
/**
 * Return the owner of the atoms represented by the receiver.
 */
public ID owner() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:8682:OExpandingLoaf methodsFor: 'accessing'!
{ID} owner
	"Return the owner of the atoms represented by the receiver."
	
	self subclassResponsibility!
*/
}
public XnRegion rangeOwners(XnRegion positions) {
	if (positions == null || (myRegion.intersects(positions))) {
		return owner().asRegion();
	}
	else {
		return owner().coordinateSpace().emptyRegion();
	}
/*
udanax-top.st:8687:OExpandingLoaf methodsFor: 'accessing'!
{XnRegion} rangeOwners: positions {XnRegion | NULL} 
	(positions == NULL or: [myRegion intersects: positions])
		ifTrue: [^self owner asRegion]
		ifFalse: [^self owner coordinateSpace emptyRegion]!
*/
}
/**
 * If the CurrentKeyMaster includes the owner of this loaf
 * then change the owner and return NULL
 * else just return self.
 */
public OrglRoot setAllOwners(ID owner) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:8692:OExpandingLoaf methodsFor: 'accessing'!
{OrglRoot} setAllOwners: owner {ID} 
	"If the CurrentKeyMaster includes the owner of this loaf
		then change the owner and return NULL
		else just return self."
		
	self subclassResponsibility!
*/
}
/**
 * Return a region describing the stuff that can backfollow to trace.
 */
public XnRegion sharedRegion(TracePosition trace, XnRegion limitRegion) {
	if (hCrum().inTrace(trace)) {
		return myRegion;
	}
	else {
		return myRegion.coordinateSpace().emptyRegion();
	}
/*
udanax-top.st:8699:OExpandingLoaf methodsFor: 'accessing'!
{XnRegion} sharedRegion: trace {TracePosition} with: limitRegion {XnRegion unused}
	"Return a region describing the stuff that can backfollow to trace."
	(self hCrum inTrace: trace)
		ifTrue: [^myRegion]
		ifFalse: [^myRegion coordinateSpace emptyRegion]!
*/
}
/**
 * Return the PrimSpec that describes the representation of the data.
 */
public PrimSpec spec() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:8705:OExpandingLoaf methodsFor: 'accessing'!
{PrimSpec} spec
	"Return the PrimSpec that describes the representation of the data."
	
	self subclassResponsibility!
*/
}
public XnRegion usedDomain() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:8710:OExpandingLoaf methodsFor: 'accessing'!
{XnRegion} usedDomain
	self subclassResponsibility!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print(getAboraClass().name());
	aStream.print("(");
	aStream.print(myRegion);
	aStream.print(")");
/*
udanax-top.st:8715:OExpandingLoaf methodsFor: 'printing'!
{void} printOn: aStream {ostream reference}
	aStream << self getCategory name << '(' << myRegion << ')'!
*/
}
/**
 * Return an Inner loaf which is an expansion of me.  The area in the region must go
 * into the leftCrum of my substitute, or the splay algorithm will fail!!  implementations
 * must call diskUpdate.
 */
public int actualSplay(XnRegion region, XnRegion limitRegion) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:8720:OExpandingLoaf methodsFor: 'protected: splay'!
{Int8} actualSplay: region {XnRegion unused} with: limitRegion {XnRegion unused}
	"Return an Inner loaf which is an expansion of me.  The area in the region must go
	 into the leftCrum of my substitute, or the splay algorithm will fail!!  implementations
	 must call diskUpdate."
	self subclassResponsibility!
*/
}
public OExpandingLoaf(XnRegion region) {
	super(null, null);
	if ( ! ( ! region.isEmpty())) {
		throw new AboraAssertionException();
	}
	myRegion = region;
/*
udanax-top.st:8729:OExpandingLoaf methodsFor: 'create'!
create: region {XnRegion}
	super create: NULL with: NULL.
region isEmpty not assert.
	myRegion _ region.!
*/
}
public OExpandingLoaf(XnRegion region, HUpperCrum hcrum, SensorCrum sensor) {
	super(hcrum, sensor);
	if ( ! ( ! region.isEmpty())) {
		throw new AboraAssertionException();
	}
	myRegion = region;
/*
udanax-top.st:8734:OExpandingLoaf methodsFor: 'create'!
create: region {XnRegion} with: hcrum {HUpperCrum | NULL} with: sensor {SensorCrum}
	super create: hcrum with: sensor.
region isEmpty not assert.
	myRegion _ region.!
*/
}
public OExpandingLoaf(int hash, XnRegion region, HUpperCrum hcrum, SensorCrum sensor) {
	super(hash, hcrum, sensor);
	if ( ! ( ! region.isEmpty())) {
		throw new AboraAssertionException();
	}
	myRegion = region;
/*
udanax-top.st:8739:OExpandingLoaf methodsFor: 'create'!
create: hash {UInt32} with: region {XnRegion} with: hcrum {HUpperCrum} with: sensor {SensorCrum}
	super create: hash with: hcrum with: sensor.
region isEmpty not assert.
	myRegion _ region.!
*/
}
public int contentsHash() {
	return super.contentsHash() ^ myRegion.hashForEqual();
/*
udanax-top.st:8746:OExpandingLoaf methodsFor: 'testing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: myRegion hashForEqual!
*/
}
/*
udanax-top.st:8753:OExpandingLoaf methodsFor: 'smalltalk:'!
crums
	^#()!
*/
public String displayString() {
	return "\"" + myRegion.printString() + "\"";
/*
udanax-top.st:8756:OExpandingLoaf methodsFor: 'smalltalk:'!
displayString
	^'"' , myRegion printString , '"'!
*/
}
/*
udanax-top.st:8759:OExpandingLoaf methodsFor: 'smalltalk:'!
inspect
	self basicInspect!
*/
/**
 * @deprecated
 */
public void wait(XnSensor sensor) {
	throw new PasseException();
/*
udanax-top.st:8764:OExpandingLoaf methodsFor: 'smalltalk: passe'!
{void} wait: sensor {XnSensor}
	
	self passe!
*/
}
public OExpandingLoaf(Rcvr receiver) {
	super(receiver);
	myRegion = (XnRegion) receiver.receiveHeaper();
/*
udanax-top.st:8770:OExpandingLoaf methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myRegion _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myRegion);
/*
udanax-top.st:8774:OExpandingLoaf methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myRegion.!
*/
}
public OExpandingLoaf() {
/*

Generated during transformation
*/
}
}
