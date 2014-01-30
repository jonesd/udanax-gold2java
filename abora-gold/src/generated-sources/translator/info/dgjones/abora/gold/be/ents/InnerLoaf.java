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
import info.dgjones.abora.gold.be.ents.DspLoaf;
import info.dgjones.abora.gold.be.ents.HUpperCrum;
import info.dgjones.abora.gold.be.ents.InnerLoaf;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.OExpandingLoaf;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.be.ents.SplitLoaf;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
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
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class InnerLoaf extends Loaf {

/*
udanax-top.st:7676:
Loaf subclass: #InnerLoaf
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:7680:
(InnerLoaf getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.ANCESTOR; add: #DEFERRED.LOCKED; yourself)!
*/
/*
udanax-top.st:7823:
InnerLoaf class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:7826:
(InnerLoaf getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.ANCESTOR; add: #DEFERRED.LOCKED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(InnerLoaf.class).setAttributes( new Set().add("DEFERRED").add("COPY").add("SHEPHERDANCESTOR").add("DEFERREDLOCKED"));
/*

Generated during transformation: AddMethod
*/
}
public InnerLoaf(HUpperCrum hcrum, SensorCrum scrum) {
	super(hcrum, scrum);
/*
udanax-top.st:7685:InnerLoaf methodsFor: 'create'!
create: hcrum {HUpperCrum} with: scrum {SensorCrum}
	super create: hcrum with: scrum!
*/
}
public InnerLoaf(int hash, HUpperCrum hcrum, SensorCrum scrum) {
	super(hash, hcrum, scrum);
/*
udanax-top.st:7688:InnerLoaf methodsFor: 'create'!
create: hash {UInt32} with: hcrum {HUpperCrum} with: scrum {SensorCrum}
	super create: hash with: hcrum with: scrum!
*/
}
/**
 * Special handle the splay cases in which the region partially intersects
 * with limitedRegion.  These require rotations and splitting.
 */
public int actualSplay(XnRegion region, XnRegion limitRegion) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7693:InnerLoaf methodsFor: 'protected: splay'!
{Int8} actualSplay: region {XnRegion} with: limitRegion {XnRegion} 
	"Special handle the splay cases in which the region partially intersects
	 with limitedRegion.  These require rotations and splitting."
	 
	 self subclassResponsibility!
*/
}
/**
 * return a mapping from my data to corresponding stuff in the given trace
 */
public Mapping compare(TracePosition trace, XnRegion region) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7701:InnerLoaf methodsFor: 'accessing'!
{Mapping} compare: trace {TracePosition} with: region {XnRegion}
	"return a mapping from my data to corresponding stuff in the given trace"
	self subclassResponsibility!
*/
}
public int count() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7705:InnerLoaf methodsFor: 'accessing'!
{IntegerVar} count
	self subclassResponsibility!
*/
}
public XnRegion domain() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7708:InnerLoaf methodsFor: 'accessing'!
{XnRegion} domain
	self subclassResponsibility!
*/
}
public FeRangeElement fetch(Position key, BeEdition edition, Position globalKey) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7711:InnerLoaf methodsFor: 'accessing'!
{FeRangeElement | NULL} fetch: key {Position} with: edition {BeEdition} with: globalKey {Position}
	self subclassResponsibility!
*/
}
/**
 * Return the bottom-most Loaf.  Used to get the owner and such of a position.
 */
public OExpandingLoaf fetchBottomAt(Position key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7714:InnerLoaf methodsFor: 'accessing'!
{OExpandingLoaf} fetchBottomAt: key {Position}
	"Return the bottom-most Loaf.  Used to get the owner and such of a position."
	self subclassResponsibility!
*/
}
public void fill(XnRegion keys, Arrangement toArrange, PrimArray toArray, Dsp globalDsp, BeEdition edition) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7719:InnerLoaf methodsFor: 'accessing'!
{void} fill: keys {XnRegion}
	with: toArrange {Arrangement}
	with: toArray {PrimArray}
	with: globalDsp {Dsp}
	with: edition {BeEdition}
	self subclassResponsibility!
*/
}
/**
 * Get or Make the BeRangeElement at the location.
 */
public BeRangeElement getBe(Position key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7727:InnerLoaf methodsFor: 'accessing'!
{BeRangeElement} getBe: key {Position}
	"Get or Make the BeRangeElement at the location."
		
	self subclassResponsibility!
*/
}
/**
 * This is used by the splay algorithms.
 */
public Loaf inPart() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7732:InnerLoaf methodsFor: 'accessing'!
{Loaf} inPart
	"This is used by the splay algorithms."
	self subclassResponsibility!
*/
}
/**
 * This is used by the splay algorithms.
 */
public Loaf outPart() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7736:InnerLoaf methodsFor: 'accessing'!
{Loaf} outPart
	"This is used by the splay algorithms."
	self subclassResponsibility!
*/
}
public XnRegion rangeOwners(XnRegion positions) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7740:InnerLoaf methodsFor: 'accessing'!
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
udanax-top.st:7744:InnerLoaf methodsFor: 'accessing'!
{OrglRoot} setAllOwners: owner {ID}
	"Recur assigning owners.  Return the portion of the o-tree that
	 couldn't be assigned, or NULL if it was all assigned."
		
	self subclassResponsibility!
*/
}
public XnRegion usedDomain() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7750:InnerLoaf methodsFor: 'accessing'!
{XnRegion} usedDomain
	self subclassResponsibility!
*/
}
public XnRegion attachTrailBlazer(TrailBlazer blazer) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7755:InnerLoaf methodsFor: 'backfollow'!
{XnRegion} attachTrailBlazer: blazer {TrailBlazer}
	
	self subclassResponsibility!
*/
}
public void checkChildRecorders(PropFinder finder) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7759:InnerLoaf methodsFor: 'backfollow'!
{void} checkChildRecorders: finder {PropFinder}
	self subclassResponsibility!
*/
}
public void checkTrailBlazer(TrailBlazer blazer) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7763:InnerLoaf methodsFor: 'backfollow'!
{void} checkTrailBlazer: blazer {TrailBlazer}
	
	self subclassResponsibility!
*/
}
/**
 * Inner loaf:  Just forward south to all children.
 */
public void delayedStoreMatching(PropFinder finder, RecorderFossil fossil, ResultRecorder recorder, HashSetCache hCrumCache) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7767:InnerLoaf methodsFor: 'backfollow'!
{void} delayedStoreMatching: finder {PropFinder} 
	with: fossil {RecorderFossil} 
	with: recorder {ResultRecorder}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	"Inner loaf:  Just forward south to all children."
	
	self subclassResponsibility!
*/
}
public TrailBlazer fetchTrailBlazer() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7776:InnerLoaf methodsFor: 'backfollow'!
{TrailBlazer | NULL} fetchTrailBlazer
	
	self subclassResponsibility!
*/
}
public void storeRecordingAgents(RecorderFossil recorder, Agenda agenda) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7780:InnerLoaf methodsFor: 'backfollow'!
{void} storeRecordingAgents: recorder {RecorderFossil}
	with: agenda {Agenda}
	
	self subclassResponsibility!
*/
}
public void triggerDetector(FeFillRangeDetector detect) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7785:InnerLoaf methodsFor: 'backfollow'!
{void} triggerDetector: detect {FeFillRangeDetector}
	
	self subclassResponsibility!
*/
}
/**
 * Return a stepper of bundles according to the order.
 */
public Stepper bundleStepper(XnRegion region, OrderSpec order, Dsp globalDsp) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7791:InnerLoaf methodsFor: 'operations'!
{Stepper} bundleStepper: region {XnRegion} with: order {OrderSpec} with: globalDsp {Dsp}
	"Return a stepper of bundles according to the order."
	
	self subclassResponsibility!
*/
}
public OrglRoot combine(ActualOrglRoot another, XnRegion limitRegion, Dsp globalDsp) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7796:InnerLoaf methodsFor: 'operations'!
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
udanax-top.st:7799:InnerLoaf methodsFor: 'operations'!
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
udanax-top.st:7804:InnerLoaf methodsFor: 'operations'!
{XnRegion} sharedRegion: trace {TracePosition} with: limitRegion {XnRegion}
	"Return a region describing the stuff that can backfollow to trace."
	self subclassResponsibility!
*/
}
/**
 * @deprecated
 */
public void wait(XnSensor sensor) {
	throw new PasseException();
/*
udanax-top.st:7810:InnerLoaf methodsFor: 'smalltalk: passe'!
{void} wait: sensor {XnSensor}
	
	self passe!
*/
}
public InnerLoaf(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:7816:InnerLoaf methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:7819:InnerLoaf methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
/**
 * Make a loaf that transforms the contents of newO.
 */
public static InnerLoaf make(Loaf newO, Dsp dsp) {
	AboraBlockSupport.enterConsistent(11);
	try {
		return new DspLoaf(newO, dsp);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:7831:InnerLoaf class methodsFor: 'create'!
make: newO {Loaf} with: dsp {Dsp}
	"Make a loaf that transforms the contents of newO."
	DiskManager consistent: 11 with: 
		[^DspLoaf create: newO with: dsp]!
*/
}
/**
 * The contents of newIn must be completely contained in newSplit.
 * newOut must be completely outside newSplit.  Should this just
 * forward to make:with:with:with:?  This should extract shared dsp
 * from newIn and newOut.
 */
public static InnerLoaf make(XnRegion newSplit, Loaf newIn, Loaf newOut) {
	AboraBlockSupport.enterConsistent(-1);
	try {
		return new SplitLoaf(newSplit, newIn, newOut);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:7837:InnerLoaf class methodsFor: 'create'!
make: newSplit {XnRegion} with: newIn {Loaf} with: newOut {Loaf}
	"The contents of newIn must be completely contained in newSplit. 
	 newOut must be completely outside newSplit.  Should this just 
	 forward to make:with:with:with:?  This should extract shared dsp 
	 from newIn and newOut."
	DiskManager consistent: -1 with: 
		[^SplitLoaf create: newSplit with: newIn with: newOut]!
*/
}
/**
 * The contents of newIn must be completely contained in newSplit.
 * newOut must be completely outside newSplit
 */
public static InnerLoaf make(XnRegion newSplit, Loaf newIn, Loaf newOut, HUpperCrum hcrum) {
	AboraBlockSupport.enterConsistent(6);
	try {
		return new SplitLoaf(newSplit, newIn, newOut, hcrum);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:7846:InnerLoaf class methodsFor: 'create'!
make: newSplit {XnRegion} with: newIn {Loaf} with: newOut {Loaf} with: hcrum {HUpperCrum}
	"The contents of newIn must be completely contained in newSplit. 
	 newOut must be completely outside newSplit"
	DiskManager consistent: 6 with: 
		[^SplitLoaf create: newSplit with: newIn with: newOut with: hcrum]!
*/
}
public InnerLoaf() {
/*

Generated during transformation
*/
}
}
