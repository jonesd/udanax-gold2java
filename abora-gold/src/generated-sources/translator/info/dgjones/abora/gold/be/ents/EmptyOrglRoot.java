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
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.ents.EmptyOrglRoot;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.HRoot;
import info.dgjones.abora.gold.java.missing.IObject;
import info.dgjones.abora.gold.java.missing.XnSensor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class EmptyOrglRoot extends OrglRoot {

	protected CoordinateSpace myCS;
/*
udanax-top.st:10250:
OrglRoot subclass: #EmptyOrglRoot
	instanceVariableNames: 'myCS {CoordinateSpace}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:10254:
(EmptyOrglRoot getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #LOCKED; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(EmptyOrglRoot.class).setAttributes( new Set().add("SHEPHERDPATRIARCH").add("COPY").add("LOCKED").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public XnRegion attachTrailBlazer(TrailBlazer blazer) {
	return domain();
/*
udanax-top.st:10259:EmptyOrglRoot methodsFor: 'backfollow'!
{XnRegion} attachTrailBlazer: blazer {TrailBlazer}
	
	^self domain!
*/
}
public void checkRecorders(PropFinder finder, SensorCrum scrum) {
/*
udanax-top.st:10263:EmptyOrglRoot methodsFor: 'backfollow'!
{void} checkRecorders: finder {PropFinder} 
	with: scrum {SensorCrum | NULL}!
*/
}
public void checkTrailBlazer(TrailBlazer blazer) {
	throw new AboraRuntimeException(AboraRuntimeException.EMPTY_TRAIL);
/*
udanax-top.st:10266:EmptyOrglRoot methodsFor: 'backfollow'!
{void} checkTrailBlazer: blazer {TrailBlazer unused}
	
	Heaper BLAST: #EmptyTrail!
*/
}
public void delayedFindMatching(PropFinder finder, RecorderFossil fossil, ResultRecorder recorder) {
/*
udanax-top.st:10270:EmptyOrglRoot methodsFor: 'backfollow'!
{void} delayedFindMatching: finder {PropFinder}
	with: fossil {RecorderFossil}
	with: recorder {ResultRecorder}!
*/
}
public TrailBlazer fetchTrailBlazer() {
	return null;
/*
udanax-top.st:10274:EmptyOrglRoot methodsFor: 'backfollow'!
{TrailBlazer | NULL} fetchTrailBlazer
	^NULL!
*/
}
public void storeRecordingAgents(RecorderFossil recorder, Agenda agenda) {
/*
udanax-top.st:10278:EmptyOrglRoot methodsFor: 'backfollow'!
{void} storeRecordingAgents: recorder {RecorderFossil}
	with: agenda {Agenda}!
*/
}
public void triggerDetector(FeFillRangeDetector detect) {
/*
udanax-top.st:10281:EmptyOrglRoot methodsFor: 'backfollow'!
{void} triggerDetector: detect {FeFillRangeDetector}!
*/
}
/**
 * the kind of domain elements allowed
 */
public CoordinateSpace coordinateSpace() {
	return myCS;
/*
udanax-top.st:10285:EmptyOrglRoot methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	"the kind of domain elements allowed"
	^myCS!
*/
}
public int count() {
	return 0;
/*
udanax-top.st:10290:EmptyOrglRoot methodsFor: 'accessing'!
{IntegerVar} count
	^IntegerVar0!
*/
}
public XnRegion domain() {
	return myCS.emptyRegion();
/*
udanax-top.st:10293:EmptyOrglRoot methodsFor: 'accessing'!
{XnRegion} domain
	^myCS emptyRegion!
*/
}
public FeRangeElement fetch(Position key, BeEdition edition) {
	return null;
/*
udanax-top.st:10296:EmptyOrglRoot methodsFor: 'accessing'!
{FeRangeElement | NULL} fetch: key {Position} with: edition {BeEdition}
	^NULL!
*/
}
/**
 * Get or Make the BeRangeElement at the location.
 */
public BeRangeElement getBe(Position key) {
	throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
/*
udanax-top.st:10300:EmptyOrglRoot methodsFor: 'accessing'!
{BeRangeElement} getBe: key {Position}
	"Get or Make the BeRangeElement at the location."
	Heaper BLAST: #NotInTable.
	^NULL!
*/
}
public boolean isEmpty() {
	return true;
/*
udanax-top.st:10306:EmptyOrglRoot methodsFor: 'accessing'!
{BooleanVar} isEmpty
	
	^true!
*/
}
/**
 * Just search for now.
 */
public XnRegion keysLabelled(BeLabel label) {
	return myCS.emptyRegion();
/*
udanax-top.st:10310:EmptyOrglRoot methodsFor: 'accessing'!
{XnRegion} keysLabelled: label {BeLabel}
	"Just search for now."
	^myCS emptyRegion!
*/
}
/**
 * return a mapping from my data to corresponding stuff in the given trace
 */
public Mapping mapSharedTo(TracePosition trace) {
	return coordinateSpace().identityDsp();
/*
udanax-top.st:10315:EmptyOrglRoot methodsFor: 'accessing'!
{Mapping} mapSharedTo: trace {TracePosition unused}
	"return a mapping from my data to corresponding stuff in the given trace"
	^self coordinateSpace identityDsp!
*/
}
/**
 * Return the owner for the given position in the receiver.
 */
public ID ownerAt(Position key) {
	throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
/*
udanax-top.st:10319:EmptyOrglRoot methodsFor: 'accessing'!
{ID} ownerAt: key {Position}
	"Return the owner for the given position in the receiver."
	
	Heaper BLAST: #NotInTable.
	^NULL!
*/
}
public XnRegion rangeOwners(XnRegion positions) {
	return IDSpace.global().emptyRegion();
/*
udanax-top.st:10325:EmptyOrglRoot methodsFor: 'accessing'!
{XnRegion} rangeOwners: positions {XnRegion | NULL} 
	
	^IDSpace global emptyRegion!
*/
}
/**
 * There aren't any contents, so just return self.
 */
public OrglRoot setAllOwners(ID owner) {
	return this;
/*
udanax-top.st:10329:EmptyOrglRoot methodsFor: 'accessing'!
{OrglRoot} setAllOwners: owner {ID}
	"There aren't any contents, so just return self."
		
	^self!
*/
}
/**
 * I have no contents, so I can't shared anything.
 */
public XnRegion sharedRegion(TracePosition trace) {
	return myCS.emptyRegion();
/*
udanax-top.st:10334:EmptyOrglRoot methodsFor: 'accessing'!
{XnRegion} sharedRegion: trace {TracePosition unused}
	"I have no contents, so I can't shared anything."
	^ myCS emptyRegion!
*/
}
/**
 * Return a simple region that encloses the domain of the receiver.
 */
public XnRegion simpleDomain() {
	return myCS.emptyRegion();
/*
udanax-top.st:10339:EmptyOrglRoot methodsFor: 'accessing'!
{XnRegion} simpleDomain
	"Return a simple region that encloses the domain of the receiver."
	
	^ myCS emptyRegion!
*/
}
/**
 * Return the owner for the given position in the receiver.
 */
public PrimSpec specAt(Position key) {
	throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
/*
udanax-top.st:10344:EmptyOrglRoot methodsFor: 'accessing'!
{PrimSpec} specAt: key {Position}
	"Return the owner for the given position in the receiver."
	
	Heaper BLAST: #NotInTable.
	^NULL "fodder"!
*/
}
public XnRegion usedDomain() {
	return myCS.emptyRegion();
/*
udanax-top.st:10350:EmptyOrglRoot methodsFor: 'accessing'!
{XnRegion} usedDomain
	^myCS emptyRegion!
*/
}
/**
 * Return a stepper of bundles according to the order.
 */
public Stepper bundleStepper(XnRegion region, OrderSpec order) {
	return Stepper.emptyStepper();
/*
udanax-top.st:10355:EmptyOrglRoot methodsFor: 'operations'!
{Stepper} bundleStepper: region {XnRegion} with: order {OrderSpec}
	"Return a stepper of bundles according to the order."
	
	^Stepper emptyStepper!
*/
}
public OrglRoot combine(OrglRoot orgl) {
	return orgl;
/*
udanax-top.st:10360:EmptyOrglRoot methodsFor: 'operations'!
{OrglRoot} combine: orgl {OrglRoot} 
	^ orgl!
*/
}
public OrglRoot copy(XnRegion externalRegion) {
	return this;
/*
udanax-top.st:10364:EmptyOrglRoot methodsFor: 'operations'!
{OrglRoot} copy: externalRegion {XnRegion unused} 
	^ self!
*/
}
/**
 * Return a copy with externalDsp added to the receiver's dsp.
 */
public OrglRoot transformedBy(Dsp externalDsp) {
	return this;
/*
udanax-top.st:10368:EmptyOrglRoot methodsFor: 'operations'!
{OrglRoot} transformedBy: externalDsp {Dsp unused}
	"Return a copy with externalDsp added to the receiver's dsp."
	^ self!
*/
}
/**
 * Return a copy with externalDsp removed from the receiver's dsp.
 */
public OrglRoot unTransformedBy(Dsp externalDsp) {
	return this;
/*
udanax-top.st:10373:EmptyOrglRoot methodsFor: 'operations'!
{OrglRoot} unTransformedBy: externalDsp {Dsp unused}
	"Return a copy with externalDsp removed from the receiver's dsp."
	^ self!
*/
}
public EmptyOrglRoot(CoordinateSpace cs) {
	super(((SensorCrum) null));
	myCS = cs;
	newShepherd();
/*
udanax-top.st:10380:EmptyOrglRoot methodsFor: 'create'!
create: cs {CoordinateSpace}
	super create: (NULL basicCast: SensorCrum).
	myCS _ cs.
	self newShepherd!
*/
}
/*
udanax-top.st:10387:EmptyOrglRoot methodsFor: 'smalltalk:'!
crums
	^#()!
*/
public int contentsHash() {
	return super.contentsHash() ^ myCS.hashForEqual();
/*
udanax-top.st:10392:EmptyOrglRoot methodsFor: 'testing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: myCS hashForEqual!
*/
}
/**
 * @deprecated
 */
public void inform(Position key, HRoot value) {
	throw new PasseException();
/*
udanax-top.st:10399:EmptyOrglRoot methodsFor: 'smalltalk: passe'!
{void} inform: key {Position unused} with: value {HRoot unused}
	
	self passe!
*/
}
/**
 * @deprecated
 */
public void propBy(IObject anIObject) {
	throw new PasseException();
/*
udanax-top.st:10403:EmptyOrglRoot methodsFor: 'smalltalk: passe'!
{void} propBy: anIObject {IObject unused}
	 
	 self passe!
*/
}
/**
 * Remove the endorsements for which aClubInfo
 * is responsible.  If there are no more references
 * to this orgl, then it should be delete.  This might
 * also triggers sensors that wait for negative filters.
 * @deprecated
 */
public void unpropBy(IObject anIObject) {
	throw new PasseException();
/*
udanax-top.st:10407:EmptyOrglRoot methodsFor: 'smalltalk: passe'!
{void} unpropBy: anIObject {IObject unused} 
	"Remove the endorsements for which aClubInfo
	 is responsible.  If there are no more references
	 to this orgl, then it should be delete.  This might
	 also triggers sensors that wait for negative filters."
	 
	 self passe!
*/
}
/**
 * @deprecated
 */
public void wait(XnSensor sensor) {
	throw new PasseException();
/*
udanax-top.st:10415:EmptyOrglRoot methodsFor: 'smalltalk: passe'!
{void} wait: sensor {XnSensor unused}
	
	self passe!
*/
}
public EmptyOrglRoot(Rcvr receiver) {
	super(receiver);
	myCS = (CoordinateSpace) receiver.receiveHeaper();
/*
udanax-top.st:10421:EmptyOrglRoot methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCS _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myCS);
/*
udanax-top.st:10425:EmptyOrglRoot methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCS.!
*/
}
public EmptyOrglRoot() {
/*

Generated during transformation
*/
}
}
