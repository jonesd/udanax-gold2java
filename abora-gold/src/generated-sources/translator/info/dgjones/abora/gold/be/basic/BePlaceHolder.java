/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.basic;

import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BePlaceHolder;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.RegionLoaf;
import info.dgjones.abora.gold.brange1.FillDetectorExecutor;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.detect.FeFillDetector;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeActualPlaceHolder;
import info.dgjones.abora.gold.nkernel.FePlaceHolder;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.primtab.PrimSet;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class BePlaceHolder extends BeRangeElement {

	protected TrailBlazer myTrailBlazer;
	protected PrimSet myDetectors;
/*
udanax-top.st:3419:
BeRangeElement subclass: #BePlaceHolder
	instanceVariableNames: '
		myTrailBlazer {TrailBlazer | NULL}
		myDetectors {PrimSet NOCOPY | NULL of: FeFillDetector}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Basic'!
*/
/*
udanax-top.st:3425:
(BePlaceHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BePlaceHolder.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void addDetector(FeFillDetector detector) {
	if (myDetectors == null) {
		myDetectors = PrimSet.weak(7, (FillDetectorExecutor.make(this)));
	}
	myDetectors.store(detector);
/*
udanax-top.st:3430:BePlaceHolder methodsFor: 'accessing'!
{void} addDetector: detector {FeFillDetector}
	myDetectors == NULL ifTrue:
		[myDetectors := PrimSet weak: 7 with: (FillDetectorExecutor make: self)].
	myDetectors store: detector!
*/
}
public boolean isPurgeable() {
	return super.isPurgeable() && (myDetectors == null);
/*
udanax-top.st:3436:BePlaceHolder methodsFor: 'accessing'!
{BooleanVar} isPurgeable
	^super isPurgeable and: [myDetectors == NULL]!
*/
}
public FeRangeElement makeFe(BeLabel label) {
	return FePlaceHolder.on(this);
/*
udanax-top.st:3439:BePlaceHolder methodsFor: 'accessing'!
{FeRangeElement} makeFe: label {BeLabel | NULL}
	^FePlaceHolder on: self!
*/
}
/**
 * Change the identity of this object to that of the other.
 */
public boolean makeIdentical(BeRangeElement other) {
	/* Make all my persistent oParents point at the other guy.
	make all the session level FeRangeElements point at the other guy. */
	ScruSet oParents;
	oParents = hCrum().oParents();
	Someone.knownBug();
	/* if there are several oParents then a given Detector may be rung more than once */
	AboraBlockSupport.enterConsistent(-1);
	try {
		Stepper stomper = oParents.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			Loaf loaf = (Loaf) stomper.fetch();
			if (loaf == null) {
				continue ;
			}
			((RegionLoaf) loaf).forwardTo(other);
		}
		stomper.destroy();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	Stepper stomper2 = feRangeElements().stepper();
	for (; stomper2.hasValue(); stomper2.step()) {
		FePlaceHolder elem = (FePlaceHolder) stomper2.fetch();
		if (elem == null) {
			continue ;
		}
		((FeActualPlaceHolder) elem).forwardTo(other);
	}
	stomper2.destroy();
	if (myDetectors != null) {
		FeRangeElement fe;
		if (other instanceof BeEdition) {
			BeEdition ed = (BeEdition) other;
			fe = ed.makeFe(((BeGrandMap) CurrentGrandMap.fluidGet()).newLabel());
		}
		else {
			fe = other.makeFe(null);
		}
		Stepper stomper3 = myDetectors.stepper();
		for (; stomper3.hasValue(); stomper3.step()) {
			FeFillDetector det = (FeFillDetector) stomper3.fetch();
			if (det == null) {
				continue ;
			}
			det.filled(fe);
		}
		stomper3.destroy();
	}
	return false;
/*
udanax-top.st:3442:BePlaceHolder methodsFor: 'accessing'!
{BooleanVar} makeIdentical: other {BeRangeElement}
	"Change the identity of this object to that of the other."
	
	"Make all my persistent oParents point at the other guy.
	make all the session level FeRangeElements point at the other guy."
	
	| oParents {ScruSet of: OPart} |
	oParents _ self hCrum oParents.
	self knownBug. "if there are several oParents then a given Detector may be rung more than once"
	DiskManager consistent: -1 with:
		[oParents stepper forEach: 
			[:loaf {Loaf} |
			(loaf cast: RegionLoaf) forwardTo: other]].
	self feRangeElements stepper forEach: 
		[:elem {FePlaceHolder} |
		(elem cast: FeActualPlaceHolder) forwardTo: other].
	myDetectors ~~ NULL ifTrue:
		[ | fe {FeRangeElement} |
		other cast: BeEdition into: [ :ed |
			fe := ed makeFe: CurrentGrandMap fluidGet newLabel]
		others:
			[fe := other makeFe: NULL].
		myDetectors stepper forEach: [ :det {FeFillDetector} |
			det filled: fe]].
	^false "fodder"!
*/
}
public void removeDetector(FeFillDetector detector) {
	if (Heaper.isDestructed(myDetectors)) {
		return ;
	}
	if (myDetectors == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_SET);
	}
	myDetectors.remove(detector);
	if (myDetectors.isEmpty()) {
		myDetectors = null;
	}
/*
udanax-top.st:3468:BePlaceHolder methodsFor: 'accessing'!
{void} removeDetector: detector {FeFillDetector}
	(Heaper isDestructed: myDetectors) ifTrue:
		[^VOID].
	myDetectors == NULL ifTrue:
		[Heaper BLAST: #NotInSet].
	myDetectors remove: detector.
	myDetectors isEmpty ifTrue:
		[myDetectors := NULL].!
*/
}
public void removeLastDetector() {
	myDetectors = null;
/*
udanax-top.st:3478:BePlaceHolder methodsFor: 'accessing'!
{void} removeLastDetector
	myDetectors := NULL!
*/
}
public BePlaceHolder() {
	super(SensorCrum.partial());
	myTrailBlazer = null;
	myDetectors = null;
	newShepherd();
/*
udanax-top.st:3484:BePlaceHolder methodsFor: 'creation'!
create
	super create: SensorCrum partial.
	myTrailBlazer := NULL.
	myDetectors := NULL.
	self newShepherd!
*/
}
public BePlaceHolder(TrailBlazer blazer) {
	super(SensorCrum.partial());
	myTrailBlazer = blazer;
	if (blazer != null) {
		blazer.addReference(this);
	}
	myDetectors = null;
	newShepherd();
/*
udanax-top.st:3490:BePlaceHolder methodsFor: 'creation'!
create: blazer {TrailBlazer | NULL}
	super create: SensorCrum partial.
	myTrailBlazer := blazer.
	blazer ~~ NULL ifTrue:
		[blazer addReference: self].
	myDetectors := NULL.
	self newShepherd!
*/
}
public void attachTrailBlazer(TrailBlazer blazer) {
	AboraBlockSupport.enterConsistent(3);
	try {
		if (myTrailBlazer != null) {
			if (myTrailBlazer.isAlive()) {
				throw new AboraRuntimeException(AboraRuntimeException.FATAL_ERROR);
			}
			else {
				myTrailBlazer.removeReference(this);
			}
		}
		myTrailBlazer = blazer;
		blazer.addReference(this);
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:3500:BePlaceHolder methodsFor: 'backfollow'!
{void} attachTrailBlazer: blazer {TrailBlazer}
	
	DiskManager consistent: 3 with:
		[myTrailBlazer ~~ NULL ifTrue:
			[myTrailBlazer isAlive
				ifTrue: [Heaper BLAST: #FatalError]
				ifFalse: [myTrailBlazer removeReference: self]].
		myTrailBlazer := blazer.
		blazer addReference: self.
		self diskUpdate]!
*/
}
public void checkTrailBlazer(TrailBlazer blazer) {
	if ( ! (myTrailBlazer != null && (myTrailBlazer.isEqual(blazer)))) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_TRAIL);
	}
/*
udanax-top.st:3511:BePlaceHolder methodsFor: 'backfollow'!
{void} checkTrailBlazer: blazer {TrailBlazer}
	(myTrailBlazer ~~ NULL and: [myTrailBlazer isEqual: blazer])
		ifFalse: [Heaper BLAST: #InvalidTrail]!
*/
}
public TrailBlazer fetchTrailBlazer() {
	if (myTrailBlazer == null || (myTrailBlazer.isAlive())) {
		return myTrailBlazer;
	}
	/* it was not successfully attached, so clean it up */
	AboraBlockSupport.enterConsistent(2);
	try {
		myTrailBlazer.removeReference(this);
		myTrailBlazer = null;
		diskUpdate();
		return null;
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:3516:BePlaceHolder methodsFor: 'backfollow'!
{TrailBlazer | NULL} fetchTrailBlazer
	
	(myTrailBlazer == NULL or: [myTrailBlazer isAlive])
		ifTrue: [^myTrailBlazer].
	"it was not successfully attached, so clean it up"
	DiskManager consistent: 2 with:
		[myTrailBlazer removeReference: self.
		myTrailBlazer := NULL.
		self diskUpdate.
		^NULL]!
*/
}
public void restartP(Rcvr rcvr) {
	myDetectors = null;
/*
udanax-top.st:3529:BePlaceHolder methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartP: rcvr {Rcvr unused}
	myDetectors := NULL.!
*/
}
public BePlaceHolder(Rcvr receiver) {
	super(receiver);
	myTrailBlazer = (TrailBlazer) receiver.receiveHeaper();
	restartP(receiver);
/*
udanax-top.st:3535:BePlaceHolder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myTrailBlazer _ receiver receiveHeaper.
	self restartP: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myTrailBlazer);
/*
udanax-top.st:3540:BePlaceHolder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myTrailBlazer.!
*/
}
}
