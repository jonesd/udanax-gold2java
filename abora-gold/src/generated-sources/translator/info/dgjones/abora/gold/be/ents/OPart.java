/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.ents;

import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.ents.HistoryCrum;
import info.dgjones.abora.gold.be.ents.OPart;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.XnSensor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class OPart extends Abraham {

	protected SensorCrum mySensorCrum;
/*
udanax-top.st:7258:
Abraham subclass: #OPart
	instanceVariableNames: 'mySensorCrum {SensorCrum}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:7262:
(OPart getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.ANCESTOR; add: #DEFERRED.LOCKED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OPart.class).setAttributes( new Set().add("DEFERRED").add("COPY").add("SHEPHERDANCESTOR").add("DEFERREDLOCKED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Attach the TrailBlazer to this Edition, and return the region of partiality it is attached
 * to
 */
public XnRegion attachTrailBlazer(TrailBlazer blazer) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7267:OPart methodsFor: 'backfollow'!
{XnRegion} attachTrailBlazer: blazer {TrailBlazer}
	"Attach the TrailBlazer to this Edition, and return the region of partiality it is attached to"
	
	self subclassResponsibility!
*/
}
/**
 * Make sure that everyone below here that might have a TrailBlazer, has the given one
 */
public void checkTrailBlazer(TrailBlazer blazer) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7272:OPart methodsFor: 'backfollow'!
{void} checkTrailBlazer: blazer {TrailBlazer}
	"Make sure that everyone below here that might have a TrailBlazer, has the given one"
	
	self subclassResponsibility!
*/
}
/**
 * If there is a TrailBlazer somewhere below this Edition, return one of them
 */
public TrailBlazer fetchTrailBlazer() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7277:OPart methodsFor: 'backfollow'!
{TrailBlazer | NULL} fetchTrailBlazer
	"If there is a TrailBlazer somewhere below this Edition, return one of them"
	
	self subclassResponsibility!
*/
}
public HistoryCrum hCrum() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:7282:OPart methodsFor: 'backfollow'!
{HistoryCrum} hCrum
	self subclassResponsibility!
*/
}
/**
 * return the mapping into the domain space of the given trace
 */
public Mapping mappingTo(TracePosition trace, Mapping initial) {
	return hCrum().mappingTo(trace, initial);
/*
udanax-top.st:7287:OPart methodsFor: 'accessing'!
{Mapping} mappingTo: trace {TracePosition} with: initial {Mapping}
	"return the mapping into the domain space of the given trace"
	^self hCrum mappingTo: trace with: initial!
*/
}
public SensorCrum sensorCrum() {
	return mySensorCrum;
/*
udanax-top.st:7291:OPart methodsFor: 'accessing'!
{SensorCrum} sensorCrum
	^mySensorCrum!
*/
}
public void dismantle() {
	AboraBlockSupport.enterInsistent(2);
	try {
		if (Heaper.isConstructed(mySensorCrum)) {
			mySensorCrum.removePointer(this);
		}
		if ((Heaper.isConstructed(hCrum())) && (Heaper.isConstructed(hCrum().bertCrum()))) {
			hCrum().bertCrum().removePointer(hCrum());
		}
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitInsistent();
	}
/*
udanax-top.st:7296:OPart methodsFor: 'protected: delete'!
{void} dismantle
	DiskManager insistent: 2 with:
		[(Heaper isConstructed: mySensorCrum)
			ifTrue: [mySensorCrum removePointer: self].
		((Heaper isConstructed: self hCrum) and: [Heaper isConstructed: self hCrum bertCrum])
			ifTrue: [self hCrum bertCrum removePointer: self hCrum].
		super dismantle]!
*/
}
public void hinspect() {
	hCrum().inspect();
/*
udanax-top.st:7306:OPart methodsFor: 'smalltalk:'!
hinspect
	self hCrum inspect!
*/
}
/*
udanax-top.st:7309:OPart methodsFor: 'smalltalk:'!
inspect
	Sensor leftShiftDown
		ifTrue: [self basicInspect]
		ifFalse: [EntView openOn: (TreeBarnacle new
					buildOn: self
					gettingChildren: [:crum | crum crums]
					gettingImage: [:crum | DisplayText text: crum displayString asText textStyle: (TextStyle styleNamed: #small)]
					at: 0 @ 0
					vertical: true
					separation: 5 @ 10)]!
*/
/*
udanax-top.st:7320:OPart methodsFor: 'smalltalk:'!
inspectCanopy
	self hCrum bertCrum inspect!
*/
/*
udanax-top.st:7323:OPart methodsFor: 'smalltalk:'!
inspectMenuArray
	^#(
		('inspect history'	hinspect				'')
		('bert canopy'		inspectCanopy			'')
		('recorder canopy'	inspectRecorderCanopy		''))!
*/
/*
udanax-top.st:7329:OPart methodsFor: 'smalltalk:'!
inspectRecorderCanopy
	self sensorCrum inspect!
*/
public void showOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print('(');
	oo.print(hCrum().hCut());
	oo.print(", ");
	oo.print(hCrum().asOop());
	oo.print(", ");
	oo.print(hCrum().oParents().count());
	oo.print(')');
/*
udanax-top.st:7332:OPart methodsFor: 'smalltalk:'!
showOn: oo
	
	oo << self getCategory name << $( << self hCrum hCut << ', ' << self hCrum asOop << ', ' << self hCrum oParents count << $)!
*/
}
public OPart(SensorCrum scrum) {
	super();
	if (scrum == null) {
		mySensorCrum = SensorCrum.make();
	}
	else {
		mySensorCrum = scrum;
	}
	mySensorCrum.addPointer(this);
/*
udanax-top.st:7338:OPart methodsFor: 'protected: create'!
create: scrum {SensorCrum | NULL} 
	super create.
	scrum == NULL
			ifTrue: [mySensorCrum _ SensorCrum make]
			ifFalse: [mySensorCrum _ scrum].
	mySensorCrum addPointer: self!
*/
}
public OPart(int hash, SensorCrum scrum) {
	super(hash);
	if (scrum == null) {
		mySensorCrum = SensorCrum.make();
	}
	else {
		mySensorCrum = scrum;
	}
	mySensorCrum.addPointer(this);
/*
udanax-top.st:7345:OPart methodsFor: 'protected: create'!
create: hash {UInt32} with: scrum {SensorCrum | NULL}
	super create: hash.
	scrum == NULL
			ifTrue: [mySensorCrum _ SensorCrum make]
			ifFalse: [mySensorCrum _ scrum].
	mySensorCrum addPointer: self!
*/
}
public int contentsHash() {
	return super.contentsHash() ^ mySensorCrum.hashForEqual();
/*
udanax-top.st:7354:OPart methodsFor: 'testing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: mySensorCrum hashForEqual!
*/
}
/**
 * @deprecated
 */
public void wait(XnSensor sensor) {
	throw new PasseException();
/*
udanax-top.st:7361:OPart methodsFor: 'smalltalk: passe'!
{void} wait: sensor {XnSensor}
	
	self passe!
*/
}
public OPart(Rcvr receiver) {
	super(receiver);
	mySensorCrum = (SensorCrum) receiver.receiveHeaper();
/*
udanax-top.st:7367:OPart methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	mySensorCrum _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(mySensorCrum);
/*
udanax-top.st:7371:OPart methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: mySensorCrum.!
*/
}
public OPart() {
/*

Generated during transformation
*/
}
}
