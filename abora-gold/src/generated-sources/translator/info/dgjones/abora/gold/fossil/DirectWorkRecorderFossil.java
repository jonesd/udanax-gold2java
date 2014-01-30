/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.fossil;

import info.dgjones.abora.gold.backrec.DirectWorkRecorder;
import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BePlaceHolder;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.fossil.DirectWorkRecorderFossil;
import info.dgjones.abora.gold.fossil.WorkRecorderFossil;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * A Fossil for a DirectWorkRecorder.
 */
public class DirectWorkRecorderFossil extends WorkRecorderFossil {

/*
udanax-top.st:10968:
WorkRecorderFossil subclass: #DirectWorkRecorderFossil
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-fossil'!
*/
/*
udanax-top.st:10972:
DirectWorkRecorderFossil comment:
'A Fossil for a DirectWorkRecorder.'!
*/
/*
udanax-top.st:10974:
(DirectWorkRecorderFossil getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DirectWorkRecorderFossil.class).setAttributes( new Set().add("SHEPHERDPATRIARCH").add("COPY").add("LOCKED").add("NOTATYPE").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public ResultRecorder actualRecorder() {
	return new DirectWorkRecorder(endorsementsFilter(), trailBlazer());
/*
udanax-top.st:10979:DirectWorkRecorderFossil methodsFor: 'protected: accessing'!
{ResultRecorder} actualRecorder
	^DirectWorkRecorder
		create: self endorsementsFilter
		with: self trailBlazer!
*/
}
public DirectWorkRecorderFossil(IDRegion loginAuthority, Filter endorsementsFilter, TrailBlazer trailBlazer) {
	super(loginAuthority, endorsementsFilter, trailBlazer);
	newShepherd();
	remember();
/*
udanax-top.st:10987:DirectWorkRecorderFossil methodsFor: 'create'!
create: loginAuthority {IDRegion}
	with: endorsementsFilter {Filter}
	with: trailBlazer {TrailBlazer}
	
	super create: loginAuthority
		with: endorsementsFilter
		with: trailBlazer.
	self newShepherd.
	self remember.!
*/
}
/**
 * do nothing
 */
public void storeDataRecordingAgents(SensorCrum sensorCrum, Agenda agenda) {
/*
udanax-top.st:10999:DirectWorkRecorderFossil methodsFor: 'backfollow'!
{void} storeDataRecordingAgents: sensorCrum {SensorCrum}
	with: agenda {Agenda}
	"do nothing"!
*/
}
public void storeRangeElementRecordingAgents(BeRangeElement rangeElement, SensorCrum sensorCrum, Agenda agenda) {
	if ((rangeElement instanceof BeEdition) || (rangeElement instanceof BePlaceHolder)) {
		super.storeRangeElementRecordingAgents(rangeElement, sensorCrum, agenda);
	}
/*
udanax-top.st:11004:DirectWorkRecorderFossil methodsFor: 'backfollow'!
{void} storeRangeElementRecordingAgents: rangeElement {BeRangeElement}
	with: sensorCrum {SensorCrum}
	with: agenda {Agenda}
	((rangeElement isKindOf: BeEdition) or: [rangeElement isKindOf: BePlaceHolder])
		ifTrue:
			[super storeRangeElementRecordingAgents: rangeElement
				with: sensorCrum
				with: agenda]!
*/
}
public DirectWorkRecorderFossil(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:11016:DirectWorkRecorderFossil methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:11019:DirectWorkRecorderFossil methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public DirectWorkRecorderFossil() {
/*

Generated during transformation
*/
}
}
