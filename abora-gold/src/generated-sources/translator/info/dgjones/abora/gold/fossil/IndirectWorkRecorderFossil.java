/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.fossil;

import info.dgjones.abora.gold.backrec.IndirectWorkRecorder;
import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.fossil.IndirectWorkRecorderFossil;
import info.dgjones.abora.gold.fossil.WorkRecorderFossil;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * A Fossil for a IndirectWorkRecorder.
 */
public class IndirectWorkRecorderFossil extends WorkRecorderFossil {

/*
udanax-top.st:11022:
WorkRecorderFossil subclass: #IndirectWorkRecorderFossil
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-fossil'!
*/
/*
udanax-top.st:11026:
IndirectWorkRecorderFossil comment:
'A Fossil for a IndirectWorkRecorder.'!
*/
/*
udanax-top.st:11028:
(IndirectWorkRecorderFossil getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IndirectWorkRecorderFossil.class).setAttributes( new Set().add("SHEPHERDPATRIARCH").add("COPY").add("LOCKED").add("NOTATYPE").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public ResultRecorder actualRecorder() {
	return new IndirectWorkRecorder(endorsementsFilter(), trailBlazer());
/*
udanax-top.st:11033:IndirectWorkRecorderFossil methodsFor: 'protected: accessing'!
{ResultRecorder} actualRecorder
	^IndirectWorkRecorder
		create: self endorsementsFilter
		with: self trailBlazer!
*/
}
public IndirectWorkRecorderFossil(IDRegion loginAuthority, Filter endorsementsFilter, TrailBlazer trailBlazer) {
	super(loginAuthority, endorsementsFilter, trailBlazer);
	newShepherd();
	remember();
/*
udanax-top.st:11041:IndirectWorkRecorderFossil methodsFor: 'create'!
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
public IndirectWorkRecorderFossil(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:11053:IndirectWorkRecorderFossil methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:11056:IndirectWorkRecorderFossil methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public IndirectWorkRecorderFossil() {
/*

Generated during transformation
*/
}
}
