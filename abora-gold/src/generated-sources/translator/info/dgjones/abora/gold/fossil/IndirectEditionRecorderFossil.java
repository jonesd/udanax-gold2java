/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.fossil;

import info.dgjones.abora.gold.backrec.IndirectEditionRecorder;
import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.fossil.EditionRecorderFossil;
import info.dgjones.abora.gold.fossil.IndirectEditionRecorderFossil;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * A Fossil for an EditionRecorder with the directOnly flag off.
 */
public class IndirectEditionRecorderFossil extends EditionRecorderFossil {

/*
udanax-top.st:10890:
EditionRecorderFossil subclass: #IndirectEditionRecorderFossil
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-fossil'!
*/
/*
udanax-top.st:10894:
IndirectEditionRecorderFossil comment:
'A Fossil for an EditionRecorder with the directOnly flag off.'!
*/
/*
udanax-top.st:10896:
(IndirectEditionRecorderFossil getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IndirectEditionRecorderFossil.class).setAttributes( new Set().add("SHEPHERDPATRIARCH").add("COPY").add("LOCKED").add("NOTATYPE").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public ResultRecorder actualRecorder() {
	return new IndirectEditionRecorder(directFilter(), indirectFilter(), trailBlazer());
/*
udanax-top.st:10901:IndirectEditionRecorderFossil methodsFor: 'protected: accessing'!
{ResultRecorder} actualRecorder
	^IndirectEditionRecorder
		create: self directFilter
		with: self indirectFilter
		with: self trailBlazer!
*/
}
public IndirectEditionRecorderFossil(IDRegion loginAuthority, Filter directFilter, Filter indirectFilter, TrailBlazer trailBlazer) {
	super(loginAuthority, directFilter, indirectFilter, trailBlazer);
	newShepherd();
	remember();
/*
udanax-top.st:10910:IndirectEditionRecorderFossil methodsFor: 'create'!
create: loginAuthority {IDRegion}
	with: directFilter {Filter}
	with: indirectFilter {Filter}
	with: trailBlazer {TrailBlazer}
	
	super create: loginAuthority
		with: directFilter
		with: indirectFilter
		with: trailBlazer.
	self newShepherd.
	self remember.!
*/
}
public IndirectEditionRecorderFossil(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:10924:IndirectEditionRecorderFossil methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:10927:IndirectEditionRecorderFossil methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public IndirectEditionRecorderFossil() {
/*

Generated during transformation
*/
}
}
