/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.fossil;

import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.fossil.WorkRecorderFossil;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * A Fossil for a WorkRecorder.
 */
public class WorkRecorderFossil extends RecorderFossil {

	protected Filter myEndorsementsFilter;
/*
udanax-top.st:10930:
RecorderFossil subclass: #WorkRecorderFossil
	instanceVariableNames: 'myEndorsementsFilter {Filter}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-fossil'!
*/
/*
udanax-top.st:10934:
WorkRecorderFossil comment:
'A Fossil for a WorkRecorder.'!
*/
/*
udanax-top.st:10936:
(WorkRecorderFossil getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #DEFERRED; add: #DEFERRED.LOCKED; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(WorkRecorderFossil.class).setAttributes( new Set().add("SHEPHERDPATRIARCH").add("COPY").add("DEFERRED").add("DEFERREDLOCKED").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public ResultRecorder actualRecorder() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:10941:WorkRecorderFossil methodsFor: 'protected: accessing'!
{ResultRecorder} actualRecorder
	self subclassResponsibility!
*/
}
public Filter endorsementsFilter() {
	return myEndorsementsFilter;
/*
udanax-top.st:10945:WorkRecorderFossil methodsFor: 'protected: accessing'!
{Filter} endorsementsFilter
	^myEndorsementsFilter!
*/
}
public WorkRecorderFossil(IDRegion loginAuthority, Filter endorsementsFilter, TrailBlazer trailBlazer) {
	super(loginAuthority, trailBlazer);
	myEndorsementsFilter = endorsementsFilter;
/*
udanax-top.st:10951:WorkRecorderFossil methodsFor: 'create'!
create: loginAuthority {IDRegion}
	with: endorsementsFilter {Filter}
	with: trailBlazer {TrailBlazer}
	
	super create: loginAuthority with: trailBlazer.
	myEndorsementsFilter := endorsementsFilter.!
*/
}
public WorkRecorderFossil(Rcvr receiver) {
	super(receiver);
	myEndorsementsFilter = (Filter) receiver.receiveHeaper();
/*
udanax-top.st:10960:WorkRecorderFossil methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myEndorsementsFilter _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myEndorsementsFilter);
/*
udanax-top.st:10964:WorkRecorderFossil methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myEndorsementsFilter.!
*/
}
public WorkRecorderFossil() {
/*

Generated during transformation
*/
}
}
