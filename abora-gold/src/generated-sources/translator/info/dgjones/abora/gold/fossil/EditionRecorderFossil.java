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
import info.dgjones.abora.gold.fossil.EditionRecorderFossil;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * A Fossil for an EditionRecorder.
 */
public class EditionRecorderFossil extends RecorderFossil {

	protected Filter myDirectFilter;
	protected Filter myIndirectFilter;
/*
udanax-top.st:10802:
RecorderFossil subclass: #EditionRecorderFossil
	instanceVariableNames: '
		myDirectFilter {Filter}
		myIndirectFilter {Filter}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-fossil'!
*/
/*
udanax-top.st:10808:
EditionRecorderFossil comment:
'A Fossil for an EditionRecorder.'!
*/
/*
udanax-top.st:10810:
(EditionRecorderFossil getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #DEFERRED; add: #DEFERRED.LOCKED; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(EditionRecorderFossil.class).setAttributes( new Set().add("SHEPHERDPATRIARCH").add("COPY").add("DEFERRED").add("DEFERREDLOCKED").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public ResultRecorder actualRecorder() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:10815:EditionRecorderFossil methodsFor: 'protected: accessing'!
{ResultRecorder} actualRecorder
	self subclassResponsibility!
*/
}
public Filter directFilter() {
	return myDirectFilter;
/*
udanax-top.st:10819:EditionRecorderFossil methodsFor: 'protected: accessing'!
{Filter} directFilter
	
	^myDirectFilter!
*/
}
public Filter indirectFilter() {
	return myIndirectFilter;
/*
udanax-top.st:10823:EditionRecorderFossil methodsFor: 'protected: accessing'!
{Filter} indirectFilter
	
	^myIndirectFilter!
*/
}
public EditionRecorderFossil(IDRegion loginAuthority, Filter directFilter, Filter indirectFilter, TrailBlazer trailBlazer) {
	super(loginAuthority, trailBlazer);
	myDirectFilter = directFilter;
	myIndirectFilter = indirectFilter;
/*
udanax-top.st:10829:EditionRecorderFossil methodsFor: 'create'!
create: loginAuthority {IDRegion}
	with: directFilter {Filter}
	with: indirectFilter {Filter}
	with: trailBlazer {TrailBlazer}
	
	super create: loginAuthority with: trailBlazer.
	myDirectFilter := directFilter.
	myIndirectFilter := indirectFilter.!
*/
}
public EditionRecorderFossil(Rcvr receiver) {
	super(receiver);
	myDirectFilter = (Filter) receiver.receiveHeaper();
	myIndirectFilter = (Filter) receiver.receiveHeaper();
/*
udanax-top.st:10840:EditionRecorderFossil methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myDirectFilter _ receiver receiveHeaper.
	myIndirectFilter _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myDirectFilter);
	xmtr.sendHeaper(myIndirectFilter);
/*
udanax-top.st:10845:EditionRecorderFossil methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myDirectFilter.
	xmtr sendHeaper: myIndirectFilter.!
*/
}
public EditionRecorderFossil() {
/*

Generated during transformation
*/
}
}
