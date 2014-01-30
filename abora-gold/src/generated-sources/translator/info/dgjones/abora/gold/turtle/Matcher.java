/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.turtle;

import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.turtle.Matcher;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * This is a one-shot agenda item.
 * When doing a delayed backFollow, after the future is taken care of (by posting recorders
 * in the Sensor Canopy), the past needs to be checked (by walking the HTree northwards
 * filtered by the Bert Canopy).  This AgendaItem is a one-shot used to remember to
 * backFollow thru the past.  (myOrglRoot == NULL when the shot has been done.)
 */
public class Matcher extends AgendaItem {

	protected OrglRoot myOrglRoot;
	protected PropFinder myFinder;
	protected RecorderFossil myFossil;
/*
udanax-top.st:609:
AgendaItem subclass: #Matcher
	instanceVariableNames: '
		myOrglRoot {OrglRoot | NULL}
		myFinder {PropFinder}
		myFossil {RecorderFossil}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-turtle'!
*/
/*
udanax-top.st:616:
Matcher comment:
'This is a one-shot agenda item.
When doing a delayed backFollow, after the future is taken care of (by posting recorders in the Sensor Canopy), the past needs to be checked (by walking the HTree northwards filtered by the Bert Canopy).  This AgendaItem is a one-shot used to remember to backFollow thru the past.  (myOrglRoot == NULL when the shot has been done.)'!
*/
/*
udanax-top.st:620:
(Matcher getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:682:
Matcher class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:685:
(Matcher getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Matcher.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean step() {
	/* If myStamp is NULL
		We've already shot once.  Do nothing.
	walk the HTree northwards filtered by the Bert Canopy, scheduling RecorderTriggers to record already-existing matching stamps.  ('past' part of backfollow)
		Remember that we're done. */
	if (myOrglRoot == null) {
		return false;
	}
	ResultRecorder recorder = AboraBlockSupport.enterRecorderFossilReanimate(myFossil);
	try {
		myOrglRoot.delayedFindMatching(myFinder, myFossil, recorder);
	}
	finally {
		AboraBlockSupport.exitRecorderFossilReanimate();
	}
	AboraBlockSupport.enterConsistent(1);
	try {
		myOrglRoot = null;
		Someone.thingToDo();
		/* stop making sure the stamp sticks around */
		diskUpdate();
		return false;
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:625:Matcher methodsFor: 'accessing'!
{BooleanVar} step
	| |
	"If myStamp is NULL
		We've already shot once.  Do nothing.
	walk the HTree northwards filtered by the Bert Canopy, scheduling RecorderTriggers to record already-existing matching stamps.  ('past' part of backfollow)
		Remember that we're done."
		
	
	myOrglRoot == NULL
		ifTrue: [^false].
	myFossil reanimate: [ :recorder {ResultRecorder} |
		myOrglRoot delayedFindMatching: myFinder
			with: myFossil
			with: recorder].
	DiskManager consistent: 1 with:
		[myOrglRoot _ NULL.
		self thingToDo. "stop making sure the stamp sticks around"
		self diskUpdate.
		^false]!
*/
}
public Matcher(OrglRoot oroot, PropFinder finder, RecorderFossil fossil) {
	super();
	myOrglRoot = oroot;
	Someone.thingToDo();
	/* make sure the stamp sticks around.  Do something like what's being done with myFossil>>addItem */
	myFinder = finder;
	myFossil = fossil;
	myFossil.addItem(this);
	/* bump refcount on myFossil */
	newShepherd();
/*
udanax-top.st:648:Matcher methodsFor: 'creation'!
create: oroot {OrglRoot} 
	with: finder {PropFinder} 
	with: fossil {RecorderFossil}
	
	super create.
	myOrglRoot _ oroot.
	self thingToDo. "make sure the stamp sticks around.  Do something like what's being done with myFossil>>addItem"
	myFinder _ finder.
	myFossil _ fossil.
	myFossil addItem: self.	"bump refcount on myFossil"
	self newShepherd.!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(3);
	try {
		myFossil.removeItem(this);
		/* Unbump refcount on myFossil. */
		Someone.thingToDo();
		/* stop making sure the OrglRoot sticks around.  AgendaItems may be aborted by the enclosing algorithm, so can't assume I dropped my reference by stepping. */
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:660:Matcher methodsFor: 'creation'!
{void} dismantle
	
	DiskManager consistent: 3 with:
		[myFossil removeItem: self.	"Unbump refcount on myFossil."
		self thingToDo. "stop making sure the OrglRoot sticks around.  AgendaItems may be aborted by the enclosing algorithm, so can't assume I dropped my reference by stepping."
		super dismantle]!
*/
}
public Matcher(Rcvr receiver) {
	super(receiver);
	myOrglRoot = (OrglRoot) receiver.receiveHeaper();
	myFinder = (PropFinder) receiver.receiveHeaper();
	myFossil = (RecorderFossil) receiver.receiveHeaper();
/*
udanax-top.st:669:Matcher methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myOrglRoot _ receiver receiveHeaper.
	myFinder _ receiver receiveHeaper.
	myFossil _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myOrglRoot);
	xmtr.sendHeaper(myFinder);
	xmtr.sendHeaper(myFossil);
/*
udanax-top.st:675:Matcher methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myOrglRoot.
	xmtr sendHeaper: myFinder.
	xmtr sendHeaper: myFossil.!
*/
}
public static Matcher make(OrglRoot oroot, PropFinder finder, RecorderFossil fossil) {
	AboraBlockSupport.enterConsistent(2);
	try {
		return new Matcher(oroot, finder, fossil);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:690:Matcher class methodsFor: 'creation'!
make: oroot {OrglRoot} 
	with: finder {PropFinder} 
	with: fossil {RecorderFossil}
	
	DiskManager consistent: 2 with:
		[^self create: oroot with: finder with: fossil]!
*/
}
public Matcher() {
/*

Generated during transformation
*/
}
}
