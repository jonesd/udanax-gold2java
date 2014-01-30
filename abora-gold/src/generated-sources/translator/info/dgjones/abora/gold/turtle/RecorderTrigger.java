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
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.turtle.RecorderTrigger;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * This is a one-shot agenda item.
 * Asks myFossil to record myElement.
 * When an answer to a delayed backFollow is found, whether thru a northwards h-walk
 * (filtered by the Bert Canopy) of a southwards o-walk (filtered by the Sensor Canopy),
 * instead of actually recording the answer into the backFollow trail immediately, we shedule
 * a RecorderTrigger to do the job.
 */
public class RecorderTrigger extends AgendaItem {

	protected RecorderFossil myFossil;
	protected BeRangeElement myElement;
/*
udanax-top.st:1089:
AgendaItem subclass: #RecorderTrigger
	instanceVariableNames: '
		myFossil {RecorderFossil | NULL}
		myElement {BeRangeElement}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-turtle'!
*/
/*
udanax-top.st:1095:
RecorderTrigger comment:
'This is a one-shot agenda item.
Asks myFossil to record myElement.
When an answer to a delayed backFollow is found, whether thru a northwards h-walk (filtered by the Bert Canopy) of a southwards o-walk (filtered by the Sensor Canopy), instead of actually recording the answer into the backFollow trail immediately, we shedule a RecorderTrigger to do the job.'!
*/
/*
udanax-top.st:1101:
(RecorderTrigger getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:1163:
RecorderTrigger class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:1166:
(RecorderTrigger getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RecorderTrigger.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean step() {
	/* If null pointer to myFossil
		We've already shot once.  Do nothing.
	If myFossil is still in suspension
		Inform myFossil with myElement
	Atomically
		Remove refcount from ourself on myFossil.
		Remember that we're done. */
	if (myFossil == null) {
		return false;
	}
	if ( ! (myFossil.isExtinct())) {
		ResultRecorder recorder = AboraBlockSupport.enterRecorderFossilReanimate(myFossil);
		try {
			recorder.record(myElement);
		}
		finally {
			AboraBlockSupport.exitRecorderFossilReanimate();
		}
	}
	AboraBlockSupport.enterConsistent(2);
	try {
		myFossil.removeItem(this);
		myFossil = null;
		Someone.thingToDo();
		/* stop making sure the Edition doesn't go away; it needs a refcount or something like it. */
		diskUpdate();
		return false;
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:1106:RecorderTrigger methodsFor: 'accessing'!
{BooleanVar} step
	||
	
	"If null pointer to myFossil
		We've already shot once.  Do nothing.
	If myFossil is still in suspension
		Inform myFossil with myElement
	Atomically
		Remove refcount from ourself on myFossil.
		Remember that we're done."
		
	myFossil == NULL
		ifTrue: [^false].
	myFossil isExtinct ifFalse: 
		[myFossil reanimate: [:recorder {ResultRecorder} |
			recorder record: myElement]].
	DiskManager consistent: 2 with:
		[myFossil removeItem: self.
		myFossil _ NULL.
		self thingToDo. "stop making sure the Edition doesn't go away; it needs a refcount or something like it."
		self diskUpdate.
		^false].!
*/
}
public RecorderTrigger(RecorderFossil fossil, BeRangeElement element) {
	super();
	myFossil = fossil;
	myFossil.addItem(this);
	myElement = element;
	Someone.thingToDo();
	/* make sure the RangeElement doesn't go away */
	newShepherd();
/*
udanax-top.st:1131:RecorderTrigger methodsFor: 'creation'!
create: fossil {RecorderFossil} with: element {BeRangeElement}
	super create.
	myFossil _ fossil.
	myFossil addItem: self.
	myElement _ element.
	self thingToDo. "make sure the RangeElement doesn't go away"
	self newShepherd.!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(2);
	try {
		if (myFossil != null) {
			myFossil.removeItem(this);
			myFossil = null;
		}
		Someone.thingToDo();
		/* stop making sure the stamp doesn't go away */
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:1140:RecorderTrigger methodsFor: 'creation'!
{void} dismantle
	DiskManager consistent: 2 with:
		[myFossil ~~ NULL
			ifTrue:
				[myFossil removeItem: self.
				myFossil _ NULL].
		self thingToDo. "stop making sure the stamp doesn't go away"
		super dismantle]!
*/
}
public RecorderTrigger(Rcvr receiver) {
	super(receiver);
	myFossil = (RecorderFossil) receiver.receiveHeaper();
	myElement = (BeRangeElement) receiver.receiveHeaper();
/*
udanax-top.st:1152:RecorderTrigger methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myFossil _ receiver receiveHeaper.
	myElement _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myFossil);
	xmtr.sendHeaper(myElement);
/*
udanax-top.st:1157:RecorderTrigger methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myFossil.
	xmtr sendHeaper: myElement.!
*/
}
public static RecorderTrigger make(RecorderFossil fossil, BeRangeElement element) {
	AboraBlockSupport.enterConsistent(2);
	try {
		return new RecorderTrigger(fossil, element);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:1171:RecorderTrigger class methodsFor: 'creation'!
make: fossil {RecorderFossil} with: element {BeRangeElement}
	DiskManager consistent: 2 with:
		[^self create: fossil with: element]!
*/
}
public RecorderTrigger() {
/*

Generated during transformation
*/
}
}
