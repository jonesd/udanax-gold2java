/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.turtle;

import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.turtle.SouthRecorderChecker;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * This is a one-shot agenda item.
 * When changing the prop(ertie)s of a Stamp, we need to first take care of the future
 * backFollow requests (by updating the Bert Canopy so the filtered HTree walk will find this
 * Stamp) before taking care of the past (the Recorders that were looking for this Stamp in
 * their future).  This AgendaItem is to remember to take care of the past (by doing a
 * southwards o-walk filtered by the Sensor Canopy) after the future is properly dealt with.
 * The RecorderHoister assumes that this southward walk is done in a single-step, so it is
 * free to make changes in a way that, if it were interleaved with an incremental southward
 * walk by a RecorderChecker looking for the recorder(s) being hoisted, might cause the
 * hoisted recorder to be missed.
 * This is also used recursively by this very o-walk to schedule a further o-walk on
 * appropriate sub-Stamps.
 * Keeping track of whether persistent objects are garbage-on-disk during AgendaItem
 * processing only remains open for Stamps, except here where it also arises for an OrglRoot.
 * The OrglRoot is itself held by a persistent Stamp, from which it can be easily obtained,
 * so we should probably just hold onto two Stamps instead of a Stamp and an OrglRoot (so I
 * only have to solve the "how to keep it around" problem for Stamps).
 */
public class SouthRecorderChecker extends AgendaItem {

	protected OrglRoot myORoot;
	protected PropFinder myFinder;
	protected SensorCrum mySCrum;
/*
udanax-top.st:1252:
AgendaItem subclass: #SouthRecorderChecker
	instanceVariableNames: '
		myORoot {OrglRoot | NULL}
		myFinder {PropFinder}
		mySCrum {SensorCrum | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-turtle'!
*/
/*
udanax-top.st:1259:
SouthRecorderChecker comment:
'This is a one-shot agenda item.
When changing the prop(ertie)s of a Stamp, we need to first take care of the future backFollow requests (by updating the Bert Canopy so the filtered HTree walk will find this Stamp) before taking care of the past (the Recorders that were looking for this Stamp in their future).  This AgendaItem is to remember to take care of the past (by doing a southwards o-walk filtered by the Sensor Canopy) after the future is properly dealt with.
The RecorderHoister assumes that this southward walk is done in a single-step, so it is free to make changes in a way that, if it were interleaved with an incremental southward walk by a RecorderChecker looking for the recorder(s) being hoisted, might cause the hoisted recorder to be missed.
This is also used recursively by this very o-walk to schedule a further o-walk on appropriate sub-Stamps.
Keeping track of whether persistent objects are garbage-on-disk during AgendaItem processing only remains open for Stamps, except here where it also arises for an OrglRoot.  The OrglRoot is itself held by a persistent Stamp, from which it can be easily obtained, so we should probably just hold onto two Stamps instead of a Stamp and an OrglRoot (so I only have to solve the "how to keep it around" problem for Stamps).'!
*/
/*
udanax-top.st:1269:
(SouthRecorderChecker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:1332:
SouthRecorderChecker class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:1335:
(SouthRecorderChecker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SouthRecorderChecker.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public SouthRecorderChecker(OrglRoot oroot, PropFinder finder, SensorCrum scrum) {
	super();
	myORoot = oroot;
	myFinder = finder;
	Someone.knownBug();
	/* make sure these objects stick around.  mySCrum has add/removePointer already.  myStamp and myORoot need something similar.  myFinder is one of my sheep and is already OK. */
	mySCrum = scrum;
	if (mySCrum != null) {
		mySCrum.addPointer(this);
	}
	newShepherd();
/*
udanax-top.st:1274:SouthRecorderChecker methodsFor: 'creation'!
create: oroot {OrglRoot}  
	with: finder {PropFinder} 
	with: scrum {SensorCrum | NULL}
	
	super create.
	myORoot _ oroot.
	myFinder _ finder.
	self knownBug. "make sure these objects stick around.  mySCrum has add/removePointer already.  myStamp and myORoot need something similar.  myFinder is one of my sheep and is already OK."
	mySCrum _ scrum.
	mySCrum ~~ NULL
		ifTrue: [mySCrum addPointer: self].
	self newShepherd.!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(3);
	try {
		if (mySCrum != null) {
			mySCrum.removePointer(this);
			mySCrum = null;
		}
		Someone.thingToDo();
		/* stop making sure these objects stick around */
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:1287:SouthRecorderChecker methodsFor: 'creation'!
{void} dismantle
	
	DiskManager consistent: 3 with:
		[mySCrum ~~ NULL
			ifTrue: 
				[mySCrum removePointer: self.
				mySCrum _ NULL].
		self thingToDo. "stop making sure these objects stick around"
		super dismantle]!
*/
}
public boolean step() {
	/* See class comment for a constraint on this method.
	
	If empty ORoot
		We've already shot once.  Do nothing.
	Check for any recorders in the sensor canopy that need to be rung.
		Remember that we're done. */
	if (myORoot == null) {
		return false;
	}
	myORoot.checkRecorders(myFinder, mySCrum);
	AboraBlockSupport.enterConsistent(1);
	try {
		myORoot = null;
		Someone.thingToDo();
		/* stop making sure these objects stick around */
		diskUpdate();
		return false;
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:1299:SouthRecorderChecker methodsFor: 'accessing'!
{BooleanVar} step
	| |
	
	"See class comment for a constraint on this method.
	
	If empty ORoot
		We've already shot once.  Do nothing.
	Check for any recorders in the sensor canopy that need to be rung.
		Remember that we're done."
	myORoot == NULL
		ifTrue: [^false].
	myORoot checkRecorders: myFinder with: mySCrum.
	DiskManager consistent: 1 with:
		[myORoot _ NULL.
		self thingToDo. "stop making sure these objects stick around"
		self diskUpdate.
		^false]!
*/
}
public SouthRecorderChecker(Rcvr receiver) {
	super(receiver);
	myORoot = (OrglRoot) receiver.receiveHeaper();
	myFinder = (PropFinder) receiver.receiveHeaper();
	mySCrum = (SensorCrum) receiver.receiveHeaper();
/*
udanax-top.st:1319:SouthRecorderChecker methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myORoot _ receiver receiveHeaper.
	myFinder _ receiver receiveHeaper.
	mySCrum _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myORoot);
	xmtr.sendHeaper(myFinder);
	xmtr.sendHeaper(mySCrum);
/*
udanax-top.st:1325:SouthRecorderChecker methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myORoot.
	xmtr sendHeaper: myFinder.
	xmtr sendHeaper: mySCrum.!
*/
}
public static SouthRecorderChecker make(OrglRoot oroot, PropFinder finder, SensorCrum scrum) {
	AboraBlockSupport.enterConsistent(2);
	try {
		return new SouthRecorderChecker(oroot, finder, scrum);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:1340:SouthRecorderChecker class methodsFor: 'creation'!
make: oroot {OrglRoot} 
	with: finder {PropFinder} 
	with: scrum {SensorCrum | NULL} 
	
	DiskManager consistent: 2 with: 
		[^self
			create: oroot
			with: finder
			with: scrum]!
*/
}
/**
 * @deprecated
 */
public static SouthRecorderChecker make(OrglRoot oroot, BeEdition stamp, PropFinder finder, SensorCrum scrum) {
	throw new PasseException();
/*
udanax-top.st:1352:SouthRecorderChecker class methodsFor: 'smalltalk: passe'!
make: oroot {OrglRoot} 
	with: stamp {BeEdition} 
	with: finder {PropFinder} 
	with: scrum {SensorCrum | NULL} 
	
	self passe "fewer args"!
*/
}
public SouthRecorderChecker() {
/*

Generated during transformation
*/
}
}
