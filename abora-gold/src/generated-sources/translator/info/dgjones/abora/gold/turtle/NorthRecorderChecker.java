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
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.turtle.NorthRecorderChecker;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * This is a one-shot agenda item.
 * See comment in SouthRecorderChecker for constraints and relationships to other pieces of
 * the algorithm.
 * Looks for and triggers WorkRecorders lying northward of this Edition up to the next
 * Edition. The Finder should only be carrying around Works.
 */
public class NorthRecorderChecker extends AgendaItem {

	protected BeEdition myEdition;
	protected PropFinder myFinder;
/*
udanax-top.st:697:
AgendaItem subclass: #NorthRecorderChecker
	instanceVariableNames: '
		myEdition {BeEdition}
		myFinder {PropFinder}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-turtle'!
*/
/*
udanax-top.st:703:
NorthRecorderChecker comment:
'This is a one-shot agenda item.
See comment in SouthRecorderChecker for constraints and relationships to other pieces of the algorithm.
Looks for and triggers WorkRecorders lying northward of this Edition up to the next Edition. The Finder should only be carrying around Works.'!
*/
/*
udanax-top.st:709:
(NorthRecorderChecker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:749:
NorthRecorderChecker class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:752:
(NorthRecorderChecker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(NorthRecorderChecker.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean step() {
	Ravi.knownBug();
	/* if my WorkRecorders have been hoisted they will not be found; there needs to be a way to walk north in the sensor canopy until we pass an edition boundary */
	if ( ! (myEdition == null)) {
		Ravi.thingToDo();
		/* Make this work */
		/* myEdition sensorCrum fetchNextAfterTriggeringRecorders: myFinder with: NULL. */
		AboraBlockSupport.enterConsistent(1);
		try {
			myEdition = null;
			Someone.thingToDo();
			/* stop making sure the edition sticks around */
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return false;
/*
udanax-top.st:714:NorthRecorderChecker methodsFor: 'accessing'!
{BooleanVar} step
	Ravi knownBug. "if my WorkRecorders have been hoisted they will not be found; there needs to be a way to walk north in the sensor canopy until we pass an edition boundary"
	
	myEdition == NULL ifFalse:
		[Ravi thingToDo. "Make this work"
		"myEdition sensorCrum fetchNextAfterTriggeringRecorders: myFinder with: NULL."
		DiskManager consistent: 1 with:
			[myEdition := NULL.
			self thingToDo. "stop making sure the edition sticks around"
			self diskUpdate]].
	^false!
*/
}
public NorthRecorderChecker(BeEdition edition, PropFinder finder) {
	super();
	myEdition = edition;
	myFinder = finder;
	newShepherd();
/*
udanax-top.st:729:NorthRecorderChecker methodsFor: 'create'!
create: edition {BeEdition} with: finder {PropFinder}
	super create.
	myEdition := edition.
	myFinder := finder.
	self newShepherd.!
*/
}
public NorthRecorderChecker(Rcvr receiver) {
	super(receiver);
	myEdition = (BeEdition) receiver.receiveHeaper();
	myFinder = (PropFinder) receiver.receiveHeaper();
/*
udanax-top.st:738:NorthRecorderChecker methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myEdition _ receiver receiveHeaper.
	myFinder _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myEdition);
	xmtr.sendHeaper(myFinder);
/*
udanax-top.st:743:NorthRecorderChecker methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myEdition.
	xmtr sendHeaper: myFinder.!
*/
}
public static AgendaItem make(BeEdition edition, PropFinder finder) {
	return new NorthRecorderChecker(edition, finder);
/*
udanax-top.st:757:NorthRecorderChecker class methodsFor: 'create'!
{AgendaItem} make: edition {BeEdition} with: finder {PropFinder}
	^self create: edition with: finder!
*/
}
public NorthRecorderChecker() {
/*

Generated during transformation
*/
}
}
