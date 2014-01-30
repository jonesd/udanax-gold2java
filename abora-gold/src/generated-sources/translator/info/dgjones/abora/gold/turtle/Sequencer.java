/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.turtle;

import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.turtle.Sequencer;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * An AgendaItem composed of two other AgendaItems.  Used for when all of the first needs to
 * be done before any of the second may be done.
 * My stepping action consists of stepping myFirst.  When it is exhausted, I destroy it and
 * then start stepping myRest
 */
public class Sequencer extends AgendaItem {

	protected AgendaItem myFirst;
	protected AgendaItem myRest;
/*
udanax-top.st:1176:
AgendaItem subclass: #Sequencer
	instanceVariableNames: '
		myFirst {AgendaItem | NULL}
		myRest {AgendaItem}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-turtle'!
*/
/*
udanax-top.st:1182:
Sequencer comment:
'An AgendaItem composed of two other AgendaItems.  Used for when all of the first needs to be done before any of the second may be done.
My stepping action consists of stepping myFirst.  When it is exhausted, I destroy it and then start stepping myRest'!
*/
/*
udanax-top.st:1186:
(Sequencer getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:1239:
Sequencer class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:1242:
(Sequencer getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Sequencer.class).setAttributes( new Set().add("SHEPHERDPATRIARCH").add("COPY").add("LOCKED").add("NOTATYPE").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Sequencer(AgendaItem first, AgendaItem rest) {
	super();
	myFirst = first;
	myRest = rest;
	first.rememberYourself();
	rest.rememberYourself();
	newShepherd();
/*
udanax-top.st:1191:Sequencer methodsFor: 'protected: creation'!
create: first {AgendaItem} with: rest {AgendaItem}
	super create.
	myFirst _ first.
	myRest _ rest.
	first rememberYourself.
	rest rememberYourself.
	self newShepherd.!
*/
}
public boolean step() {
	if (myFirst == null) {
		return myRest.step();
	}
	else {
		if ( ! (myFirst.step())) {
			AboraBlockSupport.enterConsistent(2);
			try {
				myFirst.destroy();
				myFirst = null;
				diskUpdate();
			}
			finally {
				AboraBlockSupport.exitConsistent();
			}
		}
		return true;
	}
/*
udanax-top.st:1202:Sequencer methodsFor: 'accessing'!
{BooleanVar} step
	
	myFirst == NULL
		ifTrue: [^myRest step]
		ifFalse: 
			[myFirst step
				ifFalse: 
					[DiskManager consistent: 2 with: 
						[myFirst destroy.
						myFirst _ NULL.
						self diskUpdate]].
			^true]!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(3);
	try {
		if (myFirst != null) {
			myFirst.destroy();
		}
		myRest.destroy();
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:1217:Sequencer methodsFor: 'creation'!
{void} dismantle
	
	DiskManager consistent: 3 with: 
		[myFirst ~~ NULL 
			ifTrue:
				[myFirst destroy].
		myRest destroy.
		super dismantle]!
*/
}
public Sequencer(Rcvr receiver) {
	super(receiver);
	myFirst = (AgendaItem) receiver.receiveHeaper();
	myRest = (AgendaItem) receiver.receiveHeaper();
/*
udanax-top.st:1228:Sequencer methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myFirst _ receiver receiveHeaper.
	myRest _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myFirst);
	xmtr.sendHeaper(myRest);
/*
udanax-top.st:1233:Sequencer methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myFirst.
	xmtr sendHeaper: myRest.!
*/
}
public static AgendaItem make(AgendaItem first, AgendaItem rest) {
	AboraBlockSupport.enterConsistent(3);
	try {
		return new Sequencer(first, rest);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:1247:Sequencer class methodsFor: 'creation'!
{AgendaItem} make: first {AgendaItem} with: rest {AgendaItem}
	DiskManager consistent: 3 with:
		[^self create: first with: rest]!
*/
}
public Sequencer() {
/*

Generated during transformation
*/
}
}
