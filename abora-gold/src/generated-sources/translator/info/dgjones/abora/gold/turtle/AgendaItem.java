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
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * A persistent representation of things that still need to be done.  Can think of it like a
 * persistent process record.  "schedule"ing me ensures that I will be stepped eventually,
 * and repeatedly, until step returns FALSE, even if the process should crash after I am
 * scheduled.  Scheduling me so that I am persistent may happen inside some other consistent
 * block, however I will be stepped while outside of any consistent block (The FakePacker
 * doesn''t do this yet).  Creating an AgendaItem does not imply that it is scheduled, the
 * client must explicitly schedule it as well.  Destroying it *does* ensure that it gets
 * unscheduled, though it is valid & safe to destroy one which isn''t scheduled.
 * NOTE: Right now there are no fairness guarantees (and there may never be), so all
 * AgendaItems must eventually terminate in order for other things (like the ServerLoop) to
 * be guaranteed of eventually executing
 */
public class AgendaItem extends Abraham {

/*
udanax-top.st:330:
Abraham subclass: #AgendaItem
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-turtle'!
*/
/*
udanax-top.st:334:
AgendaItem comment:
'A persistent representation of things that still need to be done.  Can think of it like a persistent process record.  "schedule"ing me ensures that I will be stepped eventually, and repeatedly, until step returns FALSE, even if the process should crash after I am scheduled.  Scheduling me so that I am persistent may happen inside some other consistent block, however I will be stepped while outside of any consistent block (The FakePacker doesn''t do this yet).  Creating an AgendaItem does not imply that it is scheduled, the client must explicitly schedule it as well.  Destroying it *does* ensure that it gets unscheduled, though it is valid & safe to destroy one which isn''t scheduled.
NOTE: Right now there are no fairness guarantees (and there may never be), so all AgendaItems must eventually terminate in order for other things (like the ServerLoop) to be guaranteed of eventually executing'!
*/
/*
udanax-top.st:338:
(AgendaItem getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #DEFERRED.LOCKED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(AgendaItem.class).setAttributes( new Set().add("DEFERRED").add("COPY").add("SHEPHERDPATRIARCH").add("DEFERREDLOCKED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * forget is protected.  This method exposes it for AgendaItems
 */
public void forgetYourself() {
	forget();
/*
udanax-top.st:343:AgendaItem methodsFor: 'accessing'!
{void} forgetYourself
	"forget is protected.  This method exposes it for AgendaItems"
	self forget!
*/
}
/**
 * remember is protected.  This method exposes it for AgendaItems
 */
public void rememberYourself() {
	remember();
/*
udanax-top.st:348:AgendaItem methodsFor: 'accessing'!
{void} rememberYourself
	"remember is protected.  This method exposes it for AgendaItems"
	
	self remember!
*/
}
/**
 * Registers me with the top level Agenda, so that I will eventually get stepped.  Also
 * causes me to be remembered.
 */
public void schedule() {
	/* Removed smalltalkOnly */
	/* for debugging */
	((DiskManager) CurrentPacker.fluidGet()).getInitialFlock().getAgenda().registerItem(this);
/*
udanax-top.st:353:AgendaItem methodsFor: 'accessing'!
{void} schedule
	"Registers me with the top level Agenda, so that I will eventually get stepped.  Also causes me to be remembered."
	
	[[self step] whileTrue] smalltalkOnly. "for debugging"
	CurrentPacker fluidGet getInitialFlock getAgenda registerItem: self!
*/
}
/**
 * Return FALSE when there's nothing left to do (at which time I should usually be
 * unregistered and destroyed, but see Agenda::step())
 */
public boolean step() {
	Someone.thingToDo();
	/* Change to return {AgendaItem (self or other) | NULL} and rename the message to fetchNextStep or the like.  If we do this, we must remember that collapsing items must be just an optimization, because they can be stepped even after returning something else. */
	throw new SubclassResponsibilityException();
/*
udanax-top.st:359:AgendaItem methodsFor: 'accessing'!
{BooleanVar} step
	"Return FALSE when there's nothing left to do (at which time I should usually be unregistered and destroyed, but see Agenda::step())"
	self thingToDo.	"Change to return {AgendaItem (self or other) | NULL} and rename the message to fetchNextStep or the like.  If we do this, we must remember that collapsing items must be just an optimization, because they can be stepped even after returning something else."
	self subclassResponsibility!
*/
}
/**
 * Unregisters me with the top level Agenda, so that I am no longer scheduled to get stepped.
 * Also causes me to be forgotten.
 */
public void unschedule() {
	((DiskManager) CurrentPacker.fluidGet()).getInitialFlock().getAgenda().unregisterItem(this);
/*
udanax-top.st:365:AgendaItem methodsFor: 'accessing'!
{void} unschedule
	"Unregisters me with the top level Agenda, so that I am no longer scheduled to get stepped.  Also causes me to be forgotten."
	
	CurrentPacker fluidGet getInitialFlock getAgenda unregisterItem: self!
*/
}
/**
 * Not so special constructor for not becoming this class
 */
public AgendaItem() {
	super();
/*
udanax-top.st:372:AgendaItem methodsFor: 'protected: creation'!
create
	"Not so special constructor for not becoming this class"
	super create!
*/
}
/**
 * Special constructor for becoming this class
 */
public AgendaItem(int hash) {
	super(hash);
/*
udanax-top.st:377:AgendaItem methodsFor: 'protected: creation'!
create: hash {UInt32}
	"Special constructor for becoming this class"
	super create: hash!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(2);
	try {
		unschedule();
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:382:AgendaItem methodsFor: 'protected: creation'!
{void} dismantle
	
	DiskManager consistent: 2 with: 
		[self unschedule.
		super dismantle]!
*/
}
/**
 * All AgendaItems use explicit deletion semantics.
 */
public void newShepherd() {
	/* ????? */
	super.newShepherd();
/*
udanax-top.st:388:AgendaItem methodsFor: 'protected: creation'!
{void} newShepherd
	"All AgendaItems use explicit deletion semantics."
	"?????"
	super newShepherd.!
*/
}
public AgendaItem(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:395:AgendaItem methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:398:AgendaItem methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
}
