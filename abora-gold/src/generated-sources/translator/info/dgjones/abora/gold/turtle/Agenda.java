/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.turtle;

import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * An AgendaItem composed of other AgendaItems.  My stepping action consists of stepping one
 * of my component items.  When I exhaust a component item, I unregister and destroy it.
 * Note: The order in which I select a component item is currently unspecified and
 * uncontrolled (depending on "MuSet::stepper()").  Eventually, it may make sense for me to
 * use the Escalator Algorithm to do prioritized scheduling.
 * Empty Agendas are also made as do-nothing AgendaItems.  The currently get duely get
 * scheduled, stepped, and unscheduled.  A possible optimization would be to avoid scheduling
 * do-nothing AgendaItems.
 */
public class Agenda extends AgendaItem {

	protected MuSet myToDoList;
/*
udanax-top.st:401:
AgendaItem subclass: #Agenda
	instanceVariableNames: 'myToDoList {MuSet of: AgendaItem}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-turtle'!
*/
/*
udanax-top.st:405:
Agenda comment:
'An AgendaItem composed of other AgendaItems.  My stepping action consists of stepping one of my component items.  When I exhaust a component item, I unregister and destroy it.
Note: The order in which I select a component item is currently unspecified and uncontrolled (depending on "MuSet::stepper()").  Eventually, it may make sense for me to use the Escalator Algorithm to do prioritized scheduling.
Empty Agendas are also made as do-nothing AgendaItems.  The currently get duely get scheduled, stepped, and unscheduled.  A possible optimization would be to avoid scheduling do-nothing AgendaItems.'!
*/
/*
udanax-top.st:411:
(Agenda getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:490:
Agenda class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:493:
(Agenda getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Agenda.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * By registering the item, we ensure that if we crash and reboot, the item will be
 * eventually and repeatedly stepped until step returns FALSE, provided we are registered up
 * through the Turtle.  Do NOT multiply register the same item.
 */
public void registerItem(AgendaItem item) {
	AboraBlockSupport.enterConsistent(2);
	try {
		myToDoList.introduce(item);
		/* Why did we once have a 'bug?' annotation that this introduce needs to preceed the rememberYourself? */
		item.rememberYourself();
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:416:Agenda methodsFor: 'accessing'!
{void} registerItem: item {AgendaItem}
	"By registering the item, we ensure that if we crash and reboot, the item will be eventually and repeatedly stepped until step returns FALSE, provided we are registered up through the Turtle.  Do NOT multiply register the same item."
	
	DiskManager consistent: 2 with:
		[myToDoList introduce: item.	"Why did we once have a 'bug?' annotation that this introduce needs to preceed the rememberYourself?"
		item rememberYourself.
		self diskUpdate]!
*/
}
/**
 * 'step' one of my component items.  If I return FALSE, that means there's nothing currently
 * left to do.  However, since more AgendaItems may get registered later, there may later be
 * something more for me to do, so I shouldn't necessarily be destroyed.  This creates a
 * composition problem: If an Agenda is stored as an item within another Agenda, then when
 * the outer Agenda is stepped and it in turn steps the inner Agenda, if the inner Agenda
 * returns FALSE, the outer Agenda will destroy it.  This is all legal and shouldn't be a
 * problem as long as one is aware of this behavior
 */
public boolean step() {
	AgendaItem item;
	Stepper stomp;
	/* fetch some one item from myToDOList by creating a stepper, fetching with it, and
	destroying the stepper.
	If there were no items left
		return, telling the caller that there is nothing left to do.  (We may do this repeatedly...)
	step the item.
		if it returned false
			unregister the item
			atomically
				destroy it  (nuke it?)
	return whether there are any more things to do. */
	item = (AgendaItem) (stomp = myToDoList.stepper()).fetch();
	stomp.destroy();
	Someone.thingToDo();
	/* The above code is n-squared.  It should probably be fixed up during tuning. */
	if (item == null) {
		return false;
	}
	if ( ! (item.step())) {
		unregisterItem(item);
		AboraBlockSupport.enterConsistent(2);
		try {
			item.destroy();
			Someone.thingToDo();
			/* find out if the consistent block is necessary/appropriate */
			;
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return ! myToDoList.isEmpty();
/*
udanax-top.st:424:Agenda methodsFor: 'accessing'!
{BooleanVar} step
	"'step' one of my component items.  If I return FALSE, that means there's nothing currently left to do.  However, since more AgendaItems may get registered later, there may later be something more for me to do, so I shouldn't necessarily be destroyed.  This creates a composition problem: If an Agenda is stored as an item within another Agenda, then when the outer Agenda is stepped and it in turn steps the inner Agenda, if the inner Agenda returns FALSE, the outer Agenda will destroy it.  This is all legal and shouldn't be a problem as long as one is aware of this behavior"
	
	| item {AgendaItem | NULL} stomp {Stepper} |
	
	"fetch some one item from myToDOList by creating a stepper, fetching with it, and
	destroying the stepper.
	If there were no items left
		return, telling the caller that there is nothing left to do.  (We may do this repeatedly...)
	step the item.
		if it returned false
			unregister the item
			atomically
				destroy it  (nuke it?)
	return whether there are any more things to do."
	
	item _ (stomp _ myToDoList stepper) fetch cast: AgendaItem.
	stomp destroy.
	self thingToDo.	"The above code is n-squared.  It should probably be fixed up during tuning."
	item == NULL
		ifTrue: [^false].
	item step
		ifFalse: 
			[self unregisterItem: item.
			DiskManager consistent: 2 with:
				[item destroy.
				self thingToDo.	"find out if the consistent block is necessary/appropriate"]].
	^myToDoList isEmpty not!
*/
}
/**
 * An item should be unregistered either when it is done (when 'step' returns FALSE) or when
 * it no longer represents something that needs to be done should we crash and reboot.
 * Unregistering an item which is not registered and already forgotten is legal and has no
 * effect.
 */
public void unregisterItem(AgendaItem item) {
	AboraBlockSupport.enterConsistent(2);
	try {
		myToDoList.wipe(item);
		item.forgetYourself();
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:453:Agenda methodsFor: 'accessing'!
{void} unregisterItem: item {AgendaItem}
	"An item should be unregistered either when it is done (when 'step' returns FALSE) or when it no longer represents something that needs to be done should we crash and reboot.  Unregistering an item which is not registered and already forgotten is legal and has no effect."
	
	DiskManager consistent: 2 with:
		[myToDoList wipe: item.
		item forgetYourself.
		self diskUpdate]!
*/
}
public Agenda() {
	super();
	myToDoList = MuSet.make();
	Someone.knownBug();
	/* A MuSet may become too big to fit within a snarf.  However, GrandHashSets spawn AgendaItems and force propogating consistent block counts up through anything else that uses them. */
	newShepherd();
/*
udanax-top.st:463:Agenda methodsFor: 'creation'!
create
	super create.
	myToDoList _ MuSet make.
	self knownBug.	"A MuSet may become too big to fit within a snarf.  However, GrandHashSets spawn AgendaItems and force propogating consistent block counts up through anything else that uses them."
	self newShepherd!
*/
}
public void dismantle() {
	Stepper stomper = myToDoList.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		AgendaItem each = (AgendaItem) stomper.fetch();
		if (each == null) {
			continue ;
		}
		unregisterItem(each);
		each.destroy();
	}
	stomper.destroy();
	AboraBlockSupport.enterConsistent(2);
	try {
		myToDoList.destroy();
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:470:Agenda methodsFor: 'creation'!
{void} dismantle
	
	myToDoList stepper forEach: [:each {AgendaItem} |
		self unregisterItem: each.
		each destroy].
	DiskManager consistent: 2 with: 
		[myToDoList destroy.
		super dismantle]!
*/
}
public Agenda(Rcvr receiver) {
	super(receiver);
	myToDoList = (MuSet) receiver.receiveHeaper();
/*
udanax-top.st:481:Agenda methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myToDoList _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myToDoList);
/*
udanax-top.st:485:Agenda methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myToDoList.!
*/
}
public static Agenda make() {
	Someone.thingToDo();
	/* see class comment for optimization possibility */
	AboraBlockSupport.enterConsistent(1);
	try {
		return new Agenda();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:498:Agenda class methodsFor: 'creation'!
make
	self thingToDo. "see class comment for optimization possibility"
	DiskManager consistent: 1 with:
		[^self create]!
*/
}
}
