/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.brange2;

import info.dgjones.abora.gold.be.basic.BeClub;
import info.dgjones.abora.gold.brange2.UpdateTransitiveMemberIDs;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * This carries on the updating of transitive member IDs for the given club.
 */
public class UpdateTransitiveMemberIDs extends AgendaItem {

	protected MuSet myClubs;
/*
udanax-top.st:1359:
AgendaItem subclass: #UpdateTransitiveMemberIDs
	instanceVariableNames: 'myClubs {MuSet of: BeClub}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-brange2'!
*/
/*
udanax-top.st:1363:
UpdateTransitiveMemberIDs comment:
'This carries on the updating of transitive member IDs for the given club.'!
*/
/*
udanax-top.st:1365:
(UpdateTransitiveMemberIDs getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:1399:
UpdateTransitiveMemberIDs class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:1402:
(UpdateTransitiveMemberIDs getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(UpdateTransitiveMemberIDs.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean step() {
	if ( ! (myClubs.isEmpty())) {
		AboraBlockSupport.enterConsistent(5);
		try {
			BeClub club;
			Stepper stomp;
			club = (BeClub) (stomp = myClubs.stepper()).fetch();
			stomp.destroy();
			club.updateTransitiveMemberIDs();
			myClubs.remove(club);
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return ! myClubs.isEmpty();
/*
udanax-top.st:1370:UpdateTransitiveMemberIDs methodsFor: 'accessing'!
{BooleanVar} step
	myClubs isEmpty ifFalse:
		[DiskManager consistent: 5 with:
			[| club {BeClub} stomp {Stepper} |
			club := (stomp := myClubs stepper) fetch cast: BeClub.
			stomp destroy.
			club updateTransitiveMemberIDs.
			myClubs remove: club.
			self diskUpdate]].
	^ myClubs isEmpty not!
*/
}
public UpdateTransitiveMemberIDs(MuSet clubs) {
	super();
	myClubs = clubs;
	newShepherd();
/*
udanax-top.st:1383:UpdateTransitiveMemberIDs methodsFor: 'protected: creation'!
create: clubs {MuSet of: BeClub}
	super create.
	myClubs := clubs.
	self newShepherd.!
*/
}
public UpdateTransitiveMemberIDs(Rcvr receiver) {
	super(receiver);
	myClubs = (MuSet) receiver.receiveHeaper();
/*
udanax-top.st:1390:UpdateTransitiveMemberIDs methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myClubs _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myClubs);
/*
udanax-top.st:1394:UpdateTransitiveMemberIDs methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myClubs.!
*/
}
public static UpdateTransitiveMemberIDs make(MuSet clubs) {
	return new UpdateTransitiveMemberIDs(clubs);
/*
udanax-top.st:1407:UpdateTransitiveMemberIDs class methodsFor: 'creation'!
make: clubs {MuSet of: BeClub}
	^ self create: clubs!
*/
}
public UpdateTransitiveMemberIDs() {
/*

Generated during transformation
*/
}
}
