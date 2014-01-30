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
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.brange2.UpdateTransitiveSuperClubIDs;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * This carries on the updating of transitive superclass IDs for the given club.
 */
public class UpdateTransitiveSuperClubIDs extends AgendaItem {

	protected MuSet myClubs;
	protected BeGrandMap myGrandMap;
/*
udanax-top.st:1410:
AgendaItem subclass: #UpdateTransitiveSuperClubIDs
	instanceVariableNames: '
		myClubs {MuSet of: BeClub | NULL}
		myGrandMap {BeGrandMap}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-brange2'!
*/
/*
udanax-top.st:1416:
UpdateTransitiveSuperClubIDs comment:
'This carries on the updating of transitive superclass IDs for the given club.'!
*/
/*
udanax-top.st:1418:
(UpdateTransitiveSuperClubIDs getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:1455:
UpdateTransitiveSuperClubIDs class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:1458:
(UpdateTransitiveSuperClubIDs getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(UpdateTransitiveSuperClubIDs.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean step() {
	if ( ! (myClubs.isEmpty())) {
		AboraBlockSupport.enterConsistent(2);
		try {
			BeClub club;
			Stepper stomp;
			club = (BeClub) (stomp = myClubs.stepper()).fetch();
			stomp.destroy();
			Object currentGrandMapOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentGrandMap, myGrandMap);
			try {
				club.updateTransitiveSuperClubIDs();
			}
			finally {
				AboraBlockSupport.exitFluidBindDuring(CurrentGrandMap, currentGrandMapOldValue);
			}
			myClubs.remove(club);
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return ! myClubs.isEmpty();
/*
udanax-top.st:1423:UpdateTransitiveSuperClubIDs methodsFor: 'accessing'!
{BooleanVar} step
	myClubs isEmpty ifFalse:
		[DiskManager consistent: 2 with:
			[| club {BeClub} stomp {Stepper} |
			club := (stomp := myClubs stepper) fetch cast: BeClub.
			stomp destroy.
			CurrentGrandMap fluidBind: myGrandMap during: [club updateTransitiveSuperClubIDs].
			myClubs remove: club.
			self diskUpdate]].
	^ myClubs isEmpty not!
*/
}
public UpdateTransitiveSuperClubIDs(MuSet clubs, BeGrandMap grandMap) {
	super();
	myClubs = clubs;
	myGrandMap = grandMap;
	newShepherd();
/*
udanax-top.st:1436:UpdateTransitiveSuperClubIDs methodsFor: 'protected: creation'!
create: clubs {MuSet of: BeClub} with: grandMap {BeGrandMap}
	super create.
	myClubs := clubs.
	myGrandMap := grandMap.
	self newShepherd.!
*/
}
public UpdateTransitiveSuperClubIDs(Rcvr receiver) {
	super(receiver);
	myClubs = (MuSet) receiver.receiveHeaper();
	myGrandMap = (BeGrandMap) receiver.receiveHeaper();
/*
udanax-top.st:1444:UpdateTransitiveSuperClubIDs methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myClubs _ receiver receiveHeaper.
	myGrandMap _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myClubs);
	xmtr.sendHeaper(myGrandMap);
/*
udanax-top.st:1449:UpdateTransitiveSuperClubIDs methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myClubs.
	xmtr sendHeaper: myGrandMap.!
*/
}
public static UpdateTransitiveSuperClubIDs make(MuSet clubs, BeGrandMap grandMap) {
	return new UpdateTransitiveSuperClubIDs(clubs, grandMap);
/*
udanax-top.st:1463:UpdateTransitiveSuperClubIDs class methodsFor: 'creation'!
make: clubs {MuSet of: BeClub} with: grandMap {BeGrandMap}
	^ self create: clubs with: grandMap!
*/
}
public UpdateTransitiveSuperClubIDs() {
/*

Generated during transformation
*/
}
}
