/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.turtle;

import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.turtle.ActualPropChanger;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.turtle.PropChanger;
import info.dgjones.abora.gold.turtle.RecorderHoister;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * NOT.A.TYPE I exist to hoist myCargo (a set of recorder fossils) up the Sensor canopy as
 * far as it needs to go, as well as to propogate the props resulting from the planting of
 * these recorders.  When I no longer have any cargo to hoist, I devolve into an
 * ActualPropChanger
 * I assume that RecorderCheckers do their southward walk in a single step, so I can hoist
 * recorders by an algorithm that would occasionally cause a recorder to be missed if
 * RecorderCheckers were incremental.
 */
public class RecorderHoister extends PropChanger {

	protected MuSet myCargo;
/*
udanax-top.st:986:
PropChanger subclass: #RecorderHoister
	instanceVariableNames: 'myCargo {MuSet of: TransclusionFossil}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-turtle'!
*/
/*
udanax-top.st:990:
RecorderHoister comment:
' NOT.A.TYPE I exist to hoist myCargo (a set of recorder fossils) up the Sensor canopy as far as it needs to go, as well as to propogate the props resulting from the planting of these recorders.  When I no longer have any cargo to hoist, I devolve into an ActualPropChanger
I assume that RecorderCheckers do their southward walk in a single step, so I can hoist recorders by an algorithm that would occasionally cause a recorder to be missed if RecorderCheckers were incremental.'!
*/
/*
udanax-top.st:994:
(RecorderHoister getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #(MAY.BECOME ActualPropChanger ); add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:1073:
RecorderHoister class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:1076:
(RecorderHoister getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #(MAY.BECOME ActualPropChanger ); add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RecorderHoister.class).setAttributes( new Set().add("LOCKED").add("COPY").add( new String[]
	{"MAYBECOME", "ActualPropChanger"}).add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public RecorderHoister(CanopyCrum crum, MuSet aSetOfRecorders) {
	super(crum);
	myCargo = aSetOfRecorders;
	newShepherd();
/*
udanax-top.st:999:RecorderHoister methodsFor: 'creation'!
create: crum {CanopyCrum} with: aSetOfRecorders {MuSet of: RecorderFossil}
	super create: crum.
	myCargo _ aSetOfRecorders.
	self newShepherd.!
*/
}
public boolean step() {
	/* See class comment for a constraint I impose on another class.
	
	If I'm done
		Stop me before I step again!!.
	atomically
		Do one step of property changing (and/or height recalculation until that's moved to HeightChanger). 
			If more needs to be done, step rootward.  (myCrum is set to NULL if I am the root.)
			else I'm done.  Remember it by setting myCrum to NULL
	return a flag saying whether I'm done */
	Someone.thingToDo();
	/* update comment after we move height calculation to HeightChanger>>step */
	if (fetchCrum() == null) {
		return false;
	}
	AboraBlockSupport.enterConsistent(3);
	try {
		CanopyCrum crum;
		boolean propsChangedFlag;
		crum = fetchCrum().fetchParent();
		propsChangedFlag = fetchCrum().changeCanopy();
		/* All the updating of myPropJoint that's needed even though I hoist recorders into my parent below, since hoisting cannot change what myPropJoint needs to be. */
		setCrum(crum);
		if (crum == null) {
			return false;
		}
		myCargo.restrictTo(((SensorCrum) crum.fetchChild1()).recorders());
		myCargo.restrictTo(((SensorCrum) crum.fetchChild2()).recorders());
		diskUpdate();
		if (myCargo.isEmpty()) {
			int hash;
			FlockInfo info;
			if ( ! (propsChangedFlag)) {
				setCrum(null);
				return false;
			}
			myCargo.destroy();
			/* Normally done by destruct, but here we do it directly because we're about to become something */
			hash = hashForEqual();
			info = fetchInfo();
			/* TODO newBecome */
			new ActualPropChanger(crum, hash, info);
			/* the special purpose constructor will not do a 'crum->addPointer(this)' so we don't have to undo it */
			return true;
		}
		/* If we reach this point, we have cargo to hoist. */
		((SensorCrum) crum.fetchChild1()).removeRecorders(myCargo.asImmuSet());
		((SensorCrum) crum.fetchChild2()).removeRecorders(myCargo.asImmuSet());
		myCargo.wipeAll(((SensorCrum) crum).recorders());
		if (myCargo.isEmpty()) {
			if ( ! (propsChangedFlag)) {
				setCrum(null);
			}
			return propsChangedFlag;
		}
		else {
			((SensorCrum) crum).installRecorders(myCargo.asImmuSet());
			crum.diskUpdate();
		}
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	return true;
/*
udanax-top.st:1007:RecorderHoister methodsFor: 'accessing'!
{BooleanVar} step
	| |
	"See class comment for a constraint I impose on another class.
	
	If I'm done
		Stop me before I step again!!.
	atomically
		Do one step of property changing (and/or height recalculation until that's moved to HeightChanger). 
			If more needs to be done, step rootward.  (myCrum is set to NULL if I am the root.)
			else I'm done.  Remember it by setting myCrum to NULL
	return a flag saying whether I'm done"
	self thingToDo.	"update comment after we move height calculation to HeightChanger>>step"	
	self fetchCrum == NULL
		ifTrue: [^false].
	DiskManager consistent: 3 with:
		[ | crum {CanopyCrum | NULL} propsChangedFlag {BooleanVar} |
		crum := self fetchCrum fetchParent.
		propsChangedFlag := self fetchCrum changeCanopy.
		"All the updating of myPropJoint that's needed even though I hoist recorders into my parent below, since hoisting cannot change what myPropJoint needs to be."
		self setCrum: crum.
		crum == NULL ifTrue:
			[^false].
		myCargo restrictTo: (crum fetchChild1 cast: SensorCrum) recorders;
			       restrictTo: (crum fetchChild2 cast: SensorCrum) recorders.
		self diskUpdate.
		myCargo isEmpty ifTrue:
			[| hash {UInt32} 
			   info {FlockInfo} |
			propsChangedFlag ifFalse:
				[self setCrum: NULL.
				^false].
			myCargo destroy.
			"Normally done by destruct, but here we do it directly because we're about to become something"
			hash _ self hashForEqual.
			info _ self fetchInfo.
			(ActualPropChanger new.Become: self)
				create: crum
				with: hash
				with: info.
			"the special purpose constructor will not do a 'crum->addPointer(this)' so we don't have to undo it"
			^true].
		"If we reach this point, we have cargo to hoist."
		(crum fetchChild1 cast: SensorCrum) removeRecorders: myCargo asImmuSet.
		(crum fetchChild2 cast: SensorCrum) removeRecorders: myCargo asImmuSet.
		myCargo wipeAll: (crum cast: SensorCrum) recorders.
		myCargo isEmpty ifTrue:
			[propsChangedFlag ifFalse:
				[self setCrum: NULL].
			^propsChangedFlag]
		ifFalse:
			[(crum cast: SensorCrum) installRecorders: myCargo asImmuSet.
			crum diskUpdate]].
	^true!
*/
}
public RecorderHoister(Rcvr receiver) {
	super(receiver);
	myCargo = (MuSet) receiver.receiveHeaper();
/*
udanax-top.st:1064:RecorderHoister methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCargo _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myCargo);
/*
udanax-top.st:1068:RecorderHoister methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCargo.!
*/
}
/**
 * Create a RecorderHoister.
 */
public static AgendaItem make(CanopyCrum crum, ScruSet aSetOfRecorders) {
	if (aSetOfRecorders.isEmpty()) {
		return Agenda.make();
	}
	AboraBlockSupport.enterConsistent(1);
	try {
		return new RecorderHoister(crum, aSetOfRecorders.asMuSet());
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:1081:RecorderHoister class methodsFor: 'creation'!
{AgendaItem} make: crum {CanopyCrum} with:  aSetOfRecorders {ScruSet of: RecorderFossil}
	"Create a RecorderHoister."
	
	aSetOfRecorders isEmpty ifTrue:
		[^Agenda make].
	DiskManager consistent: 1 with:
		[^self create: crum with: aSetOfRecorders asMuSet]!
*/
}
public RecorderHoister() {
/*

Generated during transformation
*/
}
}
