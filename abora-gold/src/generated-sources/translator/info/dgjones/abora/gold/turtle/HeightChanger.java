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
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.turtle.HeightChanger;
import info.dgjones.abora.gold.turtle.PropChanger;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * Used to propagate some prop(erty) change rootwards in some canopy.  Each step propagates
 * it one step parentwards, until it gets to a local root or no further propagation in
 * necessary.
 */
public class HeightChanger extends PropChanger {

	protected PropChange myChange;
/*
udanax-top.st:918:
PropChanger subclass: #HeightChanger
	instanceVariableNames: 'myChange {PropChange}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-turtle'!
*/
/*
udanax-top.st:922:
HeightChanger comment:
'Used to propagate some prop(erty) change rootwards in some canopy.  Each step propagates it one step parentwards, until it gets to a local root or no further propagation in necessary.'!
*/
/*
udanax-top.st:924:
(HeightChanger getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #LOCKED; add: #COPY; yourself)!
*/
/*
udanax-top.st:972:
HeightChanger class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:975:
(HeightChanger getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #LOCKED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HeightChanger.class).setAttributes( new Set().add("CONCRETE").add("LOCKED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public HeightChanger(CanopyCrum crum) {
	super(crum);
	newShepherd();
/*
udanax-top.st:929:HeightChanger methodsFor: 'creation'!
create: crum {CanopyCrum}
	super create: crum.
	self newShepherd.!
*/
}
/**
 * Special constructor for becoming this class
 */
public HeightChanger(CanopyCrum crum, int hash, FlockInfo info) {
	super(crum, hash);
	flockInfo(info);
	diskUpdate();
/*
udanax-top.st:934:HeightChanger methodsFor: 'creation'!
create: crum {CanopyCrum | NULL} with: hash {UInt32} with: info {FlockInfo}
	"Special constructor for becoming this class"
	super create: crum with: hash.
	self flockInfo: info.
	self diskUpdate.!
*/
}
public boolean step() {
	/* If I'm done
		Stop me before I step again!!.
	atomically
		Do one step of height recalculation.
			If more needs to be done, step rootward.  (myCrum is set to NULL if I am the root.)
			else I'm done.  Remember it by setting myCrum to NULL
	return a flag saying whether I'm done */
	if (fetchCrum() == null) {
		return false;
	}
	AboraBlockSupport.enterConsistent(3);
	try {
		if (fetchCrum().changeHeight()) {
			setCrum(fetchCrum().fetchParent());
		}
		else {
			setCrum(null);
		}
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	return fetchCrum() != null;
/*
udanax-top.st:943:HeightChanger methodsFor: 'accessing'!
{BooleanVar} step
	| |
	"If I'm done
		Stop me before I step again!!.
	atomically
		Do one step of height recalculation.
			If more needs to be done, step rootward.  (myCrum is set to NULL if I am the root.)
			else I'm done.  Remember it by setting myCrum to NULL
	return a flag saying whether I'm done"
	
	self fetchCrum == NULL
		ifTrue: [^false].
	DiskManager consistent: 3 with:
		[self fetchCrum changeHeight
			ifTrue: [self setCrum: self fetchCrum fetchParent]
			ifFalse: [self setCrum: NULL]].
	^self fetchCrum ~~ NULL!
*/
}
public HeightChanger(Rcvr receiver) {
	super(receiver);
	myChange = (PropChange) receiver.receiveHeaper();
/*
udanax-top.st:963:HeightChanger methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myChange _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myChange);
/*
udanax-top.st:967:HeightChanger methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myChange.!
*/
}
public static PropChanger make(CanopyCrum crum, PropChange change) {
	Someone.knownBug();
	/* BOGUS */
	AboraBlockSupport.enterConsistent(3);
	try {
		return new HeightChanger(crum);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:980:HeightChanger class methodsFor: 'creation'!
make: crum {CanopyCrum} with: change {PropChange unused}
	
	self knownBug.	"BOGUS"
	DiskManager consistent: 3 with:
		[^self create: crum]!
*/
}
public HeightChanger() {
/*

Generated during transformation
*/
}
}
