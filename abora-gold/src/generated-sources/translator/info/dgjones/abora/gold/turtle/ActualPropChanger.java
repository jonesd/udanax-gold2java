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
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.turtle.ActualPropChanger;
import info.dgjones.abora.gold.turtle.PropChanger;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * Used to propagate some prop(erty) change rootwards in some canopy.  Each step propagates
 * it one step parentwards, until it gets to a local root or no further propagation in
 * necessary.
 */
public class ActualPropChanger extends PropChanger {

/*
udanax-top.st:867:
PropChanger subclass: #ActualPropChanger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-turtle'!
*/
/*
udanax-top.st:871:
ActualPropChanger comment:
'Used to propagate some prop(erty) change rootwards in some canopy.  Each step propagates it one step parentwards, until it gets to a local root or no further propagation in necessary.'!
*/
/*
udanax-top.st:873:
(ActualPropChanger getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #LOCKED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ActualPropChanger.class).setAttributes( new Set().add("CONCRETE").add("LOCKED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public ActualPropChanger(CanopyCrum crum) {
	super(crum);
	newShepherd();
/*
udanax-top.st:878:ActualPropChanger methodsFor: 'creation'!
create: crum {CanopyCrum}
	super create: crum.
	self newShepherd.!
*/
}
/**
 * Special constructor for becoming this class
 */
public ActualPropChanger(CanopyCrum crum, int hash, FlockInfo info) {
	super(crum, hash);
	flockInfo(info);
	diskUpdate();
/*
udanax-top.st:883:ActualPropChanger methodsFor: 'creation'!
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
		Do one step of property changing.
			If more needs to be done, step rootward.  (myCrum is set to NULL if I am the root.)
			else I'm done.  Remember it by setting myCrum to NULL
	return a flag saying whether I'm done */
	if (fetchCrum() == null) {
		return false;
	}
	AboraBlockSupport.enterConsistent(3);
	try {
		if (fetchCrum().changeCanopy()) {
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
udanax-top.st:892:ActualPropChanger methodsFor: 'accessing'!
{BooleanVar} step
	| |
	"If I'm done
		Stop me before I step again!!.
	atomically
		Do one step of property changing.
			If more needs to be done, step rootward.  (myCrum is set to NULL if I am the root.)
			else I'm done.  Remember it by setting myCrum to NULL
	return a flag saying whether I'm done"
	self fetchCrum == NULL
		ifTrue: [^false].
	DiskManager consistent: 3 with:
		[(self fetchCrum changeCanopy)
			ifTrue: [self setCrum: self fetchCrum fetchParent]
			ifFalse: [self setCrum: NULL]].
	^self fetchCrum ~~ NULL!
*/
}
public ActualPropChanger(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:912:ActualPropChanger methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:915:ActualPropChanger methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public ActualPropChanger() {
/*

Generated during transformation
*/
}
}
