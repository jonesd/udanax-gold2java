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
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.turtle.ActualPropChanger;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.turtle.HeightChanger;
import info.dgjones.abora.gold.turtle.PropChanger;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * Used to propagate some prop(erty) change rootwards in some canopy.  Each step propagates
 * it one step parentwards, until it gets to a local root or no further propagation in
 * necessary.
 */
public class PropChanger extends AgendaItem {

	protected CanopyCrum myCrum;
/*
udanax-top.st:761:
AgendaItem subclass: #PropChanger
	instanceVariableNames: 'myCrum {CanopyCrum | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-turtle'!
*/
/*
udanax-top.st:765:
PropChanger comment:
'Used to propagate some prop(erty) change rootwards in some canopy.  Each step propagates it one step parentwards, until it gets to a local root or no further propagation in necessary.'!
*/
/*
udanax-top.st:767:
(PropChanger getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #COPY; add: #DEFERRED; add: #SHEPHERD.PATRIARCH; add: #DEFERRED.LOCKED; yourself)!
*/
/*
udanax-top.st:839:
PropChanger class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:842:
(PropChanger getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #COPY; add: #DEFERRED; add: #SHEPHERD.PATRIARCH; add: #DEFERRED.LOCKED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PropChanger.class).setAttributes( new Set().add("COPY").add("DEFERRED").add("SHEPHERDPATRIARCH").add("DEFERREDLOCKED"));
/*

Generated during transformation: AddMethod
*/
}
public CanopyCrum fetchCrum() {
	return myCrum;
/*
udanax-top.st:772:PropChanger methodsFor: 'protected: accessing'!
{CanopyCrum | NULL} fetchCrum
	^myCrum!
*/
}
/**
 * Move our placeholding finger to a new crum, updating refcounts accordingly
 */
public void setCrum(CanopyCrum aCrum) {
	/* atomically (though we've probably already gone nuclear)
		If there is a new crum
			bump its refcount.
		If there is an old crum
			unbump its refcount.
		Remember the new crum. */
	AboraBlockSupport.enterConsistent(3);
	try {
		if (aCrum != null) {
			aCrum.addPointer(this);
		}
		if (myCrum != null) {
			myCrum.removePointer(this);
		}
		myCrum = aCrum;
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:776:PropChanger methodsFor: 'protected: accessing'!
{void} setCrum: aCrum {CanopyCrum | NULL}
	"Move our placeholding finger to a new crum, updating refcounts accordingly"
	
	| |
	
	"atomically (though we've probably already gone nuclear)
		If there is a new crum
			bump its refcount.
		If there is an old crum
			unbump its refcount.
		Remember the new crum."
		
	DiskManager consistent: 3 with:
		[aCrum ~~ NULL ifTrue:
			[aCrum addPointer: self].
		myCrum ~~ NULL ifTrue:
			[myCrum removePointer: self].
		myCrum := aCrum.
		self diskUpdate].!
*/
}
/**
 * propagate some prop(erty) change one step parentwards, until it gets to a local root or no
 * further propagation in necessary.
 */
public boolean step() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:798:PropChanger methodsFor: 'accessing'!
{BooleanVar} step
	"propagate some prop(erty) change one step parentwards, until it gets to a local root or no further propagation in necessary."
	
	self subclassResponsibility!
*/
}
public PropChanger(CanopyCrum crum) {
	super();
	myCrum = crum;
	if (myCrum == null) {
		myCrum.addPointer(this);
	}
/*
udanax-top.st:805:PropChanger methodsFor: 'creation'!
create: crum {CanopyCrum | NULL}
	super create.
	myCrum _ crum.
	myCrum == NULL ifTrue:
		[myCrum addPointer: self].!
*/
}
/**
 * Special constructor for becoming this class
 */
public PropChanger(CanopyCrum crum, int hash) {
	super(hash);
	myCrum = crum;
	/* I don't 'myCrum addPointer: self' because, in becoming, my old self is presumed to already have pointed at the crum */
/*
udanax-top.st:812:PropChanger methodsFor: 'creation'!
create: crum {CanopyCrum | NULL} with: hash {UInt32}
	"Special constructor for becoming this class"
	super create: hash.
	myCrum _ crum.
	"I don't 'myCrum addPointer: self' because, in becoming, my old self is presumed to already have pointed at the crum"!
*/
}
public void dismantle() {
	AboraBlockSupport.enterConsistent(2);
	try {
		if (myCrum != null) {
			myCrum.removePointer(this);
			myCrum = null;
		}
		super.dismantle();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:819:PropChanger methodsFor: 'creation'!
{void} dismantle
	DiskManager consistent: 2 with:
		[myCrum ~~ NULL
			ifTrue:
				[myCrum removePointer: self.
				myCrum _ NULL].
		super dismantle]!
*/
}
public PropChanger(Rcvr receiver) {
	super(receiver);
	myCrum = (CanopyCrum) receiver.receiveHeaper();
/*
udanax-top.st:830:PropChanger methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCrum _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myCrum);
/*
udanax-top.st:834:PropChanger methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCrum.!
*/
}
public static PropChanger height(CanopyCrum crum) {
	AboraBlockSupport.enterConsistent(3);
	try {
		return new HeightChanger(crum);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:847:PropChanger class methodsFor: 'creation'!
{PropChanger} height: crum {CanopyCrum | NULL}
	
	DiskManager consistent: 3 with:
		[^HeightChanger create: crum]!
*/
}
public static PropChanger make(CanopyCrum crum) {
	AboraBlockSupport.enterConsistent(2);
	try {
		return new ActualPropChanger(crum);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:852:PropChanger class methodsFor: 'creation'!
make: crum {CanopyCrum | NULL}
	
	DiskManager consistent: 2 with:
		[^ActualPropChanger create: crum]!
*/
}
/*
udanax-top.st:859:PropChanger class methodsFor: 'smalltalk: suspended'!
make: crum {CanopyCrum | NULL} with: change {PropChange}
	self suspended.
	self thingToDo. " Separate out different things to be propagatated into different PropChanger-like classes."
	
	DiskManager consistent: 3 with:
		[^ActualPropChanger create: crum with: change]!
*/
public PropChanger() {
/*

Generated during transformation
*/
}
}
