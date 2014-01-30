/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.be.basic.BePlaceHolder;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.detect.FeFillDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeActualPlaceHolder;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FePlaceHolder;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Actually has a persistent individual PlaceHolder on the Server, or used to, and now has a
 * pointer to the rangeElement it became.
 */
public class FeActualPlaceHolder extends FePlaceHolder {

	protected BeRangeElement myRangeElement;
/*
udanax-top.st:21857:
FePlaceHolder subclass: #FeActualPlaceHolder
	instanceVariableNames: 'myRangeElement {BeRangeElement}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:21861:
FeActualPlaceHolder comment:
'Actually has a persistent individual PlaceHolder on the Server, or used to, and now has a pointer to the rangeElement it became.'!
*/
/*
udanax-top.st:21863:
(FeActualPlaceHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeActualPlaceHolder.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public FeRangeElement again() {
	Dean.shouldImplement();
	/* This must hold onto an FeRangeElement so that the label is properly maintained. */
	if (myRangeElement instanceof BePlaceHolder) {
		BePlaceHolder pl = (BePlaceHolder) myRangeElement;
		return this
		/* No change. */
		;
	}
	else {
		return myRangeElement.makeFe(null);
	}
/*
udanax-top.st:21868:FeActualPlaceHolder methodsFor: 'client accessing'!
{FeRangeElement} again
Dean shouldImplement.  "This must hold onto an FeRangeElement so that the label is properly maintained."
	myRangeElement cast: BePlaceHolder into: [:pl | ^self  "No change."]
		others: [^myRangeElement makeFe: NULL].
	^NULL "fodder"!
*/
}
public boolean canMakeIdentical(FeRangeElement newIdentity) {
	if ( ! (isIdentical(newIdentity))) {
		throw new UnimplementedException();
	}
	return true;
/*
udanax-top.st:21875:FeActualPlaceHolder methodsFor: 'client accessing'!
{BooleanVar} canMakeIdentical: newIdentity {FeRangeElement}
	(self isIdentical: newIdentity) ifFalse:
		[self unimplemented].
	^true!
*/
}
/**
 * Consolidate this PlaceHolder to the newIdentity.  Return true if successful.
 */
public void makeIdentical(FeRangeElement newIdentity) {
	/* Check permissions
		and forward the operation after coercing the newIdentity
		 to a persistent RangeElement. */
	/* myRangeElement will tell me to forward to another RangeElement. */
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority(owner()))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_OWNER);
	}
	myRangeElement.makeIdentical(newIdentity.getOrMakeBe());
/*
udanax-top.st:21881:FeActualPlaceHolder methodsFor: 'client accessing'!
{void} makeIdentical: newIdentity {FeRangeElement}
	"Consolidate this PlaceHolder to the newIdentity.  Return true if successful."
	
	"Check permissions
		and forward the operation after coercing the newIdentity
		 to a persistent RangeElement."
		
	"myRangeElement will tell me to forward to another RangeElement."
	
	(CurrentKeyMaster fluidGet hasAuthority: self owner) ifFalse:
		[Heaper BLAST: #MustBeOwner].
	myRangeElement makeIdentical: newIdentity getOrMakeBe!
*/
}
/**
 * MyBeRangeElement will know it.
 */
public ID owner() {
	return myRangeElement.owner();
/*
udanax-top.st:21894:FeActualPlaceHolder methodsFor: 'client accessing'!
{ID} owner
	"MyBeRangeElement will know it."
	
	^myRangeElement owner!
*/
}
public void removeFillDetector(FeFillDetector detector) {
	if ( ! (Heaper.isDestructed(myRangeElement))) {
		if (myRangeElement instanceof BePlaceHolder) {
			BePlaceHolder p = (BePlaceHolder) myRangeElement;
			p.removeDetector(detector);
		}
	}
/*
udanax-top.st:21899:FeActualPlaceHolder methodsFor: 'client accessing'!
{void} removeFillDetector: detector {FeFillDetector}
	(Heaper isDestructed: myRangeElement) ifFalse:
		[myRangeElement cast: BePlaceHolder into: [ :p |
			p removeDetector: detector]
		others: []]!
*/
}
public BeRangeElement fetchBe() {
	return myRangeElement;
/*
udanax-top.st:21908:FeActualPlaceHolder methodsFor: 'server accessing'!
{BeRangeElement | NULL} fetchBe
	^myRangeElement!
*/
}
/**
 * myRangeElement has become something else.  Forward to the new thing.
 */
public void forwardTo(BeRangeElement element) {
	myRangeElement.removeFeRangeElement(this);
	myRangeElement = element;
	myRangeElement.addFeRangeElement(this);
/*
udanax-top.st:21912:FeActualPlaceHolder methodsFor: 'server accessing'!
{void} forwardTo: element {BeRangeElement}
	"myRangeElement has become something else.  Forward to the new thing."
	
	myRangeElement removeFeRangeElement: self.
	myRangeElement _ element.
	myRangeElement addFeRangeElement: self.!
*/
}
public BeRangeElement getOrMakeBe() {
	return myRangeElement;
/*
udanax-top.st:21919:FeActualPlaceHolder methodsFor: 'server accessing'!
{BeRangeElement} getOrMakeBe
	^myRangeElement!
*/
}
public FeActualPlaceHolder(BeRangeElement be) {
	super();
	myRangeElement = be;
/*
udanax-top.st:21925:FeActualPlaceHolder methodsFor: 'private: create'!
create: be {BeRangeElement}
	super create.
	myRangeElement := be.!
*/
}
public void destruct() {
	myRangeElement.removeFeRangeElement(this);
	super.destruct();
/*
udanax-top.st:21932:FeActualPlaceHolder methodsFor: 'destruct'!
{void} destruct
	myRangeElement removeFeRangeElement: self.
	super destruct.!
*/
}
public FeActualPlaceHolder() {
/*

Generated during transformation
*/
}
public FeActualPlaceHolder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
