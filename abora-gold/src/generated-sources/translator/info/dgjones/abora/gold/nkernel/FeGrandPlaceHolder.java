/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.detect.FeFillDetector;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeGrandPlaceHolder;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FePlaceHolder;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Fakes a PlaceHolder in the GrandMap by just remembering the key.
 */
public class FeGrandPlaceHolder extends FePlaceHolder {

	protected ID myID;
/*
udanax-top.st:21937:
FePlaceHolder subclass: #FeGrandPlaceHolder
	instanceVariableNames: 'myID {ID}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:21941:
FeGrandPlaceHolder comment:
'Fakes a PlaceHolder in the GrandMap by just remembering the key.'!
*/
/*
udanax-top.st:21943:
(FeGrandPlaceHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #OBSOLETE; add: #SMALLTALK.ONLY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeGrandPlaceHolder.class).setAttributes( new Set().add("OBSOLETE").add("SMALLTALKONLY").add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public FeRangeElement again() {
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).getOrMakeFe(myID);
/*
udanax-top.st:21948:FeGrandPlaceHolder methodsFor: 'client accessing'!
{FeRangeElement} again
	^CurrentGrandMap fluidGet getOrMakeFe: myID!
*/
}
public boolean canMakeIdentical(FeRangeElement newIdentity) {
	if ( ! (isIdentical(newIdentity))) {
		throw new UnimplementedException();
	}
	return true;
/*
udanax-top.st:21952:FeGrandPlaceHolder methodsFor: 'client accessing'!
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
		and then try storing the other guy into the grandMap. */
	Someone.thingToDo();
	/* This doesn't need to force newIdentity into a BeRangeElement. */
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority(owner()))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_OWNER);
	}
	if ( ! (((BeGrandMap) CurrentGrandMap.fluidGet()).tryIntroduce(myID, newIdentity.getOrMakeBe()))) {
		throw new AboraRuntimeException(AboraRuntimeException.CANT_MAKE_IDENTICAL);
	}
/*
udanax-top.st:21958:FeGrandPlaceHolder methodsFor: 'client accessing'!
{void} makeIdentical: newIdentity {FeRangeElement}
	"Consolidate this PlaceHolder to the newIdentity.  Return true if successful."
	"Check permissions
		and then try storing the other guy into the grandMap."
	self thingToDo.  "This doesn't need to force newIdentity into a BeRangeElement."
	
	(CurrentKeyMaster fluidGet hasAuthority: self owner)
		ifFalse: [Heaper BLAST: #MustBeOwner].
	(CurrentGrandMap fluidGet at: myID tryIntroduce: newIdentity getOrMakeBe)
		ifFalse: [Heaper BLAST: #CantMakeIdentical]!
*/
}
/**
 * Ask the GrandMap who owns this ID
 */
public ID owner() {
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).placeOwnerID(myID);
/*
udanax-top.st:21970:FeGrandPlaceHolder methodsFor: 'client accessing'!
{ID} owner
	"Ask the GrandMap who owns this ID"
	^CurrentGrandMap fluidGet placeOwnerID: myID!
*/
}
public void removeFillDetector(FeFillDetector detector) {
	throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_SET);
/*
udanax-top.st:21975:FeGrandPlaceHolder methodsFor: 'client accessing'!
{void} removeFillDetector: detector {FeFillDetector}
	Heaper BLAST: #NotInSet!
*/
}
public BeRangeElement fetchBe() {
	return null;
/*
udanax-top.st:21981:FeGrandPlaceHolder methodsFor: 'server accessing'!
{BeRangeElement | NULL} fetchBe
	^NULL!
*/
}
/**
 * Create a new persistent PlaceHolder and register it in the GrandMap.
 */
public BeRangeElement getOrMakeBe() {
	BeRangeElement result;
	Object initialOwnerOldValue = AboraBlockSupport.enterFluidBindDuring(InitialOwner, owner());
	try {
		result = ((BeGrandMap) CurrentGrandMap.fluidGet()).newPlaceHolder();
		if (((BeGrandMap) CurrentGrandMap.fluidGet()).tryIntroduce(myID, result)) {
			return result;
		}
		else {
			return again().getOrMakeBe();
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(InitialOwner, initialOwnerOldValue);
	}
/*
udanax-top.st:21985:FeGrandPlaceHolder methodsFor: 'server accessing'!
{BeRangeElement} getOrMakeBe
	"Create a new persistent PlaceHolder and register it in the GrandMap."
	| result {BeRangeElement} |
	InitialOwner fluidBind: self owner during:
		[result _ CurrentGrandMap fluidGet newPlaceHolder.
		(CurrentGrandMap fluidGet at: myID tryIntroduce: result)
			ifTrue: [^result]
			ifFalse: [^self again getOrMakeBe]]!
*/
}
public FeGrandPlaceHolder(ID iD) {
	super();
	myID = iD;
/*
udanax-top.st:21997:FeGrandPlaceHolder methodsFor: 'private: create'!
create: iD {ID}
	super create.
	myID := iD!
*/
}
public FeGrandPlaceHolder() {
/*

Generated during transformation
*/
}
public FeGrandPlaceHolder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
