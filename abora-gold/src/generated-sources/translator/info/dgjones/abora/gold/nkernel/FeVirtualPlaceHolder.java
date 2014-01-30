/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.detect.FeFillDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FePlaceHolder;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeVirtualPlaceHolder;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Fakes a PlaceHolder by having an Edition and a key.
 */
public class FeVirtualPlaceHolder extends FePlaceHolder {

	protected BeEdition myEdition;
	protected Position myKey;
/*
udanax-top.st:22002:
FePlaceHolder subclass: #FeVirtualPlaceHolder
	instanceVariableNames: '
		myEdition {BeEdition}
		myKey {Position}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:22008:
FeVirtualPlaceHolder comment:
'Fakes a PlaceHolder by having an Edition and a key.'!
*/
/*
udanax-top.st:22010:
(FeVirtualPlaceHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeVirtualPlaceHolder.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public FeRangeElement again() {
	return myEdition.get(myKey);
/*
udanax-top.st:22015:FeVirtualPlaceHolder methodsFor: 'client accessing'!
{FeRangeElement} again
	^myEdition get: myKey!
*/
}
public boolean canMakeIdentical(FeRangeElement newIdentity) {
	if ( ! (isIdentical(newIdentity))) {
		throw new UnimplementedException();
	}
	return true;
/*
udanax-top.st:22019:FeVirtualPlaceHolder methodsFor: 'client accessing'!
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
		and coerce both of us and have the BeRangeElements try. */
	Someone.thingToDo();
	/* This doesn't need to force newIdentity into a BeRangeElement. */
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority(owner()))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_OWNER);
	}
	getOrMakeBe().makeIdentical(newIdentity.getOrMakeBe());
/*
udanax-top.st:22025:FeVirtualPlaceHolder methodsFor: 'client accessing'!
{void} makeIdentical: newIdentity {FeRangeElement}
	"Consolidate this PlaceHolder to the newIdentity.  Return true if successful."
	"Check permissions
		and coerce both of us and have the BeRangeElements try."
	self thingToDo.  "This doesn't need to force newIdentity into a BeRangeElement."
	
	(CurrentKeyMaster fluidGet hasAuthority: self owner)
		ifFalse: [Heaper BLAST: #MustBeOwner].
	self getOrMakeBe makeIdentical: newIdentity getOrMakeBe!
*/
}
public ID owner() {
	return myEdition.ownerAt(myKey);
/*
udanax-top.st:22036:FeVirtualPlaceHolder methodsFor: 'client accessing'!
{ID} owner
	^myEdition ownerAt: myKey!
*/
}
public void removeFillDetector(FeFillDetector detector) {
	throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_SET);
/*
udanax-top.st:22040:FeVirtualPlaceHolder methodsFor: 'client accessing'!
{void} removeFillDetector: detector {FeFillDetector}
	Heaper BLAST: #NotInSet!
*/
}
public BeRangeElement fetchBe() {
	return null;
/*
udanax-top.st:22046:FeVirtualPlaceHolder methodsFor: 'server accessing'!
{BeRangeElement | NULL} fetchBe
	^NULL!
*/
}
/**
 * Force the ent to generate a beRangeElement at myKey.
 */
public BeRangeElement getOrMakeBe() {
	return myEdition.getOrMakeBe(myKey);
/*
udanax-top.st:22050:FeVirtualPlaceHolder methodsFor: 'server accessing'!
{BeRangeElement} getOrMakeBe
	"Force the ent to generate a beRangeElement at myKey."
	
	^myEdition getOrMakeBe: myKey!
*/
}
public FeVirtualPlaceHolder(BeEdition edition, Position key) {
	super();
	myEdition = edition;
	myKey = key;
/*
udanax-top.st:22057:FeVirtualPlaceHolder methodsFor: 'private: create'!
create: edition {BeEdition} with: key {Position}
	super create.
	myEdition := edition.
	myKey := key!
*/
}
public FeVirtualPlaceHolder() {
/*

Generated during transformation
*/
}
public FeVirtualPlaceHolder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
