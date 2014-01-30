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
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeDataHolder;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeVirtualDataHolder;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.x.PrimValue;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Fakes a DataHolder by having an Edition and a key.
 */
public class FeVirtualDataHolder extends FeDataHolder {

	protected PrimValue myValue;
	protected Position myKey;
	protected BeEdition myEdition;
/*
udanax-top.st:20654:
FeDataHolder subclass: #FeVirtualDataHolder
	instanceVariableNames: '
		myValue {PrimValue}
		myKey {Position}
		myEdition {BeEdition}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:20661:
FeVirtualDataHolder comment:
'Fakes a DataHolder by having an Edition and a key.'!
*/
/*
udanax-top.st:20663:
(FeVirtualDataHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeVirtualDataHolder.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Fetch from my Edition again, just in case I've been consolidated.
 */
public FeRangeElement again() {
	return myEdition.fetch(myKey);
/*
udanax-top.st:20668:FeVirtualDataHolder methodsFor: 'accessing'!
{FeRangeElement} again
	"Fetch from my Edition again, just in case I've been consolidated."
	
	^myEdition fetch: myKey!
*/
}
/**
 * This can do a version comparison (which seems a bit extreme).
 */
public boolean isIdentical(FeRangeElement other) {
	Dean.shouldImplement();
	return false;
/*
udanax-top.st:20673:FeVirtualDataHolder methodsFor: 'accessing'!
{BooleanVar} isIdentical: other {FeRangeElement}
	"This can do a version comparison (which seems a bit extreme)."
	Dean shouldImplement.
	^false "fodder"!
*/
}
public ID owner() {
	return myEdition.ownerAt(myKey);
/*
udanax-top.st:20679:FeVirtualDataHolder methodsFor: 'accessing'!
{ID} owner
	^myEdition ownerAt: myKey!
*/
}
public PrimValue value() {
	return myValue;
/*
udanax-top.st:20683:FeVirtualDataHolder methodsFor: 'accessing'!
{PrimValue} value
	^myValue!
*/
}
public BeRangeElement fetchBe() {
	return null;
/*
udanax-top.st:20688:FeVirtualDataHolder methodsFor: 'server accessing'!
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
udanax-top.st:20692:FeVirtualDataHolder methodsFor: 'server accessing'!
{BeRangeElement} getOrMakeBe
	"Force the ent to generate a beRangeElement at myKey."
	
	^myEdition getOrMakeBe: myKey!
*/
}
public FeVirtualDataHolder(PrimValue value, Position key, BeEdition edition) {
	super();
	myValue = value;
	myKey = key;
	myEdition = edition;
/*
udanax-top.st:20699:FeVirtualDataHolder methodsFor: 'private: create'!
create: value {PrimValue}
	with: key {Position}
	with: edition {BeEdition}
	
	super create.
	myValue := value.
	myKey := key.
	myEdition := edition.!
*/
}
public FeVirtualDataHolder() {
/*

Generated during transformation
*/
}
public FeVirtualDataHolder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
