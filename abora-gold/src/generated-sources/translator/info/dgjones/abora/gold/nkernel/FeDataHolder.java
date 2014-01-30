/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.be.basic.BeDataHolder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeActualDataHolder;
import info.dgjones.abora.gold.nkernel.FeDataHolder;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeVirtualDataHolder;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.x.PrimValue;
import info.dgjones.abora.gold.xcvr.Rcvr;
import java.io.PrintWriter;

/**
 * The kind of FeRangeElement that represents a piece of data in the Server, along with its
 * identity.
 */
public class FeDataHolder extends FeRangeElement {

/*
udanax-top.st:20516:
FeRangeElement subclass: #FeDataHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:20520:
FeDataHolder comment:
'The kind of FeRangeElement that represents a piece of data in the Server, along with its identity.'!
*/
/*
udanax-top.st:20522:
(FeDataHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:20577:
FeDataHolder class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:20580:
(FeDataHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeDataHolder.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public FeRangeElement again() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:20527:FeDataHolder methodsFor: 'client accessing'!
{FeRangeElement} again
	self subclassResponsibility!
*/
}
/**
 * Check that it is data with the same value,
 * and check permissions,
 * and forward the operation after coercing the newIdentity to a persistent RangeElement.
 */
public boolean canMakeIdentical(FeRangeElement newIdentity) {
	return ((newIdentity instanceof FeDataHolder) && ((((FeDataHolder) newIdentity).value().isEqual(value()))));
/*
udanax-top.st:20531:FeDataHolder methodsFor: 'client accessing'!
{BooleanVar} canMakeIdentical: newIdentity {FeRangeElement}
	
	"Check that it is data with the same value,
		and check permissions,
		and forward the operation after coercing the newIdentity to a persistent RangeElement."
	
	^((newIdentity isKindOf: FeDataHolder)
			and: [((newIdentity cast: FeDataHolder) value isEqual: self value)])!
*/
}
/**
 * Allow consolidation of data in 1st product.
 */
public void makeIdentical(FeRangeElement newIdentity) {
	FeKeyMaster ckm;
	/* Check that it is data with the same value,
		and check permissions,
		and forward the operation after coercing the newIdentity to a persistent RangeElement. */
	Someone.thingToDo();
	/* better blast */
	ckm = ((FeKeyMaster) CurrentKeyMaster.fluidGet());
	if ((newIdentity instanceof FeDataHolder) && ((((FeDataHolder) newIdentity).value().isEqual(value())) && (ckm.hasAuthority(owner())))) {
		throw new AboraRuntimeException(AboraRuntimeException.CANT_MAKE_IDENTICAL);
	}
	getOrMakeBe().makeIdentical(newIdentity.getOrMakeBe());
/*
udanax-top.st:20540:FeDataHolder methodsFor: 'client accessing'!
{void} makeIdentical: newIdentity {FeRangeElement}
	"Allow consolidation of data in 1st product."
	| ckm {FeKeyMaster} |
	"Check that it is data with the same value,
		and check permissions,
		and forward the operation after coercing the newIdentity to a persistent RangeElement."
	
	self thingToDo. "better blast"
	ckm := CurrentKeyMaster fluidGet.
	((newIdentity isKindOf: FeDataHolder)
			and: [((newIdentity cast: FeDataHolder) value isEqual: self value)
			and: [ckm hasAuthority: self owner]])
		ifTrue: [Heaper BLAST: #CantMakeIdentical].
	self getOrMakeBe makeIdentical: newIdentity getOrMakeBe!
*/
}
/**
 * Essential.  The actual data value
 */
public PrimValue value() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:20555:FeDataHolder methodsFor: 'client accessing'!
{PrimValue CLIENT} value
	"Essential.  The actual data value"
	
	self subclassResponsibility!
*/
}
public BeRangeElement fetchBe() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:20562:FeDataHolder methodsFor: 'server accessing'!
{BeRangeElement | NULL} fetchBe
	self subclassResponsibility!
*/
}
public BeRangeElement getOrMakeBe() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:20566:FeDataHolder methodsFor: 'server accessing'!
{BeRangeElement} getOrMakeBe
	self subclassResponsibility!
*/
}
public void printOn(PrintWriter oo) {
	oo.print("DataHolder(");
	oo.print(value());
	oo.print(")");
/*
udanax-top.st:20572:FeDataHolder methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << 'DataHolder(' << self value << ')'!
*/
}
public static FeDataHolder fake(PrimValue value, Position key, BeEdition edition) {
	return new FeVirtualDataHolder(value, key, edition);
/*
udanax-top.st:20585:FeDataHolder class methodsFor: 'creation'!
{FeDataHolder} fake: value {PrimValue}
	with: key {Position}
	with: edition {BeEdition}
	
	^FeVirtualDataHolder create: value with: key with: edition.!
*/
}
/**
 * Make a single DataHolder with the given value
 */
public static FeDataHolder make(PrimValue value) {
	return FeDataHolder.on((((BeGrandMap) CurrentGrandMap.fluidGet()).newDataHolder(value)));
/*
udanax-top.st:20591:FeDataHolder class methodsFor: 'creation'!
{FeDataHolder CLIENT} make: value {PrimValue}
	"Make a single DataHolder with the given value"
	
	^FeDataHolder on: (CurrentGrandMap fluidGet newDataHolder: value)!
*/
}
public static FeDataHolder on(BeDataHolder be) {
	FeDataHolder result;
	result = new FeActualDataHolder(be);
	be.addFeRangeElement(result);
	return result;
/*
udanax-top.st:20596:FeDataHolder class methodsFor: 'creation'!
{FeDataHolder} on: be {BeDataHolder}
	| result {FeDataHolder} |
	result := FeActualDataHolder create: be.
	be addFeRangeElement: result.
	^result!
*/
}
/**
 * {PrimValue CLIENT} value
 */
public static void infostProtocol() {
/*
udanax-top.st:20605:FeDataHolder class methodsFor: 'smalltalk: system'!
info.stProtocol
"{PrimValue CLIENT} value
"!
*/
}
public FeDataHolder() {
/*

Generated during transformation
*/
}
public FeDataHolder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
