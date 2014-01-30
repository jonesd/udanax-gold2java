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
import info.dgjones.abora.gold.be.basic.BeIDHolder;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeIDHolder;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.xcvr.Rcvr;
import java.io.PrintWriter;

/**
 * An object for having an ID in the range of an Edition. Tentative feature.
 */
public class FeIDHolder extends FeRangeElement {

	protected BeIDHolder myBeIDHolder;
/*
udanax-top.st:21617:
FeRangeElement subclass: #FeIDHolder
	instanceVariableNames: 'myBeIDHolder {BeIDHolder}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:21621:
FeIDHolder comment:
'An object for having an ID in the range of an Edition. Tentative feature.'!
*/
/*
udanax-top.st:21623:
(FeIDHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:21679:
FeIDHolder class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:21682:
(FeIDHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeIDHolder.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public FeRangeElement again() {
	return this;
/*
udanax-top.st:21628:FeIDHolder methodsFor: 'accessing'!
{FeRangeElement} again
	^self!
*/
}
public boolean canMakeIdentical(FeRangeElement newIdentity) {
	if ( ! (isIdentical(newIdentity))) {
		throw new UnimplementedException();
	}
	return true;
/*
udanax-top.st:21632:FeIDHolder methodsFor: 'accessing'!
{BooleanVar} canMakeIdentical: newIdentity {FeRangeElement}
	(self isIdentical: newIdentity) ifFalse:
		[self unimplemented].
	^true!
*/
}
/**
 * Essential.  The ID in this holder.
 */
public ID iD() {
	return myBeIDHolder.iD();
/*
udanax-top.st:21638:FeIDHolder methodsFor: 'accessing'!
{ID CLIENT} iD
	"Essential.  The ID in this holder."
	
	^myBeIDHolder iD!
*/
}
public void makeIdentical(FeRangeElement newIdentity) {
	if ( ! (isIdentical(newIdentity))) {
		throw new UnimplementedException();
	}
/*
udanax-top.st:21643:FeIDHolder methodsFor: 'accessing'!
{void} makeIdentical: newIdentity {FeRangeElement}
	(self isIdentical: newIdentity) ifFalse:
		[self unimplemented]!
*/
}
public BeRangeElement fetchBe() {
	return myBeIDHolder;
/*
udanax-top.st:21650:FeIDHolder methodsFor: 'server accessing'!
{BeRangeElement | NULL} fetchBe
	^myBeIDHolder!
*/
}
public BeRangeElement getOrMakeBe() {
	return myBeIDHolder;
/*
udanax-top.st:21654:FeIDHolder methodsFor: 'server accessing'!
{BeRangeElement} getOrMakeBe
	^myBeIDHolder!
*/
}
public FeIDHolder(BeIDHolder be) {
	super();
	myBeIDHolder = be;
/*
udanax-top.st:21660:FeIDHolder methodsFor: 'private: create'!
create: be {BeIDHolder}
	super create.
	myBeIDHolder := be.!
*/
}
public void printOn(PrintWriter oo) {
	oo.print("IDHolder(");
	oo.print(iD());
	oo.print(")");
/*
udanax-top.st:21667:FeIDHolder methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << 'IDHolder(' << self iD << ')'!
*/
}
public void destruct() {
	myBeIDHolder.removeFeRangeElement(this);
	super.destruct();
/*
udanax-top.st:21673:FeIDHolder methodsFor: 'destruct'!
{void} destruct
	myBeIDHolder removeFeRangeElement: self.
	super destruct.!
*/
}
/**
 * Essential. Make a single IDHolder with the given ID. Tentative feature.
 */
public static FeIDHolder make(ID iD) {
	return FeIDHolder.on((((BeGrandMap) CurrentGrandMap.fluidGet()).newIDHolder(iD)));
/*
udanax-top.st:21687:FeIDHolder class methodsFor: 'creation'!
{FeIDHolder CLIENT} make: iD {ID}
	"Essential. Make a single IDHolder with the given ID. Tentative feature."
	
	^FeIDHolder on: (CurrentGrandMap fluidGet newIDHolder: iD)!
*/
}
public static FeIDHolder on(BeIDHolder be) {
	FeIDHolder result;
	result = new FeIDHolder(be);
	be.addFeRangeElement(result);
	return result;
/*
udanax-top.st:21692:FeIDHolder class methodsFor: 'creation'!
{FeIDHolder} on: be {BeIDHolder}
	| result {FeIDHolder} |
	result := self create: be.
	be addFeRangeElement: result.
	^result!
*/
}
/**
 * {ID CLIENT} iD
 */
public static void infostProtocol() {
/*
udanax-top.st:21701:FeIDHolder class methodsFor: 'smalltalk: system'!
info.stProtocol
"{ID CLIENT} iD
"!
*/
}
public FeIDHolder() {
/*

Generated during transformation
*/
}
public FeIDHolder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
