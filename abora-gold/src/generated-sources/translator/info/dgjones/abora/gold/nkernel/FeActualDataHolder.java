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
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeActualDataHolder;
import info.dgjones.abora.gold.nkernel.FeDataHolder;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.x.PrimValue;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Actually has a persistent individual DataHolder on the Server
 */
public class FeActualDataHolder extends FeDataHolder {

	protected BeDataHolder myBeDataHolder;
/*
udanax-top.st:20609:
FeDataHolder subclass: #FeActualDataHolder
	instanceVariableNames: 'myBeDataHolder {BeDataHolder}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:20613:
FeActualDataHolder comment:
'Actually has a persistent individual DataHolder on the Server'!
*/
/*
udanax-top.st:20615:
(FeActualDataHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeActualDataHolder.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * I'm completely reified.  Just return me.
 */
public FeRangeElement again() {
	return this;
/*
udanax-top.st:20620:FeActualDataHolder methodsFor: 'client accessing'!
{FeRangeElement} again
	"I'm completely reified.  Just return me."
	
	^self!
*/
}
/**
 * The actual data value
 */
public PrimValue value() {
	return myBeDataHolder.value();
/*
udanax-top.st:20625:FeActualDataHolder methodsFor: 'client accessing'!
{PrimValue} value
	"The actual data value"
	
	^myBeDataHolder value!
*/
}
public BeRangeElement fetchBe() {
	return myBeDataHolder;
/*
udanax-top.st:20632:FeActualDataHolder methodsFor: 'server accessing'!
{BeRangeElement | NULL} fetchBe
	^myBeDataHolder!
*/
}
public BeRangeElement getOrMakeBe() {
	return myBeDataHolder;
/*
udanax-top.st:20636:FeActualDataHolder methodsFor: 'server accessing'!
{BeRangeElement} getOrMakeBe
	^myBeDataHolder!
*/
}
public FeActualDataHolder(BeDataHolder be) {
	super();
	myBeDataHolder = be;
/*
udanax-top.st:20642:FeActualDataHolder methodsFor: 'private: create'!
create: be {BeDataHolder}
	super create.
	myBeDataHolder := be.!
*/
}
public void destruct() {
	myBeDataHolder.removeFeRangeElement(this);
	super.destruct();
/*
udanax-top.st:20649:FeActualDataHolder methodsFor: 'destruct'!
{void} destruct
	myBeDataHolder removeFeRangeElement: self.
	super destruct.!
*/
}
public FeActualDataHolder() {
/*

Generated during transformation
*/
}
public FeActualDataHolder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
