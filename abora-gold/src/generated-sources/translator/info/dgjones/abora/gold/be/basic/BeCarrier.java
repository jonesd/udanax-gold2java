/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.basic;

import info.dgjones.abora.gold.be.basic.BeCarrier;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * These are used to carry a combination of a rangeElement and a label.  Using
 * FeRangeElements would be a hack that drags in permissions checking, etc.
 */
public class BeCarrier extends Heaper {

	protected BeLabel myLabel;
	protected BeRangeElement myRangeElement;
/*
udanax-top.st:12811:
Heaper subclass: #BeCarrier
	instanceVariableNames: '
		myLabel {BeLabel | NULL}
		myRangeElement {BeRangeElement}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Basic'!
*/
/*
udanax-top.st:12817:
BeCarrier comment:
'These are used to carry a combination of a rangeElement and a label.  Using FeRangeElements would be a hack that drags in permissions checking, etc.'!
*/
/*
udanax-top.st:12819:
(BeCarrier getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:12854:
BeCarrier class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:12857:
(BeCarrier getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BeCarrier.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public BeLabel fetchLabel() {
	return myLabel;
/*
udanax-top.st:12824:BeCarrier methodsFor: 'accessing'!
{BeLabel | NULL} fetchLabel
	^myLabel!
*/
}
public BeLabel getLabel() {
	if (myLabel == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NO_LABEL);
	}
	return myLabel;
/*
udanax-top.st:12827:BeCarrier methodsFor: 'accessing'!
{BeLabel} getLabel
	myLabel == NULL ifTrue: [Heaper BLAST: #NoLabel].
	^myLabel!
*/
}
public FeRangeElement makeFe() {
	if (myLabel == null) {
		return myRangeElement.makeFe(myLabel);
	}
	else {
		return myRangeElement.makeFe(myLabel);
	}
/*
udanax-top.st:12831:BeCarrier methodsFor: 'accessing'!
{FeRangeElement} makeFe
	myLabel == NULL 
		ifTrue: [^myRangeElement makeFe: myLabel]
		ifFalse: [^myRangeElement makeFe: myLabel]!
*/
}
public BeRangeElement rangeElement() {
	return myRangeElement;
/*
udanax-top.st:12836:BeCarrier methodsFor: 'accessing'!
{BeRangeElement} rangeElement
	^myRangeElement!
*/
}
public BeCarrier(BeLabel label, BeRangeElement element) {
	super();
	myLabel = label;
	myRangeElement = element;
	if ( ! (myLabel != null) == (myRangeElement instanceof BeEdition)) {
		throw new AboraRuntimeException(AboraRuntimeException.INCORRECT_LABEL);
	}
/*
udanax-top.st:12841:BeCarrier methodsFor: 'creation'!
create: label {BeLabel | NULL} with: element {BeRangeElement}
	super create.
	myLabel _ label.
	myRangeElement _ element.
	(myLabel ~~ NULL) == (myRangeElement isKindOf: BeEdition)
		ifFalse: [Heaper BLAST: #IncorrectLabel]!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:12850:BeCarrier methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
/**
 * For non-Editions only.
 */
public static BeCarrier label(BeRangeElement element) {
	return new BeCarrier((((BeGrandMap) CurrentGrandMap.fluidGet()).newLabel()), element);
/*
udanax-top.st:12862:BeCarrier class methodsFor: 'creation'!
{BeCarrier} label: element {BeRangeElement}
	"For non-Editions only."
	[BeGrandMap] USES.
	^self create: (CurrentGrandMap fluidGet newLabel) with: element!
*/
}
/**
 * For non-Editions only.
 */
public static BeCarrier make(BeRangeElement element) {
	return new BeCarrier(null, element);
/*
udanax-top.st:12867:BeCarrier class methodsFor: 'creation'!
make: element {BeRangeElement}
	"For non-Editions only."
	
	^self create: NULL with: element!
*/
}
/**
 * For editions only.
 */
public static BeCarrier make(BeLabel label, BeRangeElement element) {
	return new BeCarrier(label, element);
/*
udanax-top.st:12872:BeCarrier class methodsFor: 'creation'!
make: label {BeLabel | NULL} with: element {BeRangeElement}
	"For editions only."
	
	^self create: label with: element!
*/
}
public BeCarrier() {
/*

Generated during transformation
*/
}
public BeCarrier(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
