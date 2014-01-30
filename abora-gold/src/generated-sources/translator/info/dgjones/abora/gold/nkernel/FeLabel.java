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
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeLabel;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.xcvr.Rcvr;
import java.io.PrintWriter;

/**
 * An identity attached to a RangeElement within an Edition.
 */
public class FeLabel extends FeRangeElement {

	protected BeLabel myBeLabel;
/*
udanax-top.st:21705:
FeRangeElement subclass: #FeLabel
	instanceVariableNames: 'myBeLabel {BeLabel | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:21709:
FeLabel comment:
'An identity attached to a RangeElement within an Edition.'!
*/
/*
udanax-top.st:21711:
(FeLabel getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:21764:
FeLabel class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:21767:
(FeLabel getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeLabel.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public BeRangeElement fetchBe() {
	return myBeLabel;
/*
udanax-top.st:21716:FeLabel methodsFor: 'server accessing'!
{BeRangeElement | NULL} fetchBe
	^myBeLabel!
*/
}
public BeRangeElement getOrMakeBe() {
	if (myBeLabel == null) {
		myBeLabel = ((BeGrandMap) CurrentGrandMap.fluidGet()).newLabel();
		myBeLabel.addFeRangeElement(this);
	}
	return myBeLabel;
/*
udanax-top.st:21720:FeLabel methodsFor: 'server accessing'!
{BeRangeElement} getOrMakeBe
	myBeLabel == NULL ifTrue:
		[myBeLabel _ CurrentGrandMap fluidGet newLabel.
		myBeLabel addFeRangeElement: self].
	^myBeLabel!
*/
}
public FeRangeElement again() {
	throw new UnimplementedException();
/*
udanax-top.st:21729:FeLabel methodsFor: 'client accessing'!
{FeRangeElement} again
	self unimplemented.
	^NULL "fodder"!
*/
}
public boolean canMakeIdentical(FeRangeElement newIdentity) {
	if ( ! (isIdentical(newIdentity))) {
		throw new UnimplementedException();
	}
	return true;
/*
udanax-top.st:21734:FeLabel methodsFor: 'client accessing'!
{BooleanVar} canMakeIdentical: newIdentity {FeRangeElement}
	(self isIdentical: newIdentity) ifFalse:
		[self unimplemented].
	^true!
*/
}
public void makeIdentical(FeRangeElement newIdentity) {
	throw new UnimplementedException();
/*
udanax-top.st:21740:FeLabel methodsFor: 'client accessing'!
{void} makeIdentical: newIdentity {FeRangeElement}
	self unimplemented!
*/
}
public void destruct() {
	if ( ! (myBeLabel == null)) {
		myBeLabel.removeFeRangeElement(this);
	}
	super.destruct();
/*
udanax-top.st:21746:FeLabel methodsFor: 'destruct'!
{void} destruct
	myBeLabel == NULL ifFalse:
		[myBeLabel removeFeRangeElement: self].
	super destruct.!
*/
}
public FeLabel(BeLabel label) {
	super();
	myBeLabel = label;
/*
udanax-top.st:21754:FeLabel methodsFor: 'creation'!
create: label {BeLabel | NULL}
	super create.
	myBeLabel _ label.!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(getOrMakeBe().hashForEqual());
	oo.print(")");
/*
udanax-top.st:21760:FeLabel methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << self getOrMakeBe hashForEqual << ')'!
*/
}
/**
 * The label will be made on demand.
 */
public static FeLabel fake() {
	return on(null);
/*
udanax-top.st:21772:FeLabel class methodsFor: 'creation'!
{FeLabel} fake
	"The label will be made on demand."
	^self on: NULL!
*/
}
/**
 * Essential. Create a new unique Label
 */
public static FeLabel make() {
	return FeLabel.fake();
/*
udanax-top.st:21777:FeLabel class methodsFor: 'creation'!
{FeLabel CLIENT} make
	"Essential. Create a new unique Label"
	
	^FeLabel fake!
*/
}
public static FeLabel on(BeLabel label) {
	FeLabel result;
	result = new FeLabel(label);
	if (label != null) {
		label.addFeRangeElement(result);
	}
	return result;
/*
udanax-top.st:21782:FeLabel class methodsFor: 'creation'!
{FeLabel} on: label {BeLabel | NULL}
	
	| result {FeLabel} |
	result := self create: label.
	label ~~ NULL ifTrue:
		[label addFeRangeElement: result].
	^result!
*/
}
public FeLabel() {
/*

Generated during transformation
*/
}
public FeLabel(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
