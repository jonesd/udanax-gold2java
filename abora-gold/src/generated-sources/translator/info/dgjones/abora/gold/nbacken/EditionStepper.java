/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nbacken;

import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nbacken.EditionStepper;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class EditionStepper extends TableStepper {

	protected Stepper myKeys;
	protected FeEdition myEdition;
/*
udanax-top.st:55617:
TableStepper subclass: #EditionStepper
	instanceVariableNames: '
		myKeys {Stepper of: Position}
		myEdition {FeEdition}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nbacken'!
*/
/*
udanax-top.st:55623:
(EditionStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(EditionStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Stepper copy() {
	return new EditionStepper(myKeys.copy(), myEdition);
/*
udanax-top.st:55628:EditionStepper methodsFor: 'create'!
{Stepper} copy
	^EditionStepper create: myKeys copy with: myEdition!
*/
}
public EditionStepper(Stepper keys, FeEdition edition) {
	super();
	myKeys = keys;
	myEdition = edition;
/*
udanax-top.st:55632:EditionStepper methodsFor: 'create'!
create: keys {Stepper of: Position} with: edition {FeEdition}
	
	super create.
	myKeys := keys.
	myEdition := edition.!
*/
}
public Position position() {
	return (Position) myKeys.get();
/*
udanax-top.st:55640:EditionStepper methodsFor: 'special'!
{Position} position
	^myKeys get cast: Position!
*/
}
public Heaper fetch() {
	if (myKeys.hasValue()) {
		return myEdition.get(((Position) myKeys.fetch()));
	}
	else {
		return null;
	}
/*
udanax-top.st:55646:EditionStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	myKeys hasValue
		ifTrue: [^myEdition get: (myKeys fetch cast: Position)]
		ifFalse: [^NULL]!
*/
}
public boolean hasValue() {
	return myKeys.hasValue();
/*
udanax-top.st:55652:EditionStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^myKeys hasValue!
*/
}
public void step() {
	myKeys.step();
/*
udanax-top.st:55656:EditionStepper methodsFor: 'operations'!
{void} step
	myKeys step!
*/
}
public EditionStepper() {
/*

Generated during transformation
*/
}
public EditionStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
