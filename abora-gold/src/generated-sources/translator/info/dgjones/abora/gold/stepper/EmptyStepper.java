/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.stepper;

import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.stepper.EmptyStepper;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * This is a Stepper when you just want to step across a single item.
 */
public class EmptyStepper extends Stepper {

/*
udanax-top.st:53740:
Stepper subclass: #EmptyStepper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-stepper'!
*/
/*
udanax-top.st:53744:
EmptyStepper comment:
'This is a Stepper when you just want to step across a single item.'!
*/
/*
udanax-top.st:53746:
(EmptyStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(EmptyStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * This object is a canonical single instance, so its destructor should only be called after
 * main has exited.
 */
public void destruct() {
	AboraSupport.translateOnly();
	{
		/* if (!Initializer::inStaticDestruction()) BLAST(SanityViolation); */
	}
	super.destruct();
/*
udanax-top.st:53751:EmptyStepper methodsFor: 'protected: destruct'!
{void} destruct
	"This object is a canonical single instance, so its destructor should only be called after main has exited."
	'if (!!Initializer::inStaticDestruction()) BLAST(SanityViolation);' translateOnly.
	super destruct!
*/
}
public Stepper copy() {
	return this;
/*
udanax-top.st:53759:EmptyStepper methodsFor: 'create'!
{Stepper} copy
	^ self!
*/
}
/**
 * No
 */
public void destroy() {
/*
udanax-top.st:53762:EmptyStepper methodsFor: 'create'!
{void} destroy
	"No"!
*/
}
public Heaper fetch() {
	return null;
/*
udanax-top.st:53767:EmptyStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	^ NULL!
*/
}
public boolean hasValue() {
	return false;
/*
udanax-top.st:53770:EmptyStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^ false!
*/
}
public void step() {
/*
udanax-top.st:53773:EmptyStepper methodsFor: 'operations'!
{void} step!
*/
}
public EmptyStepper() {
/*

Generated during transformation
*/
}
public EmptyStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
