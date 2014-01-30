/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.steppers;

import info.dgjones.abora.gold.collection.steppers.OffsetArrayStepper;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class OffsetArrayStepper extends TableStepper {

	protected TableStepper myArrayStepper;
	protected Dsp myDsp;
/*
udanax-top.st:56112:
TableStepper subclass: #OffsetArrayStepper
	instanceVariableNames: '
		myArrayStepper {TableStepper}
		myDsp {Dsp}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Steppers'!
*/
/*
udanax-top.st:56118:
(OffsetArrayStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:56157:
OffsetArrayStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:56160:
(OffsetArrayStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OffsetArrayStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	return myArrayStepper.fetch();
/*
udanax-top.st:56123:OffsetArrayStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	^ myArrayStepper fetch!
*/
}
public Heaper get() {
	return myArrayStepper.get();
/*
udanax-top.st:56126:OffsetArrayStepper methodsFor: 'operations'!
{Heaper wimpy} get
	^ myArrayStepper get!
*/
}
public boolean hasValue() {
	return myArrayStepper.hasValue();
/*
udanax-top.st:56129:OffsetArrayStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^myArrayStepper hasValue!
*/
}
public void step() {
	myArrayStepper.step();
/*
udanax-top.st:56132:OffsetArrayStepper methodsFor: 'operations'!
{void} step
	myArrayStepper step!
*/
}
public int index() {
	return myDsp.ofInt(myArrayStepper.index());
/*
udanax-top.st:56137:OffsetArrayStepper methodsFor: 'special'!
{IntegerVar} index
	^myDsp ofInt: myArrayStepper index!
*/
}
public Position position() {
	return myDsp.of(myArrayStepper.position());
/*
udanax-top.st:56140:OffsetArrayStepper methodsFor: 'special'!
{Position} position
	^myDsp of: myArrayStepper position!
*/
}
public OffsetArrayStepper(TableStepper onStepper, Dsp aDsp) {
	super();
	myArrayStepper = onStepper;
	myDsp = aDsp;
/*
udanax-top.st:56145:OffsetArrayStepper methodsFor: 'protected: create'!
create.Stepper: onStepper {TableStepper} with: aDsp {Dsp} 
	
	super create.
	myArrayStepper _ onStepper.
	myDsp _ aDsp!
*/
}
public Stepper copy() {
	return OffsetArrayStepper.make(((TableStepper) myArrayStepper.copy()), myDsp);
/*
udanax-top.st:56153:OffsetArrayStepper methodsFor: 'create'!
{Stepper} copy
	^ OffsetArrayStepper make: (myArrayStepper copy cast: TableStepper) with: myDsp!
*/
}
/*
udanax-top.st:56165:OffsetArrayStepper class methodsFor: 'smalltalk: creation'!
create.Stepper: aStepper
	^ self new create.Stepper: aStepper!
*/
/*
udanax-top.st:56168:OffsetArrayStepper class methodsFor: 'smalltalk: creation'!
create.Stepper: aStepper with: aDsp
	^ self new create.Stepper: aStepper with: aDsp!
*/
public static TableStepper make(TableStepper arrayStepper, Dsp aDsp) {
	return new OffsetArrayStepper(arrayStepper, aDsp);
/*
udanax-top.st:56173:OffsetArrayStepper class methodsFor: 'create'!
{TableStepper} make: arrayStepper {TableStepper} with: aDsp {Dsp} 
	^self create.Stepper: arrayStepper with: aDsp!
*/
}
public OffsetArrayStepper() {
/*

Generated during transformation
*/
}
public OffsetArrayStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
