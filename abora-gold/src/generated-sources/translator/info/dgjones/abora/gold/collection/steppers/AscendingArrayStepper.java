/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.steppers;

import info.dgjones.abora.gold.collection.steppers.ArrayStepper;
import info.dgjones.abora.gold.collection.steppers.AscendingArrayStepper;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.ActualArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class AscendingArrayStepper extends ArrayStepper {

	protected int lastValueInternal;
/*
udanax-top.st:55449:
ArrayStepper subclass: #AscendingArrayStepper
	instanceVariableNames: 'lastValueInternal {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Steppers'!
*/
/*
udanax-top.st:55453:
(AscendingArrayStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:55498:
AscendingArrayStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:55501:
(AscendingArrayStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(AscendingArrayStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Stepper copy() {
	return AscendingArrayStepper.make(array(), index(), lastValueInternal);
/*
udanax-top.st:55458:AscendingArrayStepper methodsFor: 'create'!
{Stepper} copy
	^AscendingArrayStepper
		make: self array
		with: self index
		with: lastValueInternal!
*/
}
public AscendingArrayStepper(ActualArray array) {
	super(array);
	lastValueInternal = array.endOffset();
/*
udanax-top.st:55466:AscendingArrayStepper methodsFor: 'protected: create'!
create: array {ActualArray}
	super create: array.
	lastValueInternal _ array endOffset!
*/
}
public AscendingArrayStepper(ActualArray array, int index) {
	super(array, index);
	lastValueInternal = array.endOffset();
/*
udanax-top.st:55470:AscendingArrayStepper methodsFor: 'protected: create'!
create: array {ActualArray} with: index {IntegerVar}
	super create: array with: index.
	lastValueInternal _ array endOffset!
*/
}
public AscendingArrayStepper(ActualArray array, int start, int stop) {
	super(array, start);
	lastValueInternal = stop;
/*
udanax-top.st:55474:AscendingArrayStepper methodsFor: 'protected: create'!
create: array {ActualArray} with: start {IntegerVar} with: stop {IntegerVar}
	super create: array with: start.
	lastValueInternal _ stop DOTasLong!
*/
}
public Heaper fetch() {
	if (hasValue()) {
		return array().elementsArray().fetch(index());
	}
	else {
		return null;
	}
/*
udanax-top.st:55480:AscendingArrayStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	self hasValue
		ifTrue: [^self array elementsArray fetch: self index DOTasLong]
		ifFalse: [^NULL]!
*/
}
public boolean hasValue() {
	return index() <= lastValueInternal;
/*
udanax-top.st:55485:AscendingArrayStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^self index <= lastValueInternal!
*/
}
public void step() {
	setIndex(index() + 1);
/*
udanax-top.st:55488:AscendingArrayStepper methodsFor: 'operations'!
{void} step
	self setIndex: self index DOTasLong + 1!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print(" on ");
	oo.print((array().subTableBetween(index(), lastValueInternal)));
/*
udanax-top.st:55493:AscendingArrayStepper methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << ' on ' << (self array subTableBetween: self index with: lastValueInternal)!
*/
}
public static TableStepper make(ActualArray array) {
	return new AscendingArrayStepper(array);
/*
udanax-top.st:55506:AscendingArrayStepper class methodsFor: 'create'!
{TableStepper} make: array {ActualArray}
	^ self create: array!
*/
}
public static TableStepper make(ActualArray array, int index) {
	return new AscendingArrayStepper(array, index);
/*
udanax-top.st:55509:AscendingArrayStepper class methodsFor: 'create'!
{TableStepper} make: array {ActualArray} with: index {IntegerVar}
	^ self create: array with: index!
*/
}
public static TableStepper make(ActualArray array, int start, int stop) {
	return new AscendingArrayStepper(array, start, stop);
/*
udanax-top.st:55512:AscendingArrayStepper class methodsFor: 'create'!
{TableStepper} make: array {ActualArray} with: start {IntegerVar} with: stop {IntegerVar}
	^ self create: array with: start with: stop!
*/
}
public AscendingArrayStepper() {
/*

Generated during transformation
*/
}
public AscendingArrayStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
