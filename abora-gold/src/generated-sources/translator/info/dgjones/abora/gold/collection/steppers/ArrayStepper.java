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
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.ActualArray;
import info.dgjones.abora.gold.collection.tables.MuArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class ArrayStepper extends TableStepper {

	protected ActualArray arrayInternal;
	protected int indexInternal;
/*
udanax-top.st:55393:
TableStepper subclass: #ArrayStepper
	instanceVariableNames: '
		arrayInternal {ActualArray}
		indexInternal {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Steppers'!
*/
/*
udanax-top.st:55399:
(ArrayStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #NOT.A.TYPE; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ArrayStepper.class).setAttributes( new Set().add("NOTATYPE").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:55404:ArrayStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	self subclassResponsibility!
*/
}
public Heaper get() {
	return arrayInternal.intGet(indexInternal);
/*
udanax-top.st:55407:ArrayStepper methodsFor: 'operations'!
{Heaper wimpy} get
	^arrayInternal intGet: indexInternal!
*/
}
public boolean hasValue() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:55410:ArrayStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	self subclassResponsibility!
*/
}
public void step() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:55413:ArrayStepper methodsFor: 'operations'!
{void} step
	self subclassResponsibility!
*/
}
public int index() {
	return indexInternal;
/*
udanax-top.st:55418:ArrayStepper methodsFor: 'special'!
{IntegerVar} index
	^indexInternal!
*/
}
public Position position() {
	return IntegerPos.make(indexInternal);
/*
udanax-top.st:55421:ArrayStepper methodsFor: 'special'!
{Position} position
	^indexInternal integer!
*/
}
public ActualArray array() {
	return arrayInternal;
/*
udanax-top.st:55426:ArrayStepper methodsFor: 'protected: accessing'!
{ActualArray} array
	^arrayInternal!
*/
}
public void setIndex(int i) {
	indexInternal = i;
/*
udanax-top.st:55429:ArrayStepper methodsFor: 'protected: accessing'!
{void} setIndex: i {Int32}
	indexInternal _ i!
*/
}
public Stepper copy() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:55434:ArrayStepper methodsFor: 'create'!
{Stepper} copy
	self subclassResponsibility!
*/
}
public ArrayStepper(MuArray array) {
	super();
	arrayInternal = (ActualArray) array.copy();
	indexInternal = 0;
/*
udanax-top.st:55439:ArrayStepper methodsFor: 'protected: create'!
create: array {MuArray}
	super create.
	arrayInternal _ array copy cast: ActualArray.
	indexInternal _ Int32Zero!
*/
}
public ArrayStepper(MuArray array, int index) {
	super();
	arrayInternal = (ActualArray) array.copy();
	indexInternal = index;
/*
udanax-top.st:55444:ArrayStepper methodsFor: 'protected: create'!
create: array {MuArray} with: index {IntegerVar}
	super create.
	arrayInternal _ array copy cast: ActualArray.
	indexInternal _ index DOTasLong!
*/
}
public ArrayStepper() {
/*

Generated during transformation
*/
}
public ArrayStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
