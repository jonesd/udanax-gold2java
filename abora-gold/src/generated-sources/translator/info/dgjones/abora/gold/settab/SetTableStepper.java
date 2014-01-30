/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.settab;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.settab.SetTableStepper;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class SetTableStepper extends Stepper {

	protected PtrArray myPtrs;
	protected int myIndex;
	protected static PtrArray AnArray;
	protected static SetTableStepper AStepper;
/*
udanax-top.st:55165:
Stepper subclass: #SetTableStepper
	instanceVariableNames: '
		myPtrs {PtrArray}
		myIndex {Int32}'
	classVariableNames: '
		AnArray {PtrArray} 
		AStepper {SetTableStepper} '
	poolDictionaries: ''
	category: 'Xanadu-settab'!
*/
/*
udanax-top.st:55173:
(SetTableStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
/*
udanax-top.st:55223:
SetTableStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:55226:
(SetTableStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SetTableStepper.class).setAttributes( new Set().add("CONCRETE").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	if (myIndex < myPtrs.count()) {
		return myPtrs.fetch(myIndex);
	}
	else {
		return null;
	}
/*
udanax-top.st:55178:SetTableStepper methodsFor: 'accessing'!
{Heaper} fetch
	myIndex < myPtrs count
		ifTrue: [^ myPtrs fetch: myIndex]
		ifFalse: [^ NULL]!
*/
}
public boolean hasValue() {
	return myIndex < myPtrs.count();
/*
udanax-top.st:55183:SetTableStepper methodsFor: 'accessing'!
{BooleanVar} hasValue
	^ myIndex < myPtrs count!
*/
}
public void step() {
	myIndex = myIndex + 1;
/*
udanax-top.st:55186:SetTableStepper methodsFor: 'accessing'!
{void} step
	myIndex := myIndex + 1!
*/
}
public Stepper copy() {
	return new SetTableStepper(((PtrArray) myPtrs.copy()), myIndex);
/*
udanax-top.st:55191:SetTableStepper methodsFor: 'create'!
{Stepper} copy
	^ SetTableStepper create: (myPtrs copy cast: PtrArray) with: myIndex!
*/
}
public void destroy() {
	if (AStepper == null) {
		myPtrs.storeAll();
		AnArray = myPtrs;
		AStepper = this;
		destruct();
	}
	else {
		super.destroy();
	}
/*
udanax-top.st:55194:SetTableStepper methodsFor: 'create'!
{void} destroy
	AStepper == NULL
		ifTrue: [
			myPtrs storeAll.
			AnArray := myPtrs cast: PtrArray.
			AStepper := self.
			self destruct]
		ifFalse: [
			super destroy].!
*/
}
public SetTableStepper(PtrArray array) {
	super();
	myPtrs = array;
	myIndex = 0;
/*
udanax-top.st:55206:SetTableStepper methodsFor: 'protected: create'!
create: array {PtrArray}
	super create.
	myPtrs := array.
	myIndex := Int32Zero.!
*/
}
public SetTableStepper(PtrArray array, int index) {
	super();
	myPtrs = array;
	myIndex = index;
/*
udanax-top.st:55211:SetTableStepper methodsFor: 'protected: create'!
create: array {PtrArray} with: index {Int32}
	super create.
	myPtrs := array.
	myIndex := index.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:55218:SetTableStepper methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:55220:SetTableStepper methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void linkTimeNonInherited() {
	AnArray = null;
	AStepper = null;
/*
udanax-top.st:55231:SetTableStepper class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	AnArray := NULL.
	AStepper := NULL.!
*/
}
public static SetTableStepper make(PtrArray array) {
	if (AStepper != null) {
		SetTableStepper result;
		result = (
		/* TODO newBecome */
		new SetTableStepper(array));
		AStepper = null;
		return result;
	}
	else {
		return new SetTableStepper(array);
	}
/*
udanax-top.st:55237:SetTableStepper class methodsFor: 'create'!
make: array {PtrArray}
	AStepper ~~ NULL
		ifTrue: [
			| result {SetTableStepper} |
			result := ((SetTableStepper new.Become: AStepper) create: array) .
			AStepper := NULL.
			^ result]
		ifFalse: [
			^ SetTableStepper create: array]!
*/
}
public static PtrArray array() {
	if (AnArray != null) {
		PtrArray result;
		result = AnArray;
		AnArray = null;
		return result;
	}
	else {
		return PtrArray.nulls(16);
	}
/*
udanax-top.st:55249:SetTableStepper class methodsFor: 'accessing'!
{PtrArray} array
	AnArray ~~ NULL
		ifTrue: [
			| result {PtrArray} |
			result := AnArray.
			AnArray := NULL.
			^ result]
		ifFalse: [
			^ PtrArray nulls: 16]!
*/
}
public SetTableStepper() {
/*

Generated during transformation
*/
}
public SetTableStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
