/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.steppers;

import info.dgjones.abora.gold.collection.steppers.ITAscendingStepper;
import info.dgjones.abora.gold.collection.steppers.ITDescendingStepper;
import info.dgjones.abora.gold.collection.steppers.ITGenericStepper;
import info.dgjones.abora.gold.collection.steppers.IntegerTableStepper;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.ActualIntegerTable;
import info.dgjones.abora.gold.collection.tables.IntegerTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Consider this a protected class.  It is public only for use by the "array" module.
 */
public class IntegerTableStepper extends TableStepper {

/*
udanax-top.st:55790:
TableStepper subclass: #IntegerTableStepper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Steppers'!
*/
/*
udanax-top.st:55794:
IntegerTableStepper comment:
'Consider this a protected class.  It is public only for use by the "array" module.'!
*/
/*
udanax-top.st:55796:
(IntegerTableStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:55827:
IntegerTableStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:55830:
(IntegerTableStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IntegerTableStepper.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:55801:IntegerTableStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	self subclassResponsibility!
*/
}
public Heaper get() {
	Heaper res;
	res = fetch();
	if (res == null) {
		throw new AboraRuntimeException(AboraRuntimeException.EMPTY_STEPPER);
	}
	return res;
/*
udanax-top.st:55804:IntegerTableStepper methodsFor: 'operations'!
{Heaper wimpy} get
	| res {Heaper wimpy} |
	res _ self fetch.
	res == NULL ifTrue: [Heaper BLAST: #EmptyStepper].
	^res!
*/
}
public boolean hasValue() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:55810:IntegerTableStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	self subclassResponsibility!
*/
}
public void step() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:55813:IntegerTableStepper methodsFor: 'operations'!
{void} step
	self subclassResponsibility!
*/
}
public Position position() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:55818:IntegerTableStepper methodsFor: 'special'!
{Position} position
	self subclassResponsibility!
*/
}
public Stepper copy() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:55823:IntegerTableStepper methodsFor: 'create'!
{Stepper} copy
	self subclassResponsibility!
*/
}
/**
 * Do not consider public.  Only for use by the modules inttab, array, and awarray.
 */
public static IntegerTableStepper make(IntegerTable aTable, OrderSpec anOrder) {
	if (aTable instanceof ActualIntegerTable) {
		ActualIntegerTable tab = (ActualIntegerTable) aTable;
		if (anOrder.followsInt(1, 0)) {
			return new ITAscendingStepper(tab);
		}
		else {
			return new ITDescendingStepper(tab);
		}
	}
	else {
		if (anOrder == null) {
			return new ITGenericStepper(aTable);
		}
		else {
			return new ITGenericStepper(aTable, anOrder);
		}
	}
/*
udanax-top.st:55835:IntegerTableStepper class methodsFor: 'pseudoConstructors'!
make: aTable {IntegerTable} with: anOrder {OrderSpec default: NULL} 
	"Do not consider public.  Only for use by the modules inttab, array, and awarray."
	
	aTable cast: ActualIntegerTable into: [:tab |
			(anOrder followsInt: 1  with: IntegerVar0)
				ifTrue: [^ITAscendingStepper create: tab]
				ifFalse: [^ITDescendingStepper create: tab]]
		others: [anOrder == NULL
				ifTrue: [^ITGenericStepper create: aTable]
				ifFalse: [^ITGenericStepper create: aTable with.OrderSpec: anOrder]].
	^ NULL "compiler fodder"!
*/
}
/**
 * Do not consider public.  Only for use by the modules inttab, array, and awarray.
 */
public static IntegerTableStepper make(IntegerTable aTable, int start, int stop) {
	if (aTable instanceof ActualIntegerTable) {
		ActualIntegerTable tab = (ActualIntegerTable) aTable;
		return new ITAscendingStepper(tab, start, stop);
	}
	else {
		return new ITGenericStepper(aTable, start, stop, 1);
	}
/*
udanax-top.st:55847:IntegerTableStepper class methodsFor: 'pseudoConstructors'!
make: aTable {IntegerTable} 
	with: start {IntegerVar} 
	with: stop {IntegerVar} 
	"Do not consider public.  Only for use by the modules inttab, array, and awarray."
	
	aTable cast: ActualIntegerTable into: [:tab |
			^ITAscendingStepper
				create: tab
				with: start
				with: stop]
		others: [^ITGenericStepper
				create: aTable
				with: start
				with: stop
				with: 1].
	^ NULL "compiler fodder"!
*/
}
/*
udanax-top.st:55866:IntegerTableStepper class methodsFor: 'smalltalk: smalltalk creation'!
create: onTable {IntegerTable} with.OrderSpec: anOrderSpec {OrderSpec}
	^ self new create: onTable with.OrderSpec: anOrderSpec!
*/
public IntegerTableStepper() {
/*

Generated during transformation
*/
}
public IntegerTableStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
