/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.steppers;

import info.dgjones.abora.gold.collection.steppers.ITGenericStepper;
import info.dgjones.abora.gold.collection.steppers.IntegerTableStepper;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.IntegerTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class ITGenericStepper extends IntegerTableStepper {

	protected IntegerTable arrayInternal;
	protected int indexInternal;
	protected int lastValueInternal;
	protected int incrementInternal;
/*
udanax-top.st:56012:
IntegerTableStepper subclass: #ITGenericStepper
	instanceVariableNames: '
		arrayInternal {IntegerTable}
		indexInternal {IntegerVar}
		lastValueInternal {IntegerVar}
		incrementInternal {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Steppers'!
*/
/*
udanax-top.st:56020:
(ITGenericStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ITGenericStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	if (hasValue()) {
		return arrayInternal.intFetch(indexInternal);
	}
	else {
		return null;
	}
/*
udanax-top.st:56025:ITGenericStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	self hasValue
		ifTrue: [^ arrayInternal intFetch: indexInternal]
		ifFalse: [^NULL]!
*/
}
public boolean hasValue() {
	return (incrementInternal > 0 && (indexInternal <= lastValueInternal)) || (incrementInternal < 0 && (indexInternal >= lastValueInternal));
/*
udanax-top.st:56030:ITGenericStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^(incrementInternal > Int32Zero and: [indexInternal <= lastValueInternal])
		or: [incrementInternal < Int32Zero and: [indexInternal >= lastValueInternal]]!
*/
}
public void step() {
	indexInternal = indexInternal + incrementInternal;
	verifyEntry();
/*
udanax-top.st:56034:ITGenericStepper methodsFor: 'operations'!
{void} step
	indexInternal _ indexInternal + incrementInternal.
	self verifyEntry!
*/
}
public int index() {
	return indexInternal;
/*
udanax-top.st:56040:ITGenericStepper methodsFor: 'special'!
{IntegerVar} index
	^indexInternal!
*/
}
public Position position() {
	return IntegerPos.make(indexInternal);
/*
udanax-top.st:56043:ITGenericStepper methodsFor: 'special'!
{Position} position
	^indexInternal integer!
*/
}
public Stepper copy() {
	return new ITGenericStepper(arrayInternal, indexInternal, lastValueInternal, incrementInternal);
/*
udanax-top.st:56048:ITGenericStepper methodsFor: 'create'!
{Stepper} copy
	^ITGenericStepper
		create: arrayInternal
		with: indexInternal
		with: lastValueInternal
		with: incrementInternal!
*/
}
public ITGenericStepper(IntegerTable array) {
	super();
	arrayInternal = ((IntegerTable) array.copy());
	indexInternal = 0;
	lastValueInternal = arrayInternal.highestIndex();
	incrementInternal = 1;
/*
udanax-top.st:56055:ITGenericStepper methodsFor: 'create'!
create: array {IntegerTable}
	super create.
	arrayInternal _ (array copy cast: IntegerTable).
	indexInternal _ IntegerVar0.
	lastValueInternal _ arrayInternal highestIndex.
	incrementInternal _ 1!
*/
}
public ITGenericStepper(IntegerTable onTable, OrderSpec anOrder) {
	super();
	if ((anOrder.followsInt(1, 0))
	/* order is ascending */
	) {
		arrayInternal = ((IntegerTable) onTable.copy());
		indexInternal = onTable.lowestIndex();
		lastValueInternal = onTable.highestIndex();
		incrementInternal = 1;
	}
	else 
	/* order is descending */
	{
		arrayInternal = ((IntegerTable) onTable.copy());
		indexInternal = onTable.highestIndex();
		lastValueInternal = onTable.lowestIndex();
		incrementInternal = -1;
	}
/*
udanax-top.st:56062:ITGenericStepper methodsFor: 'create'!
create: onTable {IntegerTable} with.OrderSpec: anOrder {OrderSpec} 
	
	super create.
	(anOrder followsInt: 1 with: IntegerVar0)
		ifTrue: "order is ascending"
			[arrayInternal _ (onTable copy cast: IntegerTable).
			indexInternal _ onTable lowestIndex.
			lastValueInternal _ onTable highestIndex.
			incrementInternal _ 1]
		ifFalse: "order is descending"
			[arrayInternal _ (onTable copy cast: IntegerTable).
			indexInternal _ onTable highestIndex.
			lastValueInternal _ onTable lowestIndex.
			incrementInternal _ -1]!
*/
}
public ITGenericStepper(IntegerTable array, int index) {
	super();
	arrayInternal = (IntegerTable) array.copy();
	indexInternal = index;
	lastValueInternal = arrayInternal.highestIndex();
	incrementInternal = 1;
/*
udanax-top.st:56077:ITGenericStepper methodsFor: 'create'!
create: array {IntegerTable} with: index {IntegerVar}
	super create.
	arrayInternal _ array copy cast: IntegerTable.
	indexInternal _ index.
	lastValueInternal _ arrayInternal highestIndex.
	incrementInternal _ 1!
*/
}
public ITGenericStepper(IntegerTable array, int start, int stop) {
	super();
	arrayInternal = (IntegerTable) array.copy();
	indexInternal = start;
	lastValueInternal = stop;
	incrementInternal = 1;
/*
udanax-top.st:56084:ITGenericStepper methodsFor: 'create'!
create: array {IntegerTable} with: start {IntegerVar} with: stop {IntegerVar}
	super create.
	arrayInternal _ array copy cast: IntegerTable.
	indexInternal _ start.
	lastValueInternal _ stop.
	incrementInternal _ 1!
*/
}
public ITGenericStepper(IntegerTable array, int start, int stop, int direction) {
	super();
	arrayInternal = (IntegerTable) array.copy();
	indexInternal = start;
	lastValueInternal = stop;
	incrementInternal = direction;
/*
udanax-top.st:56091:ITGenericStepper methodsFor: 'create'!
create: array {IntegerTable} with: start {IntegerVar} with: stop {IntegerVar} with: direction {IntegerVar}
	super create.
	arrayInternal _ array copy cast: IntegerTable.
	indexInternal _ start.
	lastValueInternal _ stop.
	incrementInternal _ direction DOTasLong!
*/
}
public void verifyEntry() {
	boolean notDone;
	notDone = true;
	while (notDone) {
		if ((incrementInternal > 0 && (indexInternal < lastValueInternal)) || (incrementInternal < 0 && (indexInternal >= lastValueInternal))) {
			if ((arrayInternal.intFetch(indexInternal)) == null) {
				indexInternal = indexInternal + incrementInternal;
			}
			else {
				notDone = false;
			}
		}
		else {
			notDone = false;
		}
	}
/*
udanax-top.st:56100:ITGenericStepper methodsFor: 'private: private'!
{void} verifyEntry
	| notDone {BooleanVar} |
	notDone _ true.
	[notDone]
		whileTrue: [
			((incrementInternal > Int32Zero and: [indexInternal < lastValueInternal])
					or: [incrementInternal < Int32Zero and: [indexInternal >= lastValueInternal]])
				ifTrue: [(arrayInternal intFetch: indexInternal) == NULL
						ifTrue: [indexInternal _ indexInternal + incrementInternal]
						ifFalse: [notDone _ false]]
				ifFalse: [notDone _ false]]!
*/
}
public ITGenericStepper() {
/*

Generated during transformation
*/
}
public ITGenericStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
