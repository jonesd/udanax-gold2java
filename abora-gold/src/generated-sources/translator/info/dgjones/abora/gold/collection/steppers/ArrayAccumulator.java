/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.steppers;

import info.dgjones.abora.gold.collection.steppers.Accumulator;
import info.dgjones.abora.gold.collection.steppers.ArrayAccumulator;
import info.dgjones.abora.gold.collection.steppers.TableAccumulator;
import info.dgjones.abora.gold.collection.tables.MuArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class ArrayAccumulator extends TableAccumulator {

	protected MuArray arrayInternal;
/*
udanax-top.st:12425:
TableAccumulator subclass: #ArrayAccumulator
	instanceVariableNames: 'arrayInternal {MuArray}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Steppers'!
*/
/*
udanax-top.st:12429:
(ArrayAccumulator getOrMakeCxxClassDescription)
	friends:
'friend class XuArray;';
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:12456:
ArrayAccumulator class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:12459:
(ArrayAccumulator getOrMakeCxxClassDescription)
	friends:
'friend class XuArray;';
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ArrayAccumulator.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public ArrayAccumulator(MuArray onTable) {
	super();
	arrayInternal = onTable;
/*
udanax-top.st:12436:ArrayAccumulator methodsFor: 'protected: create'!
create: onTable {MuArray}
	super create.
	arrayInternal _ onTable!
*/
}
public void step(Heaper obj) {
	if (arrayInternal.isEmpty()) {
		arrayInternal.intStore(0, obj);
	}
	else {
		arrayInternal.intIntroduce(((IntegerRegion) arrayInternal.domain()).stop(), obj);
	}
/*
udanax-top.st:12442:ArrayAccumulator methodsFor: 'operations'!
{void} step: obj {Heaper} 
	arrayInternal isEmpty
		ifTrue: [arrayInternal atInt: IntegerVar0 store: obj]
		ifFalse: [arrayInternal atInt: (arrayInternal domain quickCast: IntegerRegion) stop introduce: obj]!
*/
}
public Heaper value() {
	return arrayInternal;
/*
udanax-top.st:12447:ArrayAccumulator methodsFor: 'operations'!
{Heaper} value
	^ arrayInternal.!
*/
}
public Accumulator copy() {
	return ArrayAccumulator.make(((MuArray) arrayInternal.copy()));
/*
udanax-top.st:12452:ArrayAccumulator methodsFor: 'create'!
{Accumulator} copy
	^ ArrayAccumulator make: (arrayInternal copy cast: MuArray)!
*/
}
public static TableAccumulator make(MuArray onTable) {
	return new ArrayAccumulator(onTable);
/*
udanax-top.st:12466:ArrayAccumulator class methodsFor: 'create'!
{TableAccumulator} make: onTable {MuArray}
	^ self create: onTable!
*/
}
/*
udanax-top.st:12471:ArrayAccumulator class methodsFor: 'smalltalk: creation'!
create.IntegerTable: aTable	
	^self new create: aTable!
*/
public ArrayAccumulator() {
/*

Generated during transformation
*/
}
public ArrayAccumulator(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
