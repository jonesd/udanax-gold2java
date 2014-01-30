/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.steppers;

import info.dgjones.abora.gold.collection.steppers.ITDescendingStepper;
import info.dgjones.abora.gold.collection.steppers.IntegerTableStepper;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.ActualIntegerTable;
import info.dgjones.abora.gold.collection.tables.OberIntegerTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class ITDescendingStepper extends IntegerTableStepper {

	protected OberIntegerTable arrayInternal;
	protected int indexInternal;
	protected int lastValueInternal;
/*
udanax-top.st:55944:
IntegerTableStepper subclass: #ITDescendingStepper
	instanceVariableNames: '
		arrayInternal {OberIntegerTable}
		indexInternal {UInt32}
		lastValueInternal {UInt32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Steppers'!
*/
/*
udanax-top.st:55951:
(ITDescendingStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ITDescendingStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Stepper copy() {
	return new ITDescendingStepper(((ActualIntegerTable) arrayInternal.copy()), index(), arrayInternal.startIndex() + lastValueInternal);
/*
udanax-top.st:55956:ITDescendingStepper methodsFor: 'create'!
{Stepper} copy
	^ITDescendingStepper
		create: (arrayInternal copy cast: ActualIntegerTable)
		with: self index
		with: arrayInternal startIndex + lastValueInternal!
*/
}
public ITDescendingStepper(OberIntegerTable array) {
	super();
	arrayInternal = (OberIntegerTable) array.copy();
	indexInternal = arrayInternal.endOffset();
	lastValueInternal = arrayInternal.startOffset();
	verifyEntry();
/*
udanax-top.st:55962:ITDescendingStepper methodsFor: 'create'!
create: array {OberIntegerTable} 
	super create.
	arrayInternal _ array copy cast: OberIntegerTable.
	indexInternal _ arrayInternal endOffset.
	lastValueInternal _ arrayInternal startOffset.
	self verifyEntry!
*/
}
public ITDescendingStepper(OberIntegerTable array, int index) {
	super();
	arrayInternal = (OberIntegerTable) array.copy();
	indexInternal = (index - arrayInternal.startIndex());
	lastValueInternal = arrayInternal.startOffset();
	verifyEntry();
/*
udanax-top.st:55969:ITDescendingStepper methodsFor: 'create'!
create: array {OberIntegerTable} with: index {IntegerVar} 
	super create.
	arrayInternal _ array copy cast: OberIntegerTable.
	indexInternal _ (index - arrayInternal startIndex) DOTasLong.
	lastValueInternal _ arrayInternal startOffset.
	self verifyEntry!
*/
}
public ITDescendingStepper(OberIntegerTable array, int start, int stop) {
	super();
	arrayInternal = (OberIntegerTable) array.copy();
	indexInternal = (start - arrayInternal.startIndex());
	lastValueInternal = (stop - arrayInternal.startIndex());
	verifyEntry();
/*
udanax-top.st:55976:ITDescendingStepper methodsFor: 'create'!
create: array {OberIntegerTable} with: start {IntegerVar} with: stop {IntegerVar} 
	super create.
	arrayInternal _ array copy cast: OberIntegerTable.
	indexInternal _ (start - arrayInternal startIndex) DOTasLong.
	lastValueInternal _ (stop - arrayInternal startIndex) DOTasLong.
	self verifyEntry!
*/
}
public Heaper fetch() {
	if (indexInternal >= lastValueInternal) {
		return arrayInternal.elementsArray().fetch(indexInternal);
	}
	else {
		return null;
	}
/*
udanax-top.st:55985:ITDescendingStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	indexInternal >= lastValueInternal
		ifTrue: [^arrayInternal elementsArray fetch: indexInternal]
		ifFalse: [^NULL]!
*/
}
public boolean hasValue() {
	return indexInternal >= lastValueInternal;
/*
udanax-top.st:55990:ITDescendingStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^indexInternal >= lastValueInternal!
*/
}
public void step() {
	indexInternal = indexInternal - 1;
	verifyEntry();
/*
udanax-top.st:55993:ITDescendingStepper methodsFor: 'operations'!
{void} step
	indexInternal _ indexInternal - 1.
	self verifyEntry!
*/
}
public int index() {
	return arrayInternal.startIndex() + indexInternal;
/*
udanax-top.st:55999:ITDescendingStepper methodsFor: 'special'!
{IntegerVar} index
	^ arrayInternal startIndex + indexInternal!
*/
}
public Position position() {
	return IntegerPos.make(index());
/*
udanax-top.st:56002:ITDescendingStepper methodsFor: 'special'!
{Position} position
	^ self index integer!
*/
}
public void verifyEntry() {
	while (indexInternal >= lastValueInternal && ((arrayInternal.elementsArray().fetch(indexInternal)) == null)) {
		indexInternal = indexInternal - 1;
	}
/*
udanax-top.st:56007:ITDescendingStepper methodsFor: 'private: private'!
{void} verifyEntry
	[indexInternal >= lastValueInternal and: 
			[(arrayInternal elementsArray fetch: indexInternal) == NULL]]
		whileTrue: [indexInternal _ indexInternal - 1]!
*/
}
public ITDescendingStepper() {
/*

Generated during transformation
*/
}
public ITDescendingStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
