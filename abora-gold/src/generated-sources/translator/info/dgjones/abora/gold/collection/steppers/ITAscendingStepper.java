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
import info.dgjones.abora.gold.collection.steppers.IntegerTableStepper;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.OberIntegerTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class ITAscendingStepper extends IntegerTableStepper {

	protected OberIntegerTable arrayInternal;
	protected int indexInternal;
	protected int lastValueInternal;
/*
udanax-top.st:55869:
IntegerTableStepper subclass: #ITAscendingStepper
	instanceVariableNames: '
		arrayInternal {OberIntegerTable}
		indexInternal {UInt32}
		lastValueInternal {UInt32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Steppers'!
*/
/*
udanax-top.st:55876:
(ITAscendingStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ITAscendingStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	if (indexInternal <= lastValueInternal) {
		return arrayInternal.elementsArray().fetch(indexInternal);
	}
	else {
		return null;
	}
/*
udanax-top.st:55881:ITAscendingStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	(indexInternal <= lastValueInternal)
		ifTrue: [^arrayInternal elementsArray fetch: indexInternal]
		ifFalse: [^NULL]!
*/
}
public boolean hasValue() {
	return indexInternal <= lastValueInternal;
/*
udanax-top.st:55886:ITAscendingStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^ indexInternal <= lastValueInternal!
*/
}
public void step() {
	indexInternal = indexInternal + 1;
	while (indexInternal <= lastValueInternal && ((arrayInternal.elementsArray().fetch(indexInternal)) == null)) {
		indexInternal = indexInternal + 1;
	}
/*
udanax-top.st:55889:ITAscendingStepper methodsFor: 'operations'!
{void} step
	indexInternal _ indexInternal + 1.
	[indexInternal <= lastValueInternal and: 
			[(arrayInternal elementsArray fetch: indexInternal) == NULL]]
		whileTrue: [indexInternal _ indexInternal + 1]!
*/
}
public Stepper copy() {
	return new ITAscendingStepper(((OberIntegerTable) arrayInternal.copy()), index(), arrayInternal.startIndex() + lastValueInternal);
/*
udanax-top.st:55897:ITAscendingStepper methodsFor: 'create'!
{Stepper} copy
	^ITAscendingStepper
		create: (arrayInternal copy cast: OberIntegerTable)
		with: self index
		with: arrayInternal startIndex + lastValueInternal!
*/
}
public ITAscendingStepper(OberIntegerTable array) {
	super();
	arrayInternal = ((OberIntegerTable) array.copy());
	indexInternal = arrayInternal.startOffset();
	lastValueInternal = arrayInternal.endOffset();
	verifyEntry();
/*
udanax-top.st:55903:ITAscendingStepper methodsFor: 'create'!
create: array {OberIntegerTable}
	super create.
	arrayInternal _ (array copy cast: OberIntegerTable).
	indexInternal _ arrayInternal startOffset.
	lastValueInternal _ arrayInternal endOffset.
	self verifyEntry!
*/
}
public ITAscendingStepper(OberIntegerTable array, int index) {
	super();
	arrayInternal = ((OberIntegerTable) array.copy());
	indexInternal = (index - arrayInternal.startIndex());
	lastValueInternal = arrayInternal.endOffset();
	verifyEntry();
/*
udanax-top.st:55910:ITAscendingStepper methodsFor: 'create'!
create: array {OberIntegerTable} with: index {IntegerVar}
	super create.
	arrayInternal _ (array copy cast: OberIntegerTable).
	indexInternal _ (index - arrayInternal startIndex) DOTasLong.
	lastValueInternal _ arrayInternal endOffset.
	self verifyEntry!
*/
}
/**
 * n.b. !!!!!!!! This constructor DOES NOT COPY the table because this
 * constructor is used by the table copy (which creates a stepper).
 * The copy is done in the table->stepper(NULL) routine before calling
 * this constructor.
 */
public ITAscendingStepper(OberIntegerTable array, int start, int stop) {
	super();
	arrayInternal = array;
	indexInternal = (start - arrayInternal.startIndex());
	lastValueInternal = (stop - arrayInternal.startIndex());
	verifyEntry();
/*
udanax-top.st:55917:ITAscendingStepper methodsFor: 'create'!
create: array {OberIntegerTable} with: start {IntegerVar} with: stop {IntegerVar} 
	"n.b. !!!!!!!! This constructor DOES NOT COPY the table because this 
	constructor is used by the table copy (which creates a stepper). 
	The copy is done in the table->stepper(NULL) routine before calling 
	this constructor."
	super create.
	arrayInternal _ array.
	indexInternal _ (start - arrayInternal startIndex) DOTasLong.
	lastValueInternal _ (stop - arrayInternal startIndex) DOTasLong.
	self verifyEntry!
*/
}
public int index() {
	return arrayInternal.startIndex() + indexInternal;
/*
udanax-top.st:55931:ITAscendingStepper methodsFor: 'special'!
{IntegerVar} index
	^arrayInternal startIndex + indexInternal!
*/
}
public Position position() {
	return IntegerPos.make(index());
/*
udanax-top.st:55934:ITAscendingStepper methodsFor: 'special'!
{Position} position
	^ self index integer!
*/
}
public void verifyEntry() {
	while (indexInternal <= lastValueInternal && ((arrayInternal.elementsArray().fetch(indexInternal)) == null)) {
		indexInternal = indexInternal + 1;
	}
/*
udanax-top.st:55939:ITAscendingStepper methodsFor: 'private: private'!
{void} verifyEntry
	[indexInternal <= lastValueInternal and: 
			[(arrayInternal elementsArray fetch: indexInternal) == NULL]]
		whileTrue: [indexInternal _ indexInternal + 1]!
*/
}
public ITAscendingStepper() {
/*

Generated during transformation
*/
}
public ITAscendingStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
