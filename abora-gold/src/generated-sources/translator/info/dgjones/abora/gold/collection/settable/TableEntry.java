/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.settable;

import info.dgjones.abora.gold.collection.basic.SharedPtrArray;
import info.dgjones.abora.gold.collection.settable.BucketArrayStepper;
import info.dgjones.abora.gold.collection.settable.HashIndexEntry;
import info.dgjones.abora.gold.collection.settable.HeaperAsEntry;
import info.dgjones.abora.gold.collection.settable.IndexEntry;
import info.dgjones.abora.gold.collection.settable.PositionEntry;
import info.dgjones.abora.gold.collection.settable.TableEntry;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.unordered.HeaperAsPosition;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class TableEntry extends Heaper {

	protected TableEntry myNext;
	protected Heaper myValue;
/*
udanax-top.st:56469:
Heaper subclass: #TableEntry
	instanceVariableNames: '
		myNext {TableEntry}
		myValue {Heaper}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-SetTable'!
*/
/*
udanax-top.st:56475:
(TableEntry getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #EQ; add: #DEFERRED; add: #COPY; yourself)!
*/
/*
udanax-top.st:56563:
TableEntry class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:56566:
(TableEntry getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #EQ; add: #DEFERRED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TableEntry.class).setAttributes( new Set().add("EQ").add("DEFERRED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public TableEntry copy() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:56480:TableEntry methodsFor: 'accessing'!
{TableEntry} copy
	self subclassResponsibility!
*/
}
public TableEntry fetchNext() {
	return myNext;
/*
udanax-top.st:56484:TableEntry methodsFor: 'accessing'!
{TableEntry INLINE | NULL} fetchNext
	^myNext!
*/
}
public int index() {
	return ((IntegerPos) position()).asIntegerVar();
/*
udanax-top.st:56487:TableEntry methodsFor: 'accessing'!
{IntegerVar} index
	^ (self position cast: IntegerPos) asIntegerVar!
*/
}
/**
 * Return true if my key matches key.
 */
public boolean match(Position key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:56490:TableEntry methodsFor: 'accessing'!
{BooleanVar} match: key {Position}
	"Return true if my key matches key."
	
	self subclassResponsibility!
*/
}
/**
 * Return true if my key matches the position associated with index.
 */
public boolean matchInt(int index) {
	return match(IntegerPos.make(index));
/*
udanax-top.st:56495:TableEntry methodsFor: 'accessing'!
{BooleanVar} matchInt: index {IntegerVar}
	"Return true if my key matches the position associated with index."
	
	^self match: index integer!
*/
}
/**
 * Return true if my value matches value.  Note that this *must* test EQ first in
 * case the value is no longer a heaper.  Otherwise we could never remove a
 * destructed object.
 */
public boolean matchValue(Heaper value) {
	return value == (myValue) || (value.isEqual(myValue));
/*
udanax-top.st:56500:TableEntry methodsFor: 'accessing'!
{BooleanVar} matchValue: value {Heaper}
	"Return true if my value matches value.  Note that this *must* test EQ first in
	 case the value is no longer a heaper.  Otherwise we could never remove a 
	 destructed object."
	
	^value == (myValue basicCast: Heaper star) or: [value isEqual: myValue]!
*/
}
public Position position() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:56507:TableEntry methodsFor: 'accessing'!
{Position} position
	self subclassResponsibility!
*/
}
/**
 * Return true if my value can be replaced in place, and false if the entire entry must be
 * replaced.
 */
public boolean replaceValue(Heaper newValue) {
	/* The default implementation. */
	myValue = newValue;
	return true;
/*
udanax-top.st:56510:TableEntry methodsFor: 'accessing'!
{BooleanVar} replaceValue: newValue {Heaper}
	"Return true if my value can be replaced in place, and false if the entire entry must be replaced."
	
	"The default implementation."
	myValue _ newValue.
	^true!
*/
}
/**
 * Change my pointer to the rest of the chain in this bucket.
 */
public void setNext(TableEntry next) {
	myNext = next;
/*
udanax-top.st:56517:TableEntry methodsFor: 'accessing'!
{void INLINE} setNext: next {TableEntry | NULL}
	"Change my pointer to the rest of the chain in this bucket."
	myNext _ next!
*/
}
public Heaper value() {
	return myValue;
/*
udanax-top.st:56521:TableEntry methodsFor: 'accessing'!
{Heaper INLINE} value
	^myValue!
*/
}
public TableEntry(Heaper value) {
	super();
	myNext = null;
	myValue = value;
/*
udanax-top.st:56526:TableEntry methodsFor: 'protected: creation'!
create: value {Heaper}
	super create.
	myNext _ NULL.
	myValue _ value!
*/
}
public TableEntry(TableEntry next, Heaper value) {
	super();
	myNext = next;
	myValue = value;
/*
udanax-top.st:56531:TableEntry methodsFor: 'protected: creation'!
create: next {TableEntry} with: value {Heaper}
	super create.
	myNext _ next.
	myValue _ value!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(position());
	oo.print(" -> ");
	oo.print(value());
	oo.print(")");
/*
udanax-top.st:56538:TableEntry methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << self position << ' -> ' << self value << ')'!
*/
}
/**
 * temporarily don't destroy.
 */
public void destroy() {
/*
udanax-top.st:56543:TableEntry methodsFor: 'destroy'!
{void} destroy
	"temporarily don't destroy."!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:56548:TableEntry methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public TableEntry(Rcvr receiver) {
	super(receiver);
	myNext = (TableEntry) receiver.receiveHeaper();
	myValue = receiver.receiveHeaper();
/*
udanax-top.st:56550:TableEntry methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myNext _ receiver receiveHeaper.
	myValue _ receiver receiveHeaper.!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:56555:TableEntry methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myNext);
	xmtr.sendHeaper(myValue);
/*
udanax-top.st:56557:TableEntry methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myNext.
	xmtr sendHeaper: myValue.!
*/
}
public static TableStepper bucketStepper(SharedPtrArray array) {
	return BucketArrayStepper.make(array);
/*
udanax-top.st:56571:TableEntry class methodsFor: 'creation'!
{TableStepper INLINE} bucketStepper: array {SharedPtrArray}
	^BucketArrayStepper make: array!
*/
}
public static TableEntry makeIntegerVar(int index, Heaper value) {
	if (index == value.hashForEqual()) {
		return new HashIndexEntry(value);
	}
	else {
		return new IndexEntry(index, value);
	}
/*
udanax-top.st:56574:TableEntry class methodsFor: 'creation'!
make.IntegerVar: index {IntegerVar} with: value {Heaper}
	index == value hashForEqual
		ifTrue: [^HashIndexEntry create: value]
		ifFalse: [^IndexEntry create: index with: value]!
*/
}
public static TableEntry make(Position key, Heaper value) {
	if (key instanceof IntegerPos) {
		IntegerPos xuint = (IntegerPos) key;
		return makeIntegerVar(xuint.asIntegerVar(), value);
	}
	else if (key instanceof HeaperAsPosition) {
		HeaperAsPosition hap = (HeaperAsPosition) key;
		if (key.isEqual((HeaperAsPosition.make(value)))) {
			return new HeaperAsEntry(value);
		}
		else {
			return new PositionEntry(key, value);
		}
	}
	else {
		return new PositionEntry(key, value);
	}
/*
udanax-top.st:56579:TableEntry class methodsFor: 'creation'!
make: key {Position} with: value {Heaper}
	key cast: IntegerPos into: [:xuint | 
			^self make.IntegerVar: xuint asIntegerVar with: value]
		cast: HeaperAsPosition into: [:hap |
				(key isEqual: (HeaperAsPosition make: value))
					ifTrue: [ ^HeaperAsEntry create: value]
					ifFalse: [^PositionEntry create: key with: value]]
		others: [^PositionEntry create: key with: value].
	^ NULL "compiler fodder"!
*/
}
public TableEntry() {
/*

Generated during transformation
*/
}
}
