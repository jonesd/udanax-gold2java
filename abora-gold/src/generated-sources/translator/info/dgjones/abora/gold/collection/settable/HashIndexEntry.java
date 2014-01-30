/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.settable;

import info.dgjones.abora.gold.collection.settable.HashIndexEntry;
import info.dgjones.abora.gold.collection.settable.TableEntry;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class HashIndexEntry extends TableEntry {

/*
udanax-top.st:56590:
TableEntry subclass: #HashIndexEntry
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-SetTable'!
*/
/*
udanax-top.st:56594:
(HashIndexEntry getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HashIndexEntry.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Return true if my key matches key.
 */
public boolean match(Position key) {
	if (key instanceof IntegerPos) {
		IntegerPos pos = (IntegerPos) key;
		return pos.asIntegerVar() == value().hashForEqual();
	}
	else {
		return false;
	}
/*
udanax-top.st:56599:HashIndexEntry methodsFor: 'accessing'!
{BooleanVar} match: key {Position}
	"Return true if my key matches key."
	
	key cast: IntegerPos into: [:pos | 
			^pos asIntegerVar == self value hashForEqual]
		others: [^false].
	^ false "compiler fodder"!
*/
}
/**
 * Return true if my key matches the position associated with index.
 */
public boolean matchInt(int index) {
	return index == value().hashForEqual();
/*
udanax-top.st:56607:HashIndexEntry methodsFor: 'accessing'!
{BooleanVar} matchInt: index {IntegerVar}
	"Return true if my key matches the position associated with index."
	
	^index == self value hashForEqual!
*/
}
public Position position() {
	return IntegerPos.make(value().hashForEqual());
/*
udanax-top.st:56612:HashIndexEntry methodsFor: 'accessing'!
{Position} position
	^self value hashForEqual integer!
*/
}
/**
 * Return true if my value can be replaced in place, and false if the entire entry must be
 * replaced.
 */
public boolean replaceValue(Heaper newValue) {
	return newValue.hashForEqual() == value().hashForEqual() && (super.replaceValue(newValue));
/*
udanax-top.st:56615:HashIndexEntry methodsFor: 'accessing'!
{BooleanVar} replaceValue: newValue {Heaper}
	"Return true if my value can be replaced in place, and false if the entire entry must be replaced."
	
	^newValue hashForEqual == self value hashForEqual and: [super replaceValue: newValue]!
*/
}
public TableEntry copy() {
	return new HashIndexEntry(value());
/*
udanax-top.st:56622:HashIndexEntry methodsFor: 'creation'!
{TableEntry} copy
	^ HashIndexEntry create: self value!
*/
}
public HashIndexEntry(Heaper value) {
	super(value);
/*
udanax-top.st:56625:HashIndexEntry methodsFor: 'creation'!
create: value {Heaper}
	super create: value!
*/
}
public HashIndexEntry(TableEntry next, Heaper value) {
	super(next, value);
/*
udanax-top.st:56628:HashIndexEntry methodsFor: 'creation'!
create: next {TableEntry} with: value {Heaper}
	super create: next with: value!
*/
}
public HashIndexEntry(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:56633:HashIndexEntry methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:56636:HashIndexEntry methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public HashIndexEntry() {
/*

Generated during transformation
*/
}
}
