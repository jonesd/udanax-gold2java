/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.settable;

import info.dgjones.abora.gold.collection.settable.IndexEntry;
import info.dgjones.abora.gold.collection.settable.TableEntry;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class IndexEntry extends TableEntry {

	protected int myIndex;
/*
udanax-top.st:56680:
TableEntry subclass: #IndexEntry
	instanceVariableNames: 'myIndex {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-SetTable'!
*/
/*
udanax-top.st:56684:
(IndexEntry getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IndexEntry.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public int index() {
	return myIndex;
/*
udanax-top.st:56689:IndexEntry methodsFor: 'accessing'!
{IntegerVar} index
	^ myIndex!
*/
}
/**
 * Return true if my key matches key.
 */
public boolean match(Position key) {
	if (key instanceof IntegerPos) {
		IntegerPos pos = (IntegerPos) key;
		return pos.asIntegerVar() == myIndex;
	}
	else {
		return false;
	}
/*
udanax-top.st:56692:IndexEntry methodsFor: 'accessing'!
{BooleanVar} match: key {Position}
	"Return true if my key matches key."
	
	key cast: IntegerPos into: [:pos | 
			^pos asIntegerVar == myIndex]
		others: [^false].
	^ false "compiler fodder"!
*/
}
/**
 * Return true if my key matches the position associated with index.
 */
public boolean matchInt(int index) {
	return index == myIndex;
/*
udanax-top.st:56700:IndexEntry methodsFor: 'accessing'!
{BooleanVar} matchInt: index {IntegerVar}
	"Return true if my key matches the position associated with index."
	
	^index == myIndex!
*/
}
public Position position() {
	return IntegerPos.make(myIndex);
/*
udanax-top.st:56705:IndexEntry methodsFor: 'accessing'!
{Position} position
	^myIndex integer!
*/
}
public TableEntry copy() {
	return new IndexEntry(myIndex, value());
/*
udanax-top.st:56710:IndexEntry methodsFor: 'creation'!
{TableEntry} copy
	^ IndexEntry create: myIndex with:self value!
*/
}
public IndexEntry(int index, Heaper value) {
	super(value);
	myIndex = index;
/*
udanax-top.st:56713:IndexEntry methodsFor: 'creation'!
create: index {IntegerVar} with: value {Heaper}
	super create: value.
	myIndex _ index!
*/
}
public IndexEntry(TableEntry next, Heaper value, int index) {
	super(next, value);
	myIndex = index;
/*
udanax-top.st:56717:IndexEntry methodsFor: 'creation'!
create: next {TableEntry} with: value {Heaper} with: index {IntegerVar}
	super create: next with: value.
	myIndex _ index!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(IntegerPos.make(myIndex));
	oo.print(" -> ");
	oo.print(value());
	oo.print(")");
/*
udanax-top.st:56723:IndexEntry methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myIndex integer << ' -> ' << self value << ')'!
*/
}
public IndexEntry(Rcvr receiver) {
	super(receiver);
	myIndex = receiver.receiveIntegerVar();
/*
udanax-top.st:56728:IndexEntry methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myIndex _ receiver receiveIntegerVar.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIntegerVar(myIndex);
/*
udanax-top.st:56732:IndexEntry methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendIntegerVar: myIndex.!
*/
}
public IndexEntry() {
/*

Generated during transformation
*/
}
}
