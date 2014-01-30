/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.tables;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.COWIntegerTable;
import info.dgjones.abora.gold.collection.tables.OberIntegerTable;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class COWIntegerTable extends OberIntegerTable {

	protected OberIntegerTable myPrev;
	protected OberIntegerTable myTable;
/*
udanax-top.st:50193:
OberIntegerTable subclass: #COWIntegerTable
	instanceVariableNames: '
		myPrev {OberIntegerTable NOCOPY | NULL}
		myTable {OberIntegerTable}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Tables'!
*/
/*
udanax-top.st:50199:
(COWIntegerTable getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #(MAY.BECOME ActualIntegerTable ); add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(COWIntegerTable.class).setAttributes( new Set().add( new String[]
	{"MAYBECOME", "ActualIntegerTable"}).add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper intStore(int aKey, Heaper anObject) {
	aboutToWrite();
	return intStore(aKey, anObject);
/*
udanax-top.st:50204:COWIntegerTable methodsFor: 'accessing'!
{Heaper} atInt: aKey {IntegerVar} store: anObject {Heaper}
	self aboutToWrite.
	^ self atInt: aKey store: anObject!
*/
}
public CoordinateSpace coordinateSpace() {
	return myTable.coordinateSpace();
/*
udanax-top.st:50208:COWIntegerTable methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	
	^ myTable coordinateSpace!
*/
}
public int count() {
	return myTable.count();
/*
udanax-top.st:50212:COWIntegerTable methodsFor: 'accessing'!
{IntegerVar} count
	^ myTable count!
*/
}
public XnRegion domain() {
	return myTable.domain();
/*
udanax-top.st:50216:COWIntegerTable methodsFor: 'accessing'!
{XnRegion} domain
	^ myTable domain!
*/
}
public int highestIndex() {
	return myTable.highestIndex();
/*
udanax-top.st:50220:COWIntegerTable methodsFor: 'accessing'!
{IntegerVar} highestIndex
	^ myTable highestIndex!
*/
}
public Heaper intFetch(int key) {
	return myTable.intFetch(key);
/*
udanax-top.st:50224:COWIntegerTable methodsFor: 'accessing'!
{Heaper} intFetch: key {IntegerVar} 
	^myTable intFetch: key!
*/
}
public boolean intWipe(int anIdx) {
	aboutToWrite();
	return intWipe(anIdx);
/*
udanax-top.st:50228:COWIntegerTable methodsFor: 'accessing'!
{BooleanVar} intWipe: anIdx {IntegerVar}
	self aboutToWrite.
	^self intWipe: anIdx!
*/
}
public int lowestIndex() {
	return myTable.lowestIndex();
/*
udanax-top.st:50232:COWIntegerTable methodsFor: 'accessing'!
{IntegerVar} lowestIndex
	^ myTable lowestIndex!
*/
}
public ScruTable subTable(XnRegion reg) {
	return myTable.subTable(reg);
/*
udanax-top.st:50236:COWIntegerTable methodsFor: 'accessing'!
{ScruTable} subTable: reg {XnRegion} 
	^ myTable subTable: reg!
*/
}
public ScruTable copy() {
	return myTable.copy();
/*
udanax-top.st:50242:COWIntegerTable methodsFor: 'creation'!
{ScruTable} copy
	^ myTable copy!
*/
}
public COWIntegerTable(OberIntegerTable table) {
	super();
	myPrev = table;
	setNextCOW(table.getNextCOW());
	table.setNextCOW(this);
	myTable = table;
/*
udanax-top.st:50245:COWIntegerTable methodsFor: 'creation'!
create: table {OberIntegerTable}
	super create.
	myPrev _ table.
	self setNextCOW: table getNextCOW.
	table setNextCOW: self.
	myTable _ table!
*/
}
/**
 * only recover these during GC.  otherwise crashes occur
 */
public void destroy() {
/*
udanax-top.st:50252:COWIntegerTable methodsFor: 'creation'!
{void} destroy
	"only recover these during GC.  otherwise crashes occur"!
*/
}
public ScruTable emptySize(int size) {
	return myTable.emptySize(size);
/*
udanax-top.st:50255:COWIntegerTable methodsFor: 'creation'!
{ScruTable} emptySize: size {IntegerVar}
	^ myTable emptySize: size!
*/
}
public ScruTable offsetSubTableBetween(int startIndex, int stopIndex, int firstIndex) {
	return myTable.offsetSubTableBetween(startIndex, stopIndex, firstIndex);
/*
udanax-top.st:50259:COWIntegerTable methodsFor: 'creation'!
{ScruTable} offsetSubTableBetween: startIndex {IntegerVar} 
	with: stopIndex {IntegerVar} 
	with: firstIndex {IntegerVar} 
	^myTable offsetSubTableBetween: startIndex with: stopIndex with: firstIndex!
*/
}
public ScruTable subTableBetween(int startIndex, int stopIndex) {
	return myTable.subTableBetween(startIndex, stopIndex);
/*
udanax-top.st:50265:COWIntegerTable methodsFor: 'creation'!
{ScruTable} subTableBetween: startIndex {IntegerVar} with: stopIndex {IntegerVar}
	^myTable subTableBetween: startIndex with: stopIndex!
*/
}
public XnRegion runAtInt(int index) {
	return myTable.runAtInt(index);
/*
udanax-top.st:50271:COWIntegerTable methodsFor: 'runs'!
{XnRegion} runAtInt: index {IntegerVar}
	^myTable runAtInt: index!
*/
}
public boolean includesIntKey(int aKey) {
	return myTable.includesIntKey(aKey);
/*
udanax-top.st:50276:COWIntegerTable methodsFor: 'testing'!
{BooleanVar} includesIntKey: aKey {IntegerVar}
	^myTable includesIntKey: aKey!
*/
}
public boolean isEmpty() {
	return myTable.isEmpty();
/*
udanax-top.st:50280:COWIntegerTable methodsFor: 'testing'!
{BooleanVar} isEmpty
	^ myTable isEmpty!
*/
}
public TableStepper stepper(OrderSpec order) {
	return myTable.stepper(order);
/*
udanax-top.st:50285:COWIntegerTable methodsFor: 'enumerating'!
{TableStepper} stepper: order {OrderSpec default: NULL}
	^ myTable stepper: order!
*/
}
public OberIntegerTable getPrev() {
	if ( ! (myPrev != null)) {
		throw new AboraAssertionException("NULL in getPrev");
	}
	return myPrev;
/*
udanax-top.st:50290:COWIntegerTable methodsFor: 'COW stuff'!
{OberIntegerTable wimpy} getPrev
	(myPrev ~~ NULL) assert: 'NULL in getPrev'.
	^ myPrev!
*/
}
public void setMuTable(OberIntegerTable table) {
	myTable = table;
/*
udanax-top.st:50294:COWIntegerTable methodsFor: 'COW stuff'!
{void} setMuTable: table {OberIntegerTable}
	myTable _ table!
*/
}
public void setPrev(OberIntegerTable set) {
	myPrev = set;
/*
udanax-top.st:50297:COWIntegerTable methodsFor: 'COW stuff'!
{void} setPrev: set {OberIntegerTable}
	myPrev _ set!
*/
}
/**
 * return the elements array for rapid processing
 */
public PtrArray elementsArray() {
	return myTable.elementsArray();
/*
udanax-top.st:50302:COWIntegerTable methodsFor: 'private:'!
{PtrArray} elementsArray
	"return the elements array for rapid processing"
	^ myTable elementsArray!
*/
}
/**
 * return the size of the elements array for rapid processing
 */
public int endOffset() {
	return myTable.endOffset();
/*
udanax-top.st:50306:COWIntegerTable methodsFor: 'private:'!
{UInt32} endOffset
	"return the size of the elements array for rapid processing"
	^ myTable endOffset!
*/
}
public int startIndex() {
	return myTable.startIndex();
/*
udanax-top.st:50310:COWIntegerTable methodsFor: 'private:'!
{IntegerVar} startIndex
	^ myTable startIndex!
*/
}
/**
 * return the size of the elements array for rapid processing
 */
public int startOffset() {
	return myTable.startOffset();
/*
udanax-top.st:50313:COWIntegerTable methodsFor: 'private:'!
{UInt32} startOffset
	"return the size of the elements array for rapid processing"
	^ myTable startOffset!
*/
}
public void aboutToWrite() {
	OberIntegerTable prev;
	COWIntegerTable next;
	/* become a copy of myMuTable and remove myself from all CopyOnWrite dependendents lists.
	The caller's self/this pointer will point to the becomed object after this returns.
	This makes all my caller's look like they are recursive, but they aren't. */
	prev = getPrev();
	next = getNextCOW();
	if (next != null) {
		next.setPrev(prev);
	}
	prev.setNextCOW(next);
	myTable.becomeCloneOnWrite(this);
/*
udanax-top.st:50319:COWIntegerTable methodsFor: 'protected: COW stuff'!
{void} aboutToWrite
	| prev {OberIntegerTable wimpy} next {COWIntegerTable wimpy} |
	"become a copy of myMuTable and remove myself from all CopyOnWrite dependendents lists.
	The caller's self/this pointer will point to the becomed object after this returns.
	This makes all my caller's look like they are recursive, but they aren't."
	prev _ self getPrev.
	next _ self getNextCOW.
	next ~~ NULL ifTrue: [ next setPrev: prev ].
	prev setNextCOW: next.
	myTable becomeCloneOnWrite: self.!
*/
}
public void becomeCloneOnWrite(Heaper where) {
	shouldNotImplement();
/*
udanax-top.st:50330:COWIntegerTable methodsFor: 'protected: COW stuff'!
{void} becomeCloneOnWrite: where {Heaper unused}
	self shouldNotImplement.!
*/
}
public Heaper store(Position key, Heaper value) {
	return intStore(((IntegerPos) key).asIntegerVar(), value);
/*
udanax-top.st:50335:COWIntegerTable methodsFor: 'overload junk'!
{Heaper} at: key {Position} store: value {Heaper} 
	^ self atInt: (key cast: IntegerPos) asIntegerVar store: value!
*/
}
public Heaper fetch(Position key) {
	return intFetch(((IntegerPos) key).asIntegerVar());
/*
udanax-top.st:50339:COWIntegerTable methodsFor: 'overload junk'!
{Heaper} fetch: key {Position} 
	^ self intFetch: (key cast: IntegerPos) asIntegerVar!
*/
}
public boolean includesKey(Position aKey) {
	return includesIntKey((((IntegerPos) aKey).asIntegerVar()));
/*
udanax-top.st:50343:COWIntegerTable methodsFor: 'overload junk'!
{BooleanVar} includesKey: aKey {Position}
	^ self includesIntKey: ((aKey cast: IntegerPos) asIntegerVar)!
*/
}
public XnRegion runAt(Position key) {
	return runAtInt((((IntegerPos) key).asIntegerVar()));
/*
udanax-top.st:50346:COWIntegerTable methodsFor: 'overload junk'!
{XnRegion} runAt: key {Position} 
	^ self runAtInt: ((key cast: IntegerPos) asIntegerVar)!
*/
}
public boolean wipe(Position key) {
	return intWipe(((IntegerPos) key).asIntegerVar());
/*
udanax-top.st:50350:COWIntegerTable methodsFor: 'overload junk'!
{BooleanVar} wipe: key {Position}
	^ self intWipe: (key cast: IntegerPos) asIntegerVar!
*/
}
public COWIntegerTable(Rcvr receiver) {
	super(receiver);
	myTable = (OberIntegerTable) receiver.receiveHeaper();
/*
udanax-top.st:50355:COWIntegerTable methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myTable _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myTable);
/*
udanax-top.st:50359:COWIntegerTable methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myTable.!
*/
}
public COWIntegerTable() {
/*

Generated during transformation
*/
}
}
