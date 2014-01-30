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
import info.dgjones.abora.gold.collection.tables.IntegerTable;
import info.dgjones.abora.gold.collection.tables.OberIntegerTable;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class OberIntegerTable extends IntegerTable {

	protected COWIntegerTable myNextCOW;
/*
udanax-top.st:49582:
IntegerTable subclass: #OberIntegerTable
	instanceVariableNames: 'myNextCOW {COWIntegerTable NOCOPY | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Tables'!
*/
/*
udanax-top.st:49586:
(OberIntegerTable getOrMakeCxxClassDescription)
	friends:
'/- friends for class OberIntegerTable -/
friend class ITAscendingStepper;
friend class ITDescendingStepper;
friend class ITGenericStepper;
friend class COWIntegerTable;';
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OberIntegerTable.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper intStore(int key, Heaper value) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49597:OberIntegerTable methodsFor: 'accessing'!
{Heaper} atInt: key {IntegerVar} store: value {Heaper} 
	self subclassResponsibility!
*/
}
public CoordinateSpace coordinateSpace() {
	return IntegerSpace.make();
/*
udanax-top.st:49601:OberIntegerTable methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^IntegerSpace make!
*/
}
public int count() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49605:OberIntegerTable methodsFor: 'accessing'!
{IntegerVar} count
	self subclassResponsibility!
*/
}
public XnRegion domain() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49609:OberIntegerTable methodsFor: 'accessing'!
{XnRegion} domain
	self subclassResponsibility.!
*/
}
/**
 * Given that the table is non-empty, 'intTab->highestIndex()' is equivalent to
 * 'CAST(IntegerRegion,intTab->domain())->upperBound() -1'. The reason for the
 * '-1' is that the 'upperBound' is an exclusive upper bound (see
 * IntegerRegion::upperBound), whereas 'highestIndex' is the highest index which is
 * in my domain. I need to here specify what 'highestIndex' does if I am empty.
 */
public int highestIndex() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49613:OberIntegerTable methodsFor: 'accessing'!
{IntegerVar} highestIndex
	"Given that the table is non-empty, 'intTab->highestIndex()' is equivalent to 
	'CAST(IntegerRegion,intTab->domain())->upperBound() -1'. The reason for the 
	'-1' is that the 'upperBound' is an exclusive upper bound (see 
	IntegerRegion::upperBound), whereas 'highestIndex' is the highest index which is 
	in my domain. I need to here specify what 'highestIndex' does if I am empty."
	self subclassResponsibility!
*/
}
public Heaper intFetch(int key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49622:OberIntegerTable methodsFor: 'accessing'!
{Heaper} intFetch: key {IntegerVar} 
	self subclassResponsibility!
*/
}
public boolean intWipe(int anIdx) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49626:OberIntegerTable methodsFor: 'accessing'!
{BooleanVar} intWipe: anIdx {IntegerVar}
	self subclassResponsibility!
*/
}
/**
 * Given that the table is non-empty, 'intTab->lowestIndex()' is equivalent to
 * 'CAST(IntegerRegion,intTab->domain())->lowerBound()'. 'lowestIndex' is the
 * lowest index which is in my domain. I need to here specify what 'lowestIndex'
 * does if I am empty.
 */
public int lowestIndex() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49629:OberIntegerTable methodsFor: 'accessing'!
{IntegerVar} lowestIndex
	"Given that the table is non-empty, 'intTab->lowestIndex()' is equivalent to 
	'CAST(IntegerRegion,intTab->domain())->lowerBound()'. 'lowestIndex' is the 
	lowest index which is in my domain. I need to here specify what 'lowestIndex' 
	does if I am empty."
	self subclassResponsibility!
*/
}
public ScruTable subTable(XnRegion reg) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49637:OberIntegerTable methodsFor: 'accessing'!
{ScruTable} subTable: reg {XnRegion} 
	self subclassResponsibility!
*/
}
public ScruTable copy() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49643:OberIntegerTable methodsFor: 'creation'!
{ScruTable} copy
	self subclassResponsibility!
*/
}
public ScruTable emptySize(int size) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49646:OberIntegerTable methodsFor: 'creation'!
{ScruTable} emptySize: size {IntegerVar}
	self subclassResponsibility!
*/
}
/**
 * Return a table which contains the elements from start to stop, starting at
 * firstIndex. Zero-based subclasses will blast if firstIndex is non-zero
 */
public ScruTable offsetSubTableBetween(int startIndex, int stopIndex, int firstIndex) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49650:OberIntegerTable methodsFor: 'creation'!
{ScruTable} offsetSubTableBetween: startIndex {IntegerVar} 
	with: stopIndex {IntegerVar} 
	with: firstIndex {IntegerVar} 
	"Return a table which contains the elements from start to stop, starting at 
	firstIndex. Zero-based subclasses will blast if firstIndex is non-zero"
	self subclassResponsibility!
*/
}
/**
 * Hack for C++ overloading problem
 */
public ScruTable subTableBetween(int startIndex, int stopIndex) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49658:OberIntegerTable methodsFor: 'creation'!
{ScruTable} subTableBetween: startIndex {IntegerVar} with: stopIndex {IntegerVar}
	"Hack for C++ overloading problem"
	self subclassResponsibility!
*/
}
public XnRegion runAtInt(int key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49664:OberIntegerTable methodsFor: 'runs'!
{XnRegion} runAtInt: key {IntegerVar}
	self subclassResponsibility!
*/
}
public boolean includesIntKey(int aKey) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49669:OberIntegerTable methodsFor: 'testing'!
{BooleanVar} includesIntKey: aKey {IntegerVar}
	self subclassResponsibility!
*/
}
public boolean isEmpty() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49672:OberIntegerTable methodsFor: 'testing'!
{BooleanVar} isEmpty
	self subclassResponsibility.!
*/
}
public TableStepper stepper(OrderSpec order) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49677:OberIntegerTable methodsFor: 'enumerating'!
{TableStepper} stepper: order {OrderSpec default: NULL}
	self subclassResponsibility!
*/
}
/**
 * return the elements array for rapid processing
 */
public PtrArray elementsArray() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49682:OberIntegerTable methodsFor: 'private:'!
{PtrArray} elementsArray
	"return the elements array for rapid processing"
	self subclassResponsibility!
*/
}
/**
 * return the size of the elements array for rapid processing
 */
public int endOffset() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49686:OberIntegerTable methodsFor: 'private:'!
{UInt32} endOffset
	"return the size of the elements array for rapid processing"
	self subclassResponsibility!
*/
}
public int startIndex() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49690:OberIntegerTable methodsFor: 'private:'!
{IntegerVar} startIndex
	self subclassResponsibility!
*/
}
/**
 * return the size of the elements array for rapid processing
 */
public int startOffset() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49693:OberIntegerTable methodsFor: 'private:'!
{UInt32} startOffset
	"return the size of the elements array for rapid processing"
	self subclassResponsibility!
*/
}
public OberIntegerTable() {
	super();
	myNextCOW = null;
/*
udanax-top.st:49699:OberIntegerTable methodsFor: 'protected: create'!
create
	super create.
	myNextCOW _ NULL!
*/
}
public void aboutToWrite() {
	COWIntegerTable nextCOW;
	/* make a copy of myself for all outstanding CopyOnWrites on me.
	pass that copy to each of the CopyOnWrite objects.
	One of the COWs gets to become my clone, and the rest point at it. */
	nextCOW = getNextCOW();
	if (nextCOW != null) {
		COWIntegerTable cowP;
		becomeCloneOnWrite(nextCOW);
		cowP = nextCOW.getNextCOW();
		while (cowP != null) {
			cowP.setMuTable(nextCOW);
			cowP = cowP.getNextCOW();
		}
		setNextCOW(null);
	}
/*
udanax-top.st:49705:OberIntegerTable methodsFor: 'vulnerable: COW stuff'!
{void} aboutToWrite
	| nextCOW {COWIntegerTable wimpy} |
	"make a copy of myself for all outstanding CopyOnWrites on me.
	pass that copy to each of the CopyOnWrite objects.
	One of the COWs gets to become my clone, and the rest point at it."
	nextCOW _ self getNextCOW.
	nextCOW ~~ NULL ifTrue:
		[| cowP {COWIntegerTable wimpy} |
		 self becomeCloneOnWrite: nextCOW.
		 cowP _ nextCOW getNextCOW.
		 [cowP ~~ NULL] whileTrue:
		 	[cowP setMuTable: nextCOW.
		 	cowP _ cowP getNextCOW].
		 self setNextCOW: NULL]!
*/
}
public void becomeCloneOnWrite(Heaper where) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49720:OberIntegerTable methodsFor: 'vulnerable: COW stuff'!
{void} becomeCloneOnWrite: where {Heaper unused}
	self subclassResponsibility!
*/
}
public COWIntegerTable getNextCOW() {
	return myNextCOW;
/*
udanax-top.st:49723:OberIntegerTable methodsFor: 'vulnerable: COW stuff'!
{COWIntegerTable wimpy} getNextCOW
	^ myNextCOW!
*/
}
public void setNextCOW(COWIntegerTable table) {
	myNextCOW = table;
/*
udanax-top.st:49726:OberIntegerTable methodsFor: 'vulnerable: COW stuff'!
{void} setNextCOW: table {COWIntegerTable}
	myNextCOW _ table!
*/
}
public Heaper store(Position key, Heaper value) {
	return intStore(((IntegerPos) key).asIntegerVar(), value);
/*
udanax-top.st:49731:OberIntegerTable methodsFor: 'overload junk'!
{Heaper} at: key {Position} store: value {Heaper} 
	^ self atInt: (key cast: IntegerPos) asIntegerVar store: value!
*/
}
public Heaper fetch(Position key) {
	return intFetch(((IntegerPos) key).asIntegerVar());
/*
udanax-top.st:49735:OberIntegerTable methodsFor: 'overload junk'!
{Heaper} fetch: key {Position} 
	^ self intFetch: (key cast: IntegerPos) asIntegerVar!
*/
}
public boolean includesKey(Position aKey) {
	return includesIntKey(((IntegerPos) aKey).asIntegerVar());
/*
udanax-top.st:49739:OberIntegerTable methodsFor: 'overload junk'!
{BooleanVar} includesKey: aKey {Position}
	^ self includesIntKey: (aKey cast: IntegerPos) asIntegerVar!
*/
}
public XnRegion runAt(Position key) {
	return runAtInt(((IntegerPos) key).asIntegerVar());
/*
udanax-top.st:49742:OberIntegerTable methodsFor: 'overload junk'!
{XnRegion} runAt: key {Position} 
	^ self runAtInt: (key cast: IntegerPos) asIntegerVar!
*/
}
public boolean wipe(Position key) {
	return intWipe(((IntegerPos) key).asIntegerVar());
/*
udanax-top.st:49745:OberIntegerTable methodsFor: 'overload junk'!
{BooleanVar} wipe: key {Position}
	^ self intWipe: (key cast: IntegerPos) asIntegerVar!
*/
}
public OberIntegerTable(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
