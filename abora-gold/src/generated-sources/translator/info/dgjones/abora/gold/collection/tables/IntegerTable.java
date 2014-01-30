/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.tables;

import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.ActualIntegerTable;
import info.dgjones.abora.gold.collection.tables.IntegerTable;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.MuWordArray;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * The IntegerTable class is used for tables that have arbitrary XuInteger keys in their
 * domain.  Since ScruTable & MuTable already provide all the unboxed versions of the table
 * protocol, there is little need for this class to be a type.  However, this class does
 * provide a bit of extra protocol convenience: highestIndex & lowestIndex. Unless these are
 * retired, we cannot retire this class from type status.
 * Note that there may be tables with XuInteger keys (i.e., IntegerSpace domains) which are
 * not kinds of IntegerTables.  In particular it is perfectly sensible to create a HashTable
 * with XuInteger keys when the domain region is likely to be sparse.
 */
public class IntegerTable extends MuTable {

/*
udanax-top.st:48897:
MuTable subclass: #IntegerTable
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Tables'!
*/
/*
udanax-top.st:48901:
IntegerTable comment:
'The IntegerTable class is used for tables that have arbitrary XuInteger keys in their domain.  Since ScruTable & MuTable already provide all the unboxed versions of the table protocol, there is little need for this class to be a type.  However, this class does provide a bit of extra protocol convenience: highestIndex & lowestIndex. Unless these are retired, we cannot retire this class from type status.
	
	Note that there may be tables with XuInteger keys (i.e., IntegerSpace domains) which are not kinds of IntegerTables.  In particular it is perfectly sensible to create a HashTable with XuInteger keys when the domain region is likely to be sparse.'!
*/
/*
udanax-top.st:48905:
(IntegerTable getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:49045:
IntegerTable class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:49048:
(IntegerTable getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IntegerTable.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public void intIntroduce(int key, Heaper value) {
	Heaper old;
	if ((old = intStore(key, value)) != null) {
		intStore(key, old);
		/* restore prior condition */
		throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
	}
/*
udanax-top.st:48910:IntegerTable methodsFor: 'accessing'!
{void} atInt: key {IntegerVar} introduce: value {Heaper}
	| old {Heaper} |
	(old _ self atInt: key store: value) ~~ NULL
		ifTrue: [self atInt: key store: old. "restore prior condition"
			Heaper BLAST: #AlreadyInTable]!
*/
}
public void intReplace(int key, Heaper value) {
	if ((intStore(key, value)) == null) {
		intWipe(key);
		/* restore prior condition */
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
/*
udanax-top.st:48916:IntegerTable methodsFor: 'accessing'!
{void} atInt: key {IntegerVar} replace: value {Heaper} 
	(self atInt: key store: value) == NULL
		ifTrue: [self intWipe: key. "restore prior condition"
			Heaper BLAST: #NotInTable]!
*/
}
public Heaper intStore(int key, Heaper value) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48921:IntegerTable methodsFor: 'accessing'!
{Heaper} atInt: key {IntegerVar} store: value {Heaper} 
	self subclassResponsibility!
*/
}
public CoordinateSpace coordinateSpace() {
	return IntegerSpace.make();
/*
udanax-top.st:48925:IntegerTable methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^IntegerSpace make!
*/
}
public int count() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48929:IntegerTable methodsFor: 'accessing'!
{IntegerVar} count
	self subclassResponsibility!
*/
}
public XnRegion domain() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48933:IntegerTable methodsFor: 'accessing'!
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
udanax-top.st:48937:IntegerTable methodsFor: 'accessing'!
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
udanax-top.st:48946:IntegerTable methodsFor: 'accessing'!
{Heaper} intFetch: key {IntegerVar} 
	self subclassResponsibility!
*/
}
public void intRemove(int anIdx) {
	if ( ! (intWipe(anIdx))) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
/*
udanax-top.st:48950:IntegerTable methodsFor: 'accessing'!
{void} intRemove: anIdx {IntegerVar}
	
	(self intWipe: anIdx) ifFalse: [Heaper BLAST: #NotInTable]!
*/
}
public boolean intWipe(int anIdx) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48954:IntegerTable methodsFor: 'accessing'!
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
udanax-top.st:48957:IntegerTable methodsFor: 'accessing'!
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
udanax-top.st:48965:IntegerTable methodsFor: 'accessing'!
{ScruTable} subTable: reg {XnRegion} 
	self subclassResponsibility!
*/
}
public void introduce(Position key, Heaper value) {
	intIntroduce(((IntegerPos) key).asIntegerVar(), value);
/*
udanax-top.st:48971:IntegerTable methodsFor: 'accessing overloads'!
{void} at: key {Position} introduce: value {Heaper} 
	self atInt: (key cast: IntegerPos) asIntegerVar introduce: value!
*/
}
public void replace(Position key, Heaper value) {
	intReplace(((IntegerPos) key).asIntegerVar(), value);
/*
udanax-top.st:48975:IntegerTable methodsFor: 'accessing overloads'!
{void} at: key {Position} replace: value {Heaper} 
	self atInt: (key cast: IntegerPos) asIntegerVar replace: value!
*/
}
public Heaper store(Position key, Heaper value) {
	return intStore(((IntegerPos) key).asIntegerVar(), value);
/*
udanax-top.st:48979:IntegerTable methodsFor: 'accessing overloads'!
{Heaper} at: key {Position} store: value {Heaper} 
	^ self atInt: (key cast: IntegerPos) asIntegerVar store: value!
*/
}
public Heaper fetch(Position key) {
	return intFetch(((IntegerPos) key).asIntegerVar());
/*
udanax-top.st:48983:IntegerTable methodsFor: 'accessing overloads'!
{Heaper} fetch: key {Position} 
	^ self intFetch: (key cast: IntegerPos) asIntegerVar!
*/
}
public boolean includesKey(Position aKey) {
	return includesIntKey(((IntegerPos) aKey).asIntegerVar());
/*
udanax-top.st:48987:IntegerTable methodsFor: 'accessing overloads'!
{BooleanVar} includesKey: aKey {Position}
	^ self includesIntKey: (aKey cast: IntegerPos) asIntegerVar!
*/
}
public void remove(Position aPos) {
	intRemove(((IntegerPos) aPos).asIntegerVar());
/*
udanax-top.st:48990:IntegerTable methodsFor: 'accessing overloads'!
{void} remove: aPos {Position}
	
	self intRemove: (aPos cast: IntegerPos) asIntegerVar!
*/
}
public XnRegion runAt(Position key) {
	return runAtInt(((IntegerPos) key).asIntegerVar());
/*
udanax-top.st:48994:IntegerTable methodsFor: 'accessing overloads'!
{XnRegion} runAt: key {Position} 
	^ self runAtInt: (key cast: IntegerPos) asIntegerVar!
*/
}
public boolean wipe(Position anIdx) {
	return intWipe((((IntegerPos) anIdx).asIntegerVar()));
/*
udanax-top.st:48997:IntegerTable methodsFor: 'accessing overloads'!
{BooleanVar} wipe: anIdx {Position}
	^ self intWipe: ((anIdx cast: IntegerPos) asIntegerVar)!
*/
}
public boolean includesIntKey(int aKey) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49003:IntegerTable methodsFor: 'testing'!
{BooleanVar} includesIntKey: aKey {IntegerVar}
	self subclassResponsibility!
*/
}
public boolean isEmpty() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49006:IntegerTable methodsFor: 'testing'!
{BooleanVar} isEmpty
	self subclassResponsibility.!
*/
}
public TableStepper stepper(OrderSpec order) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49011:IntegerTable methodsFor: 'enumerating'!
{TableStepper} stepper: order {OrderSpec default: NULL}
	self subclassResponsibility!
*/
}
public XnRegion runAtInt(int key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49016:IntegerTable methodsFor: 'runs'!
{XnRegion} runAtInt: key {IntegerVar}
	self subclassResponsibility!
*/
}
public ScruTable copy() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49021:IntegerTable methodsFor: 'creation'!
{ScruTable} copy
	self subclassResponsibility!
*/
}
/**
 * Create a new table with an unspecified number of initial domain positions.
 */
public IntegerTable() {
	super();
/*
udanax-top.st:49024:IntegerTable methodsFor: 'creation'!
create
	"Create a new table with an unspecified number of initial domain positions."
	super create!
*/
}
public ScruTable emptySize(int size) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:49028:IntegerTable methodsFor: 'creation'!
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
udanax-top.st:49032:IntegerTable methodsFor: 'creation'!
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
udanax-top.st:49040:IntegerTable methodsFor: 'creation'!
{ScruTable} subTableBetween: startIndex {IntegerVar} with: stopIndex {IntegerVar}
	"Hack for C++ overloading problem"
	self subclassResponsibility!
*/
}
/*
udanax-top.st:49053:IntegerTable class methodsFor: 'smalltalk: pseudoConstructors'!
{IntegerTable} make: something {Heaper}
	(something isKindOf: String) 
		ifTrue: [^ self make.charVector: something].
	^ self make.IntegerVar: (something cast: Integer)!
*/
public static MuTable make(int from, int to) {
	return makeIntegerVar(from, to);
/*
udanax-top.st:49058:IntegerTable class methodsFor: 'smalltalk: pseudoConstructors'!
{MuTable} make: from {IntegerVar} with: to {IntegerVar}
	^self make.IntegerVar: from with: to!
*/
}
/**
 * A new empty IntegerTable
 */
public static IntegerTable make() {
	return new ActualIntegerTable();
/*
udanax-top.st:49064:IntegerTable class methodsFor: 'pseudoConstructors'!
{IntegerTable} make
	"A new empty IntegerTable"
	
	^ ActualIntegerTable create.!
*/
}
/**
 * A new empty IntegerTable. 'someSize' is a hint about how big the table is likely
 * to need to be ('highestIndex - lowestIndex + 1', not 'count').
 */
public static IntegerTable makeIntegerVar(int someSize) {
	return new ActualIntegerTable(someSize);
/*
udanax-top.st:49069:IntegerTable class methodsFor: 'pseudoConstructors'!
{IntegerTable} make.IntegerVar: someSize {IntegerVar} 
	"A new empty IntegerTable. 'someSize' is a hint about how big the table is likely 
	to need to be ('highestIndex - lowestIndex + 1', not 'count')."
	^ActualIntegerTable create.IntegerVar: someSize!
*/
}
/**
 * Hint that the domain's lowerBound (inclusive) will eventually be 'fromIdx', and
 * the domain's upperBound (exclusive) will eventually be 'toIdx'.
 */
public static IntegerTable makeIntegerVar(int fromIdx, int toIdx) {
	return new ActualIntegerTable(fromIdx, toIdx);
/*
udanax-top.st:49075:IntegerTable class methodsFor: 'pseudoConstructors'!
{IntegerTable} make.IntegerVar: fromIdx {IntegerVar} with: toIdx {IntegerVar} 
	"Hint that the domain's lowerBound (inclusive) will eventually be 'fromIdx', and 
	the domain's upperBound (exclusive) will eventually be 'toIdx'."
	^ActualIntegerTable create: fromIdx with: toIdx!
*/
}
/**
 * Hint that the domain of the new table will eventually be (or at least resemble)
 * 'reg'.
 */
public static IntegerTable makeRegion(IntegerRegion reg) {
	return new ActualIntegerTable(reg.start(), reg.stop());
/*
udanax-top.st:49081:IntegerTable class methodsFor: 'pseudoConstructors'!
{IntegerTable} make.Region: reg {IntegerRegion} 
	"Hint that the domain of the new table will eventually be (or at least resemble) 
	'reg'."
	^ActualIntegerTable create: reg start with: reg stop!
*/
}
/**
 * A new IntegerTable initialized from 'table' in a wierd and screwy way
 * @deprecated
 */
public static IntegerTable makeScruTable(ScruTable table) {
	throw new PasseException();
/*
udanax-top.st:49089:IntegerTable class methodsFor: 'smalltalk: passe'!
{IntegerTable} make.ScruTable: table {ScruTable} 
	"A new IntegerTable initialized from 'table' in a wierd and screwy way"
	
	"| newTable {IntegerTable} stomp {TableStepper} |
	newTable _ IntegerTable make.IntegerVar: (table count).
	stomp _ table stepper.
	[stomp hasValue] whileTrue:
		[newTable at.IntegerVar: stomp index introduce: (table get.IntegerVar: stomp index).
		stomp step].
	^newTable"
	
	self passe!
*/
}
/**
 * Make a copy of 'wv' as an IntegerTable. The IntegerTable starts out with the
 * same state as 'wv', but unlike 'wv' is not obligated to maintain MuArray
 * constraints.
 * @deprecated
 */
public static IntegerTable makeWordArray(MuWordArray wv) {
	throw new PasseException();
/*
udanax-top.st:49102:IntegerTable class methodsFor: 'smalltalk: passe'!
{IntegerTable} make.WordArray: wv {MuWordArray}
	"Make a copy of 'wv' as an IntegerTable. The IntegerTable starts out with the 
	same state as 'wv', but unlike 'wv' is not obligated to maintain MuArray 
	constraints."
	self passe.!
*/
}
public IntegerTable(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public static IntegerTable make(int i) {
	return makeIntegerVar(i);
/*

Generated during transformation: AddMethod
*/
}
}
