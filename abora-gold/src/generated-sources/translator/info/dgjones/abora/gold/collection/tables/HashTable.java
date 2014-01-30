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
import info.dgjones.abora.gold.collection.tables.ActualHashTable;
import info.dgjones.abora.gold.collection.tables.HashTable;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class HashTable extends MuTable {

/*
udanax-top.st:48454:
MuTable subclass: #HashTable
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Tables'!
*/
/*
udanax-top.st:48458:
(HashTable getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:48532:
HashTable class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:48535:
(HashTable getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HashTable.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Associate value with key, whether or not
 * there is a previous association.
 */
public Heaper store(Position key, Heaper value) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48463:HashTable methodsFor: 'accessing'!
{Heaper} at: key {Position} store: value {Heaper} 
	"Associate value with key, whether or not
	 there is a previous association."
	self subclassResponsibility!
*/
}
public CoordinateSpace coordinateSpace() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48469:HashTable methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	
	self subclassResponsibility!
*/
}
public int count() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48473:HashTable methodsFor: 'accessing'!
{IntegerVar} count
	self subclassResponsibility!
*/
}
public XnRegion domain() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48477:HashTable methodsFor: 'accessing'!
{XnRegion} domain
	self subclassResponsibility.!
*/
}
public Heaper fetch(Position key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48481:HashTable methodsFor: 'accessing'!
{Heaper} fetch: key {Position} 
	self subclassResponsibility!
*/
}
public ScruTable subTable(XnRegion reg) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48485:HashTable methodsFor: 'accessing'!
{ScruTable} subTable: reg {XnRegion} 
	self subclassResponsibility!
*/
}
/**
 * Remove a key->value association from the table.
 * Do not blast (or do anything else) if the key is not in my current domain.
 */
public boolean wipe(Position anIdx) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48489:HashTable methodsFor: 'accessing'!
{BooleanVar} wipe: anIdx {Position}
	"Remove a key->value association from the table.
	 Do not blast (or do anything else) if the key is not in my current domain."
	self subclassResponsibility!
*/
}
public boolean includesKey(Position aKey) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48497:HashTable methodsFor: 'testing'!
{BooleanVar} includesKey: aKey {Position}
	self subclassResponsibility!
*/
}
public boolean isEmpty() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48500:HashTable methodsFor: 'testing'!
{BooleanVar} isEmpty
	self subclassResponsibility.!
*/
}
public TableStepper stepper(OrderSpec order) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48505:HashTable methodsFor: 'enumerating'!
{TableStepper} stepper: order {OrderSpec default: NULL}
	self subclassResponsibility!
*/
}
public Heaper theOne() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48508:HashTable methodsFor: 'enumerating'!
{Heaper} theOne
	self subclassResponsibility!
*/
}
public XnRegion runAt(Position key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48513:HashTable methodsFor: 'runs'!
{XnRegion} runAt: key {Position} 
	self subclassResponsibility!
*/
}
public ScruTable copy() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48519:HashTable methodsFor: 'creation'!
{ScruTable} copy
	self subclassResponsibility!
*/
}
public ScruTable emptySize(int size) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:48522:HashTable methodsFor: 'creation'!
{ScruTable} emptySize: size {IntegerVar}
	self subclassResponsibility!
*/
}
public HashTable() {
	super();
/*
udanax-top.st:48528:HashTable methodsFor: 'protected: create'!
create
	super create!
*/
}
public static HashTable makeCoordinateSpace(CoordinateSpace cs) {
	return (HashTable) ActualHashTable.make(cs);
/*
udanax-top.st:48540:HashTable class methodsFor: 'pseudo constructors'!
{HashTable INLINE} make.CoordinateSpace: cs {CoordinateSpace}
	^ActualHashTable make: cs!
*/
}
public static HashTable makeCoordinateSpace(CoordinateSpace cs, int size) {
	return ActualHashTable.make(cs, (size | 1));
/*
udanax-top.st:48543:HashTable class methodsFor: 'pseudo constructors'!
make.CoordinateSpace: cs {CoordinateSpace} with: size {IntegerVar}
	^ActualHashTable make: cs with: (size DOTasLong bitOr: 1)!
*/
}
public static void initTimeNonInherited() {
	/* for the empty set domain */
/*
udanax-top.st:48548:HashTable class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	self REQUIRES: ImmuSet. "for the empty set domain"
	self REQUIRES: LPPrimeSizeProvider!
*/
}
public HashTable(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
