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
import info.dgjones.abora.gold.collection.tables.ImmuTable;
import info.dgjones.abora.gold.collection.tables.ImmuTableOnMu;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class ImmuTableOnMu extends ImmuTable {

	protected MuTable myMuTable;
/*
udanax-top.st:47406:
ImmuTable subclass: #ImmuTableOnMu
	instanceVariableNames: 'myMuTable {MuTable}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Tables'!
*/
/*
udanax-top.st:47410:
(ImmuTableOnMu getOrMakeCxxClassDescription)
	friends:
'friend SPTR(ImmuTable) immuTable (MuTable*);
friend SPTR(ImmuTable) immuTable (CoordinateSpace * cs);
friend SPTR(ImmuTable) MuTable::asImmuTable ();';
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ImmuTableOnMu.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * use the given Mu to store current value
 */
public ImmuTableOnMu(MuTable aMuTable) {
	super();
	/* it should be a copy for my exclusive use */
	/* this should only be called from the pseudo constructor or from class methods */
	myMuTable = aMuTable;
/*
udanax-top.st:47419:ImmuTableOnMu methodsFor: 'private: instance creation'!
create: aMuTable {MuTable}
	"use the given Mu to store current value"
	"it should be a copy for my exclusive use"
	"this should only be called from the pseudo constructor or from class methods"
	super create.
	myMuTable _ aMuTable!
*/
}
public CoordinateSpace coordinateSpace() {
	return myMuTable.coordinateSpace();
/*
udanax-top.st:47429:ImmuTableOnMu methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	
	^myMuTable coordinateSpace!
*/
}
public int count() {
	return myMuTable.count();
/*
udanax-top.st:47433:ImmuTableOnMu methodsFor: 'accessing'!
{IntegerVar} count
	^myMuTable count!
*/
}
public XnRegion domain() {
	return myMuTable.domain();
/*
udanax-top.st:47437:ImmuTableOnMu methodsFor: 'accessing'!
{XnRegion} domain
	^myMuTable domain!
*/
}
public Heaper fetch(Position key) {
	return myMuTable.fetch(key);
/*
udanax-top.st:47441:ImmuTableOnMu methodsFor: 'accessing'!
{Heaper} fetch: key {Position} 
	^myMuTable fetch: key!
*/
}
public Heaper intFetch(int key) {
	return myMuTable.intFetch(key);
/*
udanax-top.st:47445:ImmuTableOnMu methodsFor: 'accessing'!
{Heaper} intFetch: key {IntegerVar}
	^myMuTable intFetch: key!
*/
}
public ScruTable subTable(XnRegion region) {
	return new ImmuTableOnMu(((MuTable) (myMuTable.subTable(region))));
/*
udanax-top.st:47448:ImmuTableOnMu methodsFor: 'accessing'!
{ScruTable} subTable: region {XnRegion} 
	^ImmuTableOnMu create: ((myMuTable subTable: region) cast: MuTable)!
*/
}
public ImmuTable combineWith(ImmuTable other) {
	MuTable newMuTable;
	TableStepper others;
	newMuTable = (MuTable) myMuTable.copy();
	others = other.stepper();
	while (others.hasValue()) {
		newMuTable.store(others.position(), others.fetch());
		others.step();
	}
	others.destroy();
	return new ImmuTableOnMu(newMuTable);
/*
udanax-top.st:47454:ImmuTableOnMu methodsFor: 'SEF manipulation'!
{ImmuTable} combineWith: other {ImmuTable}
	| newMuTable {MuTable} others {TableStepper} |
	newMuTable _ myMuTable copy cast: MuTable.
	others _ other stepper.
	[others hasValue] whileTrue:
		[newMuTable at: others position store: others fetch.
		others step].
	others destroy.
	^ImmuTableOnMu create: newMuTable!
*/
}
public ImmuTable without(Position index) {
	MuTable newMuTable;
	newMuTable = (MuTable) myMuTable.copy();
	newMuTable.wipe(index);
	return new ImmuTableOnMu(newMuTable);
/*
udanax-top.st:47465:ImmuTableOnMu methodsFor: 'SEF manipulation'!
{ImmuTable} without: index {Position}
	| newMuTable {MuTable} |
	newMuTable _ myMuTable copy cast: MuTable.
	newMuTable wipe: index.
	^ ImmuTableOnMu create: newMuTable!
*/
}
public MuTable asMuTable() {
	return (MuTable) myMuTable.copy();
/*
udanax-top.st:47474:ImmuTableOnMu methodsFor: 'conversion'!
{MuTable} asMuTable
	^myMuTable copy cast: MuTable!
*/
}
public boolean includesIntKey(int aKey) {
	return myMuTable.includesIntKey(aKey);
/*
udanax-top.st:47480:ImmuTableOnMu methodsFor: 'testing'!
{BooleanVar} includesIntKey: aKey {IntegerVar}
	^ myMuTable includesIntKey: aKey!
*/
}
public boolean includesKey(Position aKey) {
	return myMuTable.includesKey(aKey);
/*
udanax-top.st:47483:ImmuTableOnMu methodsFor: 'testing'!
{BooleanVar} includesKey: aKey {Position}
	^ myMuTable includesKey: aKey!
*/
}
public boolean isEmpty() {
	return myMuTable.isEmpty();
/*
udanax-top.st:47486:ImmuTableOnMu methodsFor: 'testing'!
{BooleanVar} isEmpty
	^myMuTable isEmpty!
*/
}
public TableStepper stepper(OrderSpec order) {
	return myMuTable.copy().stepper(order
	/* making the copy prevents anyone from getting access to the array through TableStepper array */
	);
/*
udanax-top.st:47491:ImmuTableOnMu methodsFor: 'enumerating'!
{TableStepper} stepper: order {OrderSpec default: NULL}
	^myMuTable copy stepper: order	"making the copy prevents anyone from getting access to the array through TableStepper array"!
*/
}
public Heaper theOne() {
	return myMuTable.theOne();
/*
udanax-top.st:47494:ImmuTableOnMu methodsFor: 'enumerating'!
{Heaper} theOne
	^ myMuTable theOne!
*/
}
public MuTable getMuTable() {
	return myMuTable;
/*
udanax-top.st:47499:ImmuTableOnMu methodsFor: 'private: accessing'!
{MuTable} getMuTable
	^myMuTable!
*/
}
public ScruTable emptySize(int size) {
	return new ImmuTableOnMu(((MuTable) (myMuTable.emptySize(size))));
/*
udanax-top.st:47504:ImmuTableOnMu methodsFor: 'creation'!
{ScruTable} emptySize: size {IntegerVar}
	^ImmuTableOnMu create: ((myMuTable emptySize: size) cast: MuTable)!
*/
}
public XnRegion runAt(Position key) {
	return myMuTable.runAt(key);
/*
udanax-top.st:47510:ImmuTableOnMu methodsFor: 'runs'!
{XnRegion} runAt: key {Position} 
	^myMuTable runAt: key!
*/
}
public XnRegion runAtInt(int key) {
	return myMuTable.runAtInt(key);
/*
udanax-top.st:47514:ImmuTableOnMu methodsFor: 'runs'!
{XnRegion} runAtInt: key {IntegerVar}
	^myMuTable runAtInt: key!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myMuTable);
	oo.print(")");
/*
udanax-top.st:47519:ImmuTableOnMu methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myMuTable << ')'!
*/
}
public ImmuTableOnMu(Rcvr receiver) {
	super(receiver);
	myMuTable = (MuTable) receiver.receiveHeaper();
/*
udanax-top.st:47525:ImmuTableOnMu methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myMuTable _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myMuTable);
/*
udanax-top.st:47529:ImmuTableOnMu methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myMuTable.!
*/
}
public ImmuTableOnMu() {
/*

Generated during transformation
*/
}
}
