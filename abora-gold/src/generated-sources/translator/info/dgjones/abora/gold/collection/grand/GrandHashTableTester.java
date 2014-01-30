/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.grand;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.grand.GrandHashTable;
import info.dgjones.abora.gold.collection.grand.GrandHashTableTester;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.Pair;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.spaces.unordered.HeaperAsPosition;
import info.dgjones.abora.gold.spaces.unordered.HeaperSpace;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

public class GrandHashTableTester extends Tester {

/*
udanax-top.st:58350:
Tester subclass: #GrandHashTableTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Grand'!
*/
/*
udanax-top.st:58354:
(GrandHashTableTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandHashTableTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
/**
 * self runTest: #bigTableTestOn:
 */
public void bigTableTestOn(PrintWriter aStream) {
	/* test growing */
	MuTable tab;
	MuSet keys;
	aStream.print("Test growth behavior of GrandHashTable\n"+
"");
	tab = GrandHashTable.make(HeaperSpace.make());
	keys = MuSet.makeIntegerVar(4000);
	for (int i = 1; i <= 4000; i ++ ) {
		Pair thing;
		HeaperAsPosition key;
		thing = Pair.make((IntegerPos.make(4000)), (IntegerPos.make(3 * i)));
		key = HeaperAsPosition.make(thing);
		tab.introduce(key, thing);
		keys.introduce(key);
		/* i > 400 ifTrue:
			[keys stepper forEach: [ : foo {HeaperAsPosition} |
				tab get: foo]] */
		;
	}
	Stepper stomper = keys.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		HeaperAsPosition key1 = (HeaperAsPosition) stomper.fetch();
		if (key1 == null) {
			continue ;
		}
		tab.get(key1);
	}
	stomper.destroy();
	aStream.print("Growth test successful.\n"+
"");
/*
udanax-top.st:58359:GrandHashTableTester methodsFor: 'tests'!
{void} bigTableTestOn: aStream {ostream reference} 
	"self runTest: #bigTableTestOn:"
	"test growing"
	| tab {MuTable of: Pair} keys {MuSet of: HeaperAsPosition} |
	aStream << 'Test growth behavior of GrandHashTable
'.
	tab _ GrandHashTable make: HeaperSpace make.
	keys _ MuSet make: 4000.
	1 to: 4000 do: [ :i {Int32} | | thing {Pair} key {HeaperAsPosition} |
		thing _ Pair make: (IntegerPos make: 4000) with: (IntegerPos make: 3 * i).
		key _ HeaperAsPosition make: thing.
		tab at: key introduce: thing.
		keys introduce: key.
		"i > 400 ifTrue:
			[keys stepper forEach: [ : foo {HeaperAsPosition} |
				tab get: foo]]"].
	
	keys stepper forEach: [ :key {HeaperAsPosition} |
		tab get: key].
	
	aStream << 'Growth test successful.
'.!
*/
}
/**
 * self runTest: #test1On:
 */
public void test1On(PrintWriter oo) {
	/* test creation */
	MuTable tab1;
	MuTable tab2;
	oo.print("Create tables with create, create: and create:with:\n"+
"\n"+
"");
	tab1 = GrandHashTable.make(IntegerSpace.make());
	tab2 = GrandHashTable.make(IntegerSpace.make(), 4);
	/* test printing */
	oo.print("Printing tables:\n"+
"\n"+
"");
	oo.print(tab1);
	oo.print("\n"+
"\n"+
"");
	oo.print(tab2);
	oo.print("\n"+
"\n"+
"");
	/* testing empty */
	oo.print("Test empty table: ");
	if (tab1.isEmpty()) {
		oo.print("Empty");
	}
	else {
		oo.print("Not Empty");
	}
	oo.print("\n"+
"\n"+
"");
	/* inserting */
	tab1.intIntroduce(1, (UInt8Array.string("filly")));
	tab1.intIntroduce(0, (UInt8Array.string("mare")));
	oo.print("Test introduce: ");
	oo.print(tab1);
	oo.print(", table count now: ");
	oo.print(tab1.count());
	oo.print("\n"+
"\n"+
"");
	tab1.intIntroduce(-1, (UInt8Array.string("colt")));
	oo.print("Test introduce: ");
	oo.print(tab1);
	oo.print(", table count now: ");
	oo.print(tab1.count());
	oo.print("\n"+
"\n"+
"");
	tab1.intIntroduce(27, (UInt8Array.string("stallion")));
	oo.print("Test introduce: ");
	oo.print(tab1);
	oo.print(", table count now: ");
	oo.print(tab1.count());
	oo.print("\n"+
"\n"+
"");
	try {
		tab1.intIntroduce(1, (UInt8Array.string("palooka")));
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.ALREADY_IN_TABLE.equals(ex.getMessage())) {
			oo.print("already in table blast caught, table now:\n"+
"\n"+
"");
			oo.print(tab1);
			oo.print("\n"+
"\n"+
"and table count: ");
			oo.print(tab1.count());
			oo.print("\n"+
"\n"+
"");
			return ;
		}
		else {
			throw ex;
		}
	}
	oo.print("Test empty table: ");
	if (tab1.isEmpty()) {
		oo.print("Empty");
	}
	else {
		oo.print("Not Empty");
	}
	oo.print("\n"+
"\n"+
"");
/*
udanax-top.st:58384:GrandHashTableTester methodsFor: 'tests'!
{void} test1On: oo {ostream reference} 
	"self runTest: #test1On:"
	"test creation"
	| tab1 {MuTable} tab2 {MuTable} |
	oo << 'Create tables with create, create: and create:with:
'.
	tab1 _ GrandHashTable make: IntegerSpace make.
	tab2 _ GrandHashTable make: IntegerSpace make with: 4.	"test printing"
	oo << 'Printing tables:
' << tab1 << '
' << tab2 << '
'.	"testing empty"
	oo << 'Test empty table: '.
	tab1 isEmpty
			ifTrue: [oo << 'Empty']
			ifFalse: [oo << 'Not Empty'].
	oo << '
'.	"inserting"
	tab1 atInt: 1 introduce: (UInt8Array string: 'filly').
	tab1 atInt: IntegerVar0 introduce: (UInt8Array string: 'mare').
	oo << 'Test introduce: ' << tab1 << ', table count now: ' << tab1 count << '
'.
	tab1 atInt: -1 introduce: (UInt8Array string: 'colt').
	oo << 'Test introduce: ' << tab1 << ', table count now: ' << tab1 count << '
'.
	tab1 atInt: 27 introduce: (UInt8Array string: 'stallion').
	oo << 'Test introduce: ' << tab1 << ', table count now: ' << tab1 count << '
'.
	MuTable problems.AlreadyInTable handle: [:ex | oo << 'already in table blast caught, table now:
' << tab1 << '
and table count: ' << tab1 count << '
'.
		^VOID]
		do: [tab1 atInt: 1 introduce: (UInt8Array string: 'palooka')].
	oo << 'Test empty table: '.
	tab1 isEmpty
			ifTrue: [oo << 'Empty']
			ifFalse: [oo << 'Not Empty'].
	oo << '
'.!
*/
}
/**
 * self runTest: #test2On:
 */
public void test2On(PrintWriter aStream) {
	/* test creation */
	MuTable tab1;
	aStream.print("Create tables.\n"+
"\n"+
"");
	tab1 = GrandHashTable.make(IntegerSpace.make());
	tab1.intIntroduce(1, (UInt8Array.string("filly")));
	tab1.intIntroduce(0, (UInt8Array.string("mare")));
	tab1.intIntroduce(-1, (UInt8Array.string("colt")));
	tab1.intIntroduce(27, (UInt8Array.string("stallion")));
	aStream.print("Starting table is:\n"+
"\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"\n"+
"");
	tab1.intReplace(1, (UInt8Array.string("mare")));
	aStream.print("after replace:\n"+
"");
	aStream.print(tab1);
	aStream.print(" and table count: ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"");
	aStream.print("Test replace() in unknown territory. \n"+
"");
	try {
		tab1.intReplace(2, (UInt8Array.string("palooka")));
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.NOT_IN_TABLE.equals(ex.getMessage())) {
			aStream.print("NotInTable blast caught, table now:\n"+
"");
			aStream.print(tab1);
			aStream.print("\n"+
"and table count: ");
			aStream.print(tab1.count());
			aStream.print("\n"+
"");
			return ;
		}
		else {
			throw ex;
		}
	}
	aStream.print("Test replace() with NULL. \n"+
"");
	try {
		tab1.intReplace(1, null);
		aStream.print("Replace(NULL) not caught!\n"+
"");
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.NULL_INSERTION.equals(ex.getMessage())) {
			aStream.print("NullInsertion blast caught, table now:\n"+
"");
			aStream.print(tab1);
			aStream.print("\n"+
"and table count: ");
			aStream.print(tab1.count());
			aStream.print("\n"+
"");
			return ;
		}
		else {
			throw ex;
		}
	}
/*
udanax-top.st:58438:GrandHashTableTester methodsFor: 'tests'!
{void} test2On: aStream {ostream reference} 
	"self runTest: #test2On:"
	"test creation"
	| tab1 {MuTable} |
	aStream << 'Create tables.
'.
	tab1 _ GrandHashTable make: IntegerSpace make.
	tab1 atInt: 1 introduce: (UInt8Array string: 'filly').
	tab1 atInt: IntegerVar0 introduce: (UInt8Array string: 'mare').
	tab1 atInt: -1 introduce: (UInt8Array string: 'colt').
	tab1 atInt: 27 introduce: (UInt8Array string: 'stallion').
	aStream << 'Starting table is:
' << tab1 << '
'.
	tab1 atInt: 1 replace: (UInt8Array string: 'mare').
	aStream << 'after replace:
' << tab1 << ' and table count: ' << tab1 count << '
'.
	aStream << 'Test replace() in unknown territory. 
'.
	ScruTable problems.NotInTable handle: [:ex | aStream << 'NotInTable blast caught, table now:
' << tab1 << '
and table count: ' << tab1 count << '
'.
		^VOID]
		do: [tab1 atInt: 2 replace: (UInt8Array string: 'palooka')].
	aStream << 'Test replace() with NULL. 
'.
	MuTable problems.NullInsertion handle: [:ex | aStream << 'NullInsertion blast caught, table now:
' << tab1 << '
and table count: ' << tab1 count << '
'.
		^VOID]
		do: [tab1 atInt: 1 replace: NULL.
			aStream << 'Replace(NULL) not caught!!
']!
*/
}
/**
 * self runTest: #test3On:
 */
public void test3On(PrintWriter aStream) {
	/* test creation */
	MuTable tab1;
	aStream.print("Create tables.\n"+
"\n"+
"");
	tab1 = GrandHashTable.make(IntegerSpace.make());
	tab1.intIntroduce(1, (UInt8Array.string("filly")));
	tab1.intIntroduce(0, (UInt8Array.string("mare")));
	tab1.intIntroduce(-1, (UInt8Array.string("colt")));
	tab1.intIntroduce(27, (UInt8Array.string("stallion")));
	aStream.print("Starting table is:\n"+
"\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"\n"+
"");
	tab1.intStore(1, (UInt8Array.string("mare")));
	aStream.print("after store:\n"+
"\n"+
"");
	aStream.print(tab1);
	aStream.print(" and table count: ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"\n"+
"");
	aStream.print("Test store() in unknown territory. \n"+
"\n"+
"");
	try {
		tab1.intStore(2, (UInt8Array.string("palooka")));
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.NOT_IN_TABLE.equals(ex.getMessage())) {
			aStream.print("NotInTable blast caught, table now:\n"+
"\n"+
"");
			aStream.print(tab1);
			aStream.print("\n"+
"\n"+
"and table count: ");
			aStream.print(tab1.count());
			aStream.print("\n"+
"\n"+
"");
			return ;
		}
		else {
			throw ex;
		}
	}
	aStream.print("after store:\n"+
"\n"+
"");
	aStream.print(tab1);
	aStream.print(" and table count: ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"\n"+
"");
	aStream.print("Test store() with NULL. \n"+
"\n"+
"");
	try {
		tab1.intStore(3, null);
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.NULL_INSERTION.equals(ex.getMessage())) {
			aStream.print("NullInsertion blast caught, table now:\n"+
"\n"+
"");
			aStream.print(tab1);
			aStream.print("\n"+
"\n"+
"and table count: ");
			aStream.print(tab1.count());
			aStream.print("\n"+
"\n"+
"");
			return ;
		}
		else {
			throw ex;
		}
	}
/*
udanax-top.st:58479:GrandHashTableTester methodsFor: 'tests'!
{void} test3On: aStream {ostream reference} 
	"self runTest: #test3On:"
	"test creation"
	| tab1 {MuTable} |
	aStream << 'Create tables.
'.
	tab1 _ GrandHashTable make: IntegerSpace make.
	tab1 atInt: 1 introduce: (UInt8Array string: 'filly').
	tab1 atInt: IntegerVar0 introduce: (UInt8Array string: 'mare').
	tab1 atInt: -1 introduce: (UInt8Array string: 'colt').
	tab1 atInt: 27 introduce: (UInt8Array string: 'stallion').
	aStream << 'Starting table is:
' << tab1 << '
'.
	tab1 atInt: 1 store: (UInt8Array string: 'mare').
	aStream << 'after store:
' << tab1 << ' and table count: ' << tab1 count << '
'.
	aStream << 'Test store() in unknown territory. 
'.
	ScruTable problems.NotInTable handle: [:ex | aStream << 'NotInTable blast caught, table now:
' << tab1 << '
and table count: ' << tab1 count << '
'.
		^VOID]
		do: [tab1 atInt: 2 store: (UInt8Array string: 'palooka')].
	aStream << 'after store:
' << tab1 << ' and table count: ' << tab1 count << '
'.
	aStream << 'Test store() with NULL. 
'.
	MuTable problems.NullInsertion handle: [:ex | aStream << 'NullInsertion blast caught, table now:
' << tab1 << '
and table count: ' << tab1 count << '
'.
		^VOID]
		do: [tab1 atInt: 3 store: NULL]!
*/
}
/**
 * self runTest: #test4On:
 */
public void test4On(PrintWriter aStream) {
	/* test creation */
	MuTable tab1;
	aStream.print("Create tables.\n"+
"\n"+
"");
	tab1 = GrandHashTable.make(IntegerSpace.make());
	tab1.introduce(IntegerPos.make(1), (UInt8Array.string("filly")));
	tab1.introduce(IntegerPos.zero(), (UInt8Array.string("mare")));
	tab1.introduce(IntegerPos.make(-1), (UInt8Array.string("colt")));
	tab1.introduce(IntegerPos.make(27), (UInt8Array.string("stallion")));
	aStream.print("Starting table is:\n"+
"\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"\n"+
"");
	/* testing enclosure */
	aStream.print("Testing domain\n"+
"\n"+
"");
	aStream.print(tab1.domain());
	aStream.print("\n"+
"\n"+
"");
	/* test get */
	aStream.print("Test get(1) ");
	aStream.print((tab1.intGet(1)));
	aStream.print("\n"+
"\n"+
"");
	aStream.print("Test get() in unknown territory. \n"+
"\n"+
"");
	try {
		tab1.intGet(14);
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.NOT_IN_TABLE.equals(ex.getMessage())) {
			aStream.print("NotInTable blast caught, table now:\n"+
"\n"+
"");
			aStream.print(tab1);
			aStream.print("\n"+
"\n"+
"and table count: ");
			aStream.print(tab1.count());
			aStream.print("\n"+
"\n"+
"");
			return ;
		}
		else {
			throw ex;
		}
	}
/*
udanax-top.st:58533:GrandHashTableTester methodsFor: 'tests'!
{void} test4On: aStream {ostream reference} 
	"self runTest: #test4On:"
	"test creation"
	| tab1 {MuTable} |
	aStream << 'Create tables.
'.
	tab1 _ GrandHashTable make: IntegerSpace make.
	tab1 at: 1 integer introduce: (UInt8Array string: 'filly').
	tab1 at: Integer0 introduce: (UInt8Array string: 'mare').
	tab1 at: -1 integer introduce: (UInt8Array string: 'colt').
	tab1 at: 27 integer introduce: (UInt8Array string: 'stallion').
	aStream << 'Starting table is:
' << tab1 << '
with count ' << tab1 count << '
'.	"testing enclosure"
	aStream << 'Testing domain
' << tab1 domain << '
'.	"test get"
	aStream << 'Test get(1) ' << (tab1 intGet: 1) << '
'.
	aStream << 'Test get() in unknown territory. 
'.
	ScruTable problems.NotInTable handle: [:ex | aStream << 'NotInTable blast caught, table now:
' << tab1 << '
and table count: ' << tab1 count << '
'.
		^VOID]
		do: [tab1 intGet: 14]!
*/
}
/**
 * self runTest: #test5On:
 */
public void test5On(PrintWriter aStream) {
	/* test creation */
	MuTable tab1;
	aStream.print("Create tables.\n"+
"\n"+
"");
	tab1 = GrandHashTable.make(IntegerSpace.make());
	tab1.intIntroduce(1, (UInt8Array.string("filly")));
	tab1.intIntroduce(0, (UInt8Array.string("mare")));
	tab1.intIntroduce(-1, (UInt8Array.string("colt")));
	tab1.intIntroduce(27, (UInt8Array.string("stallion")));
	aStream.print("Starting table is:\n"+
"\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"\n"+
"Now, testing remove(1)\n"+
"\n"+
"");
	tab1.intRemove(1);
	aStream.print("Table now:\n"+
"\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"\n"+
"");
	aStream.print("Test remove(1) in unknown territory. \n"+
"\n"+
"");
	try {
		tab1.intRemove(1);
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.NOT_IN_TABLE.equals(ex.getMessage())) {
			aStream.print("NotInTable blast caught, table now:\n"+
"\n"+
"");
			aStream.print(tab1);
			aStream.print("\n"+
"\n"+
"and table count: ");
			aStream.print(tab1.count());
			aStream.print("\n"+
"\n"+
"");
			return ;
		}
		else {
			throw ex;
		}
	}
	aStream.print("Test wipe(0)\n"+
"\n"+
"");
	tab1.wipe(IntegerPos.zero());
	aStream.print("Table now:\n"+
"\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"\n"+
"And wipe(0) again: ");
	tab1.wipe(IntegerPos.zero());
	aStream.print("Table now:\n"+
"\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"\n"+
"");
/*
udanax-top.st:58573:GrandHashTableTester methodsFor: 'tests'!
{void} test5On: aStream {ostream reference} 
	"self runTest: #test5On:"
	"test creation"
	| tab1 {MuTable} |
	aStream << 'Create tables.
'.
	tab1 _ GrandHashTable make: IntegerSpace make.
	tab1 atInt: 1 introduce: (UInt8Array string: 'filly').
	tab1 atInt: IntegerVar0 introduce: (UInt8Array string: 'mare').
	tab1 atInt: -1 introduce: (UInt8Array string: 'colt').
	tab1 atInt: 27 introduce: (UInt8Array string: 'stallion').
	aStream << 'Starting table is:
' << tab1 << '
with count ' << tab1 count << '
Now, testing remove(1)
'.
	tab1 intRemove: 1.
	aStream << 'Table now:
' << tab1 << '
with count ' << tab1 count << '
'.
	aStream << 'Test remove(1) in unknown territory. 
'.
	ScruTable problems.NotInTable handle: [:ex | aStream << 'NotInTable blast caught, table now:
' << tab1 << '
and table count: ' << tab1 count << '
'.
		^VOID]
		do: [tab1 intRemove: 1].
	aStream << 'Test wipe(0)
'.
	tab1 wipe: Integer0.
	aStream << 'Table now:
' << tab1 << '
with count ' << tab1 count << '
And wipe(0) again: '.
	tab1 wipe: Integer0.
	aStream << 'Table now:
' << tab1 << '
with count ' << tab1 count << '
'!
*/
}
/**
 * self runTest: #test7On:
 */
public void test7On(PrintWriter aStream) {
	/* Not currently appropriate to GrandHashTable */
	/* runs {Iterator} */
	/* test creation */
	MuTable tab1;
	aStream.print("Create tables.\n"+
"\n"+
"");
	tab1 = GrandHashTable.make(IntegerSpace.make());
	tab1.intIntroduce(1, (UInt8Array.string("filly")));
	tab1.intIntroduce(0, (UInt8Array.string("mare")));
	tab1.intIntroduce(-1, (UInt8Array.string("colt")));
	tab1.intIntroduce(27, (UInt8Array.string("stallion")));
	aStream.print("Starting table is:\n"+
"\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"\n"+
"Now, testing runEnclosures\n"+
"\n"+
"");
	/* 	runs _ tab1 domain. 
	
	aStream << 'And the results (ta ta TUM!!) 
	
	' << runs << ' 
	
	and now, run lengths.... 
	
	'. */
	aStream.print("tab1 runAt: -20 ->");
	aStream.print((tab1.runAtInt(-20)));
	aStream.print("\n"+
"\n"+
"tab1 runLengthAt: -10 ->");
	aStream.print((tab1.runAtInt(-10)));
	aStream.print("\n"+
"\n"+
"tab1 runLengthAt: -9 ->");
	aStream.print((tab1.runAtInt(-9)));
	for (int i = -1; i <= 4; i ++ ) {
		aStream.print("\n"+
"\n"+
"tab1 runLengthAt: ");
		aStream.print(i);
		aStream.print(" ->");
		aStream.print((tab1.runAtInt(i)));
	}
	aStream.print("\n"+
"\n"+
"tab1 runLengthAt: 26 ->");
	aStream.print((tab1.runAtInt(26)));
	aStream.print("\n"+
"\n"+
"tab1 runLengthAt: 27 ->");
	aStream.print((tab1.runAtInt(27)));
	aStream.print("\n"+
"\n"+
"tab1 runLengthAt: 28 ->");
	aStream.print((tab1.runAtInt(28)));
	aStream.print("\n"+
"\n"+
"tab1 runLengthAt: 30 ->");
	aStream.print((tab1.runAtInt(30)));
	aStream.print("\n"+
"\n"+
"tab1 runAt.IntegerVar: 31 ->");
	aStream.print((tab1.runAtInt(31)));
	aStream.print("\n"+
"\n"+
"tab1 runAt.IntegerVar: 32 ->");
	aStream.print((tab1.runAtInt(32)));
/*
udanax-top.st:58635:GrandHashTableTester methodsFor: 'tests'!
{void} test7On: aStream {ostream reference} 
	"self runTest: #test7On:"
	"Not currently appropriate to GrandHashTable"
	"runs {Iterator}"
	"test creation"
	| tab1 {MuTable} |
	aStream << 'Create tables.
'.
	tab1 _ GrandHashTable make: IntegerSpace make.
	tab1 atInt: 1 introduce: (UInt8Array string: 'filly').
	tab1 atInt: IntegerVar0 introduce: (UInt8Array string: 'mare').
	tab1 atInt: -1 introduce: (UInt8Array string: 'colt').
	tab1 atInt: 27 introduce: (UInt8Array string: 'stallion').
	aStream << 'Starting table is:
' << tab1 << '
with count ' << tab1 count << '
Now, testing runEnclosures
'.	"	runs _ tab1 domain. 
	
	aStream << 'And the results (ta ta TUM!!) 
	
	' << runs << ' 
	
	and now, run lengths.... 
	
	'."
	aStream << 'tab1 runAt: -20 ->' << (tab1 runAtInt: -20).
	aStream << '
tab1 runLengthAt: -10 ->' << (tab1 runAtInt: -10).
	aStream << '
tab1 runLengthAt: -9 ->' << (tab1 runAtInt: -9).
	-1 to: 4 do: [:i {IntegerVar} | aStream << '
tab1 runLengthAt: ' << i << ' ->' << (tab1 runAtInt: i)].
	aStream << '
tab1 runLengthAt: 26 ->' << (tab1 runAtInt: 26).
	aStream << '
tab1 runLengthAt: 27 ->' << (tab1 runAtInt: 27).
	aStream << '
tab1 runLengthAt: 28 ->' << (tab1 runAtInt: 28).
	aStream << '
tab1 runLengthAt: 30 ->' << (tab1 runAtInt: 30).
	aStream << '
tab1 runAt.IntegerVar: 31 ->' << (tab1 runAtInt: 31).
	aStream << '
tab1 runAt.IntegerVar: 32 ->' << (tab1 runAtInt: 32)!
*/
}
public void allTestsOn(PrintWriter aStream) {
	aStream.print("Running all HashTable tests.\n"+
"Test 1\n"+
"");
	test1On(aStream);
	aStream.print("\n"+
"Test 2\n"+
"");
	test2On(aStream);
	aStream.print("\n"+
"Test 3\n"+
"");
	test3On(aStream);
	aStream.print("\n"+
"Test 4\n"+
"");
	test4On(aStream);
	aStream.print("\n"+
"Test 5\n"+
"");
	test5On(aStream);
	bigTableTestOn(aStream);
/*
udanax-top.st:58697:GrandHashTableTester methodsFor: 'running tests'!
{void} allTestsOn: aStream {ostream reference} 
	aStream << 'Running all HashTable tests.
Test 1
'.
	self test1On: aStream.
	aStream << '
Test 2
'.
	self test2On: aStream.
	aStream << '
Test 3
'.
	self test3On: aStream.
	aStream << '
Test 4
'.
	self test4On: aStream.
	aStream << '
Test 5
'.
	self test5On: aStream.
	self bigTableTestOn: aStream.!
*/
}
public void runTest(Object test) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:58724:GrandHashTableTester methodsFor: 'smalltalk: smalltalk tests'!
runTest: test 
	| str |
	str _ WriteStream on: (String new: 200).
	self perform: test with: str.
	Transcript show: str contents; endEntry!
*/
}
public GrandHashTable stomp(int anInt) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:58730:GrandHashTableTester methodsFor: 'smalltalk: smalltalk tests'!
{GrandHashTable} stomp: anInt {UInt32} 
	| table rGen rNum |
	table _ GrandHashTable make: anInt.
	rGen _ Random new.
	0 to: 1000 do: [:i | rNum _ rGen next * 32768.
		(table includesKey: rNum)
			ifTrue: [table at: rNum replace: i]
			ifFalse: [table at: rNum introduce: i]].
	^table!
*/
}
public GrandHashTable stomp(int anInt, int anotherInt) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:58740:GrandHashTableTester methodsFor: 'smalltalk: smalltalk tests'!
{GrandHashTable} stomp: anInt {UInt32} with: anotherInt {UInt32}
|table rGen rNum|
table _ GrandHashTable make: anInt.
rGen _ Random new.
0 to: (anotherInt - 1) do: [:i |
	rNum _ ((rGen next) * 32768) asInteger.
	(table includesIntKey: rNum) ifTrue: [table atInt: rNum replace: i]
	ifFalse: [table atInt: rNum introduce: i]].
^ table!
*/
}
public GrandHashTableTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:58752:GrandHashTableTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:58755:GrandHashTableTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public GrandHashTableTester() {
/*

Generated during transformation
*/
}
}
