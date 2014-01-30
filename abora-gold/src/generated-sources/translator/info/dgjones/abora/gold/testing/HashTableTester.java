/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.testing;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.HashTable;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.testing.HashTableTester;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class HashTableTester extends Tester {

/*
udanax-top.st:58758:
Tester subclass: #HashTableTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Testing'!
*/
/*
udanax-top.st:58762:
(HashTableTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HashTableTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
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
	tab1 = HashTable.makeCoordinateSpace(IntegerSpace.make());
	tab2 = HashTable.makeCoordinateSpace(IntegerSpace.make(), 4);
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
	tab1.intIntroduce(1, (Sequence.string("filly")));
	tab1.intIntroduce(0, (Sequence.string("mare")));
	oo.print("Test introduce: ");
	oo.print(tab1);
	oo.print(", table count now: ");
	oo.print(tab1.count());
	oo.print("\n"+
"\n"+
"");
	tab1.introduce((Sequence.string("mare")), (Sequence.string("colt")));
	oo.print("Test introduce: ");
	oo.print(tab1);
	oo.print(", table count now: ");
	oo.print(tab1.count());
	oo.print("\n"+
"\n"+
"");
	tab1.intIntroduce(27, (Sequence.string("stallion")));
	oo.print("Test introduce: ");
	oo.print(tab1);
	oo.print(", table count now: ");
	oo.print(tab1.count());
	oo.print("\n"+
"\n"+
"");
	try {
		tab1.intIntroduce(1, (Sequence.string("palooka")));
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
udanax-top.st:58767:HashTableTester methodsFor: 'tests'!
{void} test1On: oo {ostream reference} 
	"self runTest: #test1On:"
	"test creation"
	| tab1 {MuTable} tab2 {MuTable} |
	oo << 'Create tables with create, create: and create:with:
'.
	tab1 _ HashTable make.CoordinateSpace: IntegerSpace make.
	tab2 _ HashTable make.CoordinateSpace: IntegerSpace make with: 4.	"test printing"
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
	tab1 atInt: 1 introduce: (Sequence string: 'filly').
	tab1 atInt: IntegerVar0 introduce: (Sequence string: 'mare').
	oo << 'Test introduce: ' << tab1 << ', table count now: ' << tab1 count << '
'.
	tab1 at: (Sequence string: 'mare') introduce: (Sequence string: 'colt').
	oo << 'Test introduce: ' << tab1 << ', table count now: ' << tab1 count << '
'.
	tab1 atInt: 27 introduce: (Sequence string: 'stallion').
	oo << 'Test introduce: ' << tab1 << ', table count now: ' << tab1 count << '
'.
	MuTable problems.AlreadyInTable handle: [:ex | oo << 'already in table blast caught, table now:
' << tab1 << '
and table count: ' << tab1 count << '
'.
		^VOID]
		do: [tab1 atInt: 1 introduce: (Sequence string: 'palooka')].
	oo << 'Test empty table: '.
	tab1 isEmpty
			ifTrue: [oo << 'Empty']
			ifFalse: [oo << 'Not Empty'].
	oo << '
'!
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
	tab1 = HashTable.makeCoordinateSpace(IntegerSpace.make());
	tab1.intIntroduce(1, (Sequence.string("filly")));
	tab1.intIntroduce(0, (Sequence.string("mare")));
	tab1.intIntroduce(-1, (Sequence.string("colt")));
	tab1.intIntroduce(27, (Sequence.string("stallion")));
	aStream.print("Starting table is:\n"+
"\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"\n"+
"");
	tab1.intReplace(1, (Sequence.string("mare")));
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
		tab1.intReplace(2, (Sequence.string("palooka")));
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
udanax-top.st:58821:HashTableTester methodsFor: 'tests'!
{void} test2On: aStream {ostream reference} 
	"self runTest: #test2On:"
	"test creation"
	| tab1 {MuTable} |
	aStream << 'Create tables.
'.
	tab1 _ HashTable make.CoordinateSpace: IntegerSpace make.
	tab1 atInt: 1 introduce: (Sequence string: 'filly').
	tab1 atInt: IntegerVar0 introduce: (Sequence string: 'mare').
	tab1 atInt: -1 introduce: (Sequence string: 'colt').
	tab1 atInt: 27 introduce: (Sequence string: 'stallion').
	aStream << 'Starting table is:
' << tab1 << '
'.
	tab1 atInt: 1 replace: (Sequence string: 'mare').
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
		do: [tab1 atInt: 2 replace: (Sequence string: 'palooka')].
	aStream << 'Test replace() with NULL. 
'.
	MuTable problems.NullInsertion handle: [:x | aStream << 'NullInsertion blast caught, table now:
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
	tab1 = HashTable.makeCoordinateSpace(IntegerSpace.make());
	tab1.intIntroduce(1, (Sequence.string("filly")));
	tab1.intIntroduce(0, (Sequence.string("mare")));
	tab1.intIntroduce(-1, (Sequence.string("colt")));
	tab1.intIntroduce(27, (Sequence.string("stallion")));
	aStream.print("Starting table is:\n"+
"\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"");
	tab1.intStore(1, (Sequence.string("mare")));
	aStream.print("after store:\n"+
"");
	aStream.print(tab1);
	aStream.print(" and table count: ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"");
	aStream.print("Test store() in unknown territory. \n"+
"");
	try {
		tab1.intStore(2, (Sequence.string("palooka")));
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
	aStream.print("after store:\n"+
"");
	aStream.print(tab1);
	aStream.print(" and table count: ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"");
	aStream.print("Test store() with NULL. \n"+
"");
	try {
		tab1.intStore(3, null);
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
udanax-top.st:58862:HashTableTester methodsFor: 'tests'!
{void} test3On: aStream {ostream reference} 
	"self runTest: #test3On:"
	"test creation"
	| tab1 {MuTable} |
	aStream << 'Create tables.
'.
	tab1 _ HashTable make.CoordinateSpace: IntegerSpace make.
	tab1 atInt: 1 introduce: (Sequence string: 'filly').
	tab1 atInt: IntegerVar0 introduce: (Sequence string: 'mare').
	tab1 atInt: -1 introduce: (Sequence string: 'colt').
	tab1 atInt: 27 introduce: (Sequence string: 'stallion').
	aStream << 'Starting table is:
' << tab1 << '
'.
	tab1 atInt: 1 store: (Sequence string: 'mare'). 
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
		do: [tab1 atInt: 2 store: (Sequence string: 'palooka')].
	aStream << 'after store:
' << tab1 << ' and table count: ' << tab1 count << '
'.
	aStream << 'Test store() with NULL. 
'.
	MuTable problems.NullInsertion handle: [:exc | aStream << 'NullInsertion blast caught, table now:
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
	tab1 = HashTable.makeCoordinateSpace(IntegerSpace.make());
	tab1.introduce(IntegerPos.make(1), (Sequence.string("filly")));
	tab1.introduce(IntegerPos.zero(), (Sequence.string("mare")));
	tab1.introduce(IntegerPos.make(-1), (Sequence.string("colt")));
	tab1.introduce(IntegerPos.make(27), (Sequence.string("stallion")));
	aStream.print("Starting table is:\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"");
	/* testing domain */
	aStream.print("Testing domain\n"+
"");
	aStream.print(tab1.domain());
	aStream.print("\n"+
"");
	/* test get */
	aStream.print("Test get(1) ");
	aStream.print((tab1.intGet(1)));
	aStream.print("\n"+
"");
	aStream.print("Test get() in unknown territory. \n"+
"");
	try {
		tab1.intGet(14);
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
/*
udanax-top.st:58903:HashTableTester methodsFor: 'tests'!
{void} test4On: aStream {ostream reference} 
	"self runTest: #test4On:"
	"test creation"
	| tab1 {MuTable} |
	aStream << 'Create tables.
'.
	tab1 _ HashTable make.CoordinateSpace: IntegerSpace make.
	tab1 at: 1 integer introduce: (Sequence string: 'filly').
	tab1 at: Integer0 introduce: (Sequence string: 'mare').
	tab1 at: -1 integer introduce: (Sequence string: 'colt').
	tab1 at: 27 integer introduce: (Sequence string: 'stallion').
	aStream << 'Starting table is:
' << tab1 << '
with count ' << tab1 count << '
'.	"testing domain"
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
	tab1 = HashTable.makeCoordinateSpace(IntegerSpace.make());
	tab1.intIntroduce(1, (Sequence.string("filly")));
	tab1.intIntroduce(0, (Sequence.string("mare")));
	tab1.intIntroduce(-1, (Sequence.string("colt")));
	tab1.intIntroduce(27, (Sequence.string("stallion")));
	aStream.print("Starting table is:\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"Now, testing remove(1)\n"+
"");
	tab1.intRemove(1);
	aStream.print("Table now:\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"");
	aStream.print("Test remove(1) in unknown territory. \n"+
"");
	try {
		tab1.intRemove(1);
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
	aStream.print("Test wipe(0)\n"+
"");
	tab1.wipe(IntegerPos.zero());
	aStream.print("Table now:\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"And wipe(0) again: ");
	tab1.wipe(IntegerPos.zero());
	aStream.print("Table now:\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"");
/*
udanax-top.st:58934:HashTableTester methodsFor: 'tests'!
{void} test5On: aStream {ostream reference} 
	"self runTest: #test5On:"
	"test creation"
	| tab1 {MuTable} |
	aStream << 'Create tables.
'.
	tab1 _ HashTable make.CoordinateSpace: IntegerSpace make.
	tab1 atInt: 1 introduce: (Sequence string: 'filly').
	tab1 atInt: IntegerVar0 introduce: (Sequence string: 'mare').
	tab1 atInt: -1 introduce: (Sequence string: 'colt').
	tab1 atInt: 27 introduce: (Sequence string: 'stallion').
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
 * self runTest: #test6On:
 */
public void test6On(PrintWriter aStream) {
	/* test creation */
	MuTable tab1;
	aStream.print("Create tables.\n"+
"\n"+
"");
	tab1 = HashTable.makeCoordinateSpace(IntegerSpace.make());
	tab1.intIntroduce(1, (Sequence.string("filly")));
	tab1.intIntroduce(0, (Sequence.string("mare")));
	tab1.intIntroduce(-1, (Sequence.string("colt")));
	tab1.intIntroduce(27, (Sequence.string("stallion")));
	/* 	tab2 _ tab1 subTable: 0 integer with: 40. 
	
	aStream << 'Table now: 
	' << tab1 << ' 
	with count ' << tab1 count << ' 
	and the subtable is 
	' << tab2 << ' 
	and its count is ' << tab2 count << '. 
	'. */
	aStream.print("Starting table is:\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"Now, testing subTable(0,40)\n"+
"");
/*
udanax-top.st:58978:HashTableTester methodsFor: 'tests'!
{void} test6On: aStream {ostream reference} 
	"self runTest: #test6On:"
	"test creation"
	| tab1 {MuTable} |
	aStream << 'Create tables.
'.
	tab1 _ HashTable make.CoordinateSpace: IntegerSpace make.
	tab1 atInt: 1 introduce: (Sequence string: 'filly').
	tab1 atInt: IntegerVar0 introduce: (Sequence string: 'mare').
	tab1 atInt: -1 introduce: (Sequence string: 'colt').
	tab1 atInt: 27 introduce: (Sequence string: 'stallion').	"	tab2 _ tab1 subTable: 0 integer with: 40. 
	
	aStream << 'Table now: 
	' << tab1 << ' 
	with count ' << tab1 count << ' 
	and the subtable is 
	' << tab2 << ' 
	and its count is ' << tab2 count << '. 
	'."
	aStream << 'Starting table is:
' << tab1 << '
with count ' << tab1 count << '
Now, testing subTable(0,40)
'!
*/
}
/**
 * self runTest: #test7On:
 */
public void test7On(PrintWriter aStream) {
	/* runs {Iterator} */
	/* test creation */
	MuTable tab1;
	XnRegion domain;
	aStream.print("Create tables.\n"+
"\n"+
"");
	tab1 = HashTable.makeCoordinateSpace(IntegerSpace.make());
	tab1.intIntroduce(1, (UInt8Array.string("filly")));
	tab1.intIntroduce(0, (UInt8Array.string("mare")));
	tab1.intIntroduce(-1, (UInt8Array.string("colt")));
	tab1.intIntroduce(27, (UInt8Array.string("stallion")));
	aStream.print("Starting table is:\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"Now, testing domain\n"+
"");
	domain = tab1.domain();
	aStream.print("And the results (ta ta TUM!) \n"+
"	");
	aStream.print(domain);
	aStream.print("\n"+
"");
/*
udanax-top.st:59005:HashTableTester methodsFor: 'tests'!
{void} test7On: aStream {ostream reference} 
	"self runTest: #test7On:"
	"runs {Iterator}"
	"test creation"
	| tab1 {MuTable} domain {XnRegion} |
	aStream << 'Create tables.
'.
	tab1 _ HashTable make.CoordinateSpace: IntegerSpace make.
	tab1 atInt: 1 introduce: (UInt8Array string: 'filly').
	tab1 atInt: IntegerVar0 introduce: (UInt8Array string: 'mare').
	tab1 atInt: -1 introduce: (UInt8Array string: 'colt').
	tab1 atInt: 27 introduce: (UInt8Array string: 'stallion').
	aStream << 'Starting table is:
' << tab1 << '
with count ' << tab1 count << '
Now, testing domain
'.	
	domain _ tab1 domain. 
	
	aStream << 'And the results (ta ta TUM!!) 
	' << domain << '
'.!
*/
}
/**
 * self runTest: #testStepperCopyOn:
 */
public void testStepperCopyOn(PrintWriter aStream) {
	MuTable tab1;
	MuTable tab2;
	TableStepper stp;
	aStream.print("Test copy by stepper.\n"+
"");
	tab1 = HashTable.makeCoordinateSpace(IntegerSpace.make());
	tab1.intIntroduce(1, (UInt8Array.string("filly")));
	tab1.intIntroduce(0, (UInt8Array.string("mare")));
	tab1.intIntroduce(-1, (UInt8Array.string("colt")));
	tab1.intIntroduce(27, (UInt8Array.string("stallion")));
	aStream.print("Starting table is:\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print(".\n"+
"Now testing store during forEach loop\n"+
"");
	Stepper stomper = (stp = tab1.stepper());
	for (; stomper.hasValue(); stomper.step()) {
		Heaper e = (Heaper) stomper.fetch();
		if (e == null) {
			continue ;
		}
		aStream.print("at index ");
		aStream.print(stp.position());
		aStream.print(" storing ");
		aStream.print(stp.position());
		aStream.print(" on top of ");
		aStream.print(e);
		aStream.print("\n"+
"");
		tab1.store(stp.position(), stp.position());
	}
	stomper.destroy();
	aStream.print("Ending table is:\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print(".\n"+
"");
	tab2 = (MuTable) tab1.copy();
	Stepper stomper2 = (stp = tab2.stepper());
	for (; stomper2.hasValue(); stomper2.step()) {
		Heaper x = (Heaper) stomper2.fetch();
		if (x == null) {
			continue ;
		}
		aStream.print("at index ");
		aStream.print(stp.position());
		aStream.print(" storing 'foo' on top of ");
		aStream.print(x);
		aStream.print("\n"+
"");
		tab2.store(stp.position(), (UInt8Array.string("foo")));
	}
	stomper2.destroy();
	aStream.print("Ending table is:\n"+
"");
	aStream.print(tab2);
	aStream.print("\n"+
"with count ");
	aStream.print(tab2.count());
	aStream.print(".\n"+
"Done with stepperCopy test.\n"+
"");
/*
udanax-top.st:59030:HashTableTester methodsFor: 'tests'!
{void} testStepperCopyOn: aStream {ostream reference} 
	"self runTest: #testStepperCopyOn:"
	| tab1 {MuTable} tab2 {MuTable} stp {TableStepper} |
	aStream << 'Test copy by stepper.
'.
	tab1 _ HashTable make.CoordinateSpace: IntegerSpace make.
	tab1 atInt: 1 introduce: (UInt8Array string: 'filly').
	tab1 atInt: IntegerVar0 introduce: (UInt8Array string: 'mare').
	tab1 atInt: -1 introduce: (UInt8Array string: 'colt').
	tab1 atInt: 27 introduce: (UInt8Array string: 'stallion').
	aStream << 'Starting table is:
' << tab1 << '
with count ' << tab1 count << '.
Now testing store during forEach loop
'.	
	(stp _ tab1 stepper) forEach: [ :e {Heaper} |
		aStream << 'at index ' << stp position << ' storing ' << stp position << ' on top of ' << e << '
'.
		tab1 at: stp position store: stp position].
	aStream << 'Ending table is:
' << tab1 << '
with count ' << tab1 count << '.
'.
	tab2 _ tab1 copy cast: MuTable.
	(stp _ tab2 stepper) forEach: [ :x {Heaper} |
		aStream << 'at index ' << stp position << ' storing ''foo'' on top of ' << x << '
'.
		tab2 at: stp position store: (UInt8Array string: 'foo')].
	aStream << 'Ending table is:
' << tab2 << '
with count ' << tab2 count << '.
Done with stepperCopy test.
'.!
*/
}
/**
 * self runTest: #allTestsOn:
 */
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
	aStream.print("\n"+
"Test 6\n"+
"");
	test6On(aStream);
	aStream.print("\n"+
"Test 7\n"+
"");
	test7On(aStream);
	testStepperCopyOn(aStream);
/*
udanax-top.st:59067:HashTableTester methodsFor: 'running tests'!
{void} allTestsOn: aStream {ostream reference}
	"self runTest: #allTestsOn:"
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
	aStream << '
Test 6
'.
	self test6On: aStream.
	aStream << '
Test 7
'.
	self test7On: aStream.
	self testStepperCopyOn: aStream.!
*/
}
public HashTableTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:59103:HashTableTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:59106:HashTableTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public HashTableTester() {
/*

Generated during transformation
*/
}
}
