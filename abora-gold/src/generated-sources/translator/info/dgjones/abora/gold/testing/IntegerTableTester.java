/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.testing;

import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.IntegerTable;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.testing.IntegerTableTester;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

public class IntegerTableTester extends Tester {

/*
udanax-top.st:59146:
Tester subclass: #IntegerTableTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Testing'!
*/
/*
udanax-top.st:59150:
(IntegerTableTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IntegerTableTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public void cleanTable(ScruTable aTable) {
	TableStepper stomp;
	stomp = aTable.stepper();
	while (stomp.hasValue()) {
		stomp.fetch().destroy();
		stomp.step();
	}
	aTable.destroy();
/*
udanax-top.st:59155:IntegerTableTester methodsFor: 'testing'!
{void} cleanTable: aTable {ScruTable} 
	| stomp {TableStepper} |
	stomp _ aTable stepper.
	[stomp hasValue]
		whileTrue: 
			[stomp fetch destroy.
			stomp step].
	aTable destroy!
*/
}
/**
 * IntegerTableTester runTest: #test1On:
 */
public void test1On(PrintWriter oo) {
	/* test creation */
	MuTable tab1;
	MuTable tab2;
	MuTable tab3;
	oo.print("Create tables with create, create: and create:with:\n"+
"");
	tab1 = IntegerTable.make();
	tab2 = IntegerTable.makeIntegerVar(4);
	tab3 = IntegerTable.makeIntegerVar(5, 9);
	/* test printing */
	oo.print("Printing tables:\n"+
"");
	oo.print(tab1);
	oo.print("\n"+
"");
	oo.print(tab2);
	oo.print("\n"+
"");
	oo.print(tab3);
	oo.print("\n"+
"");
	/* testing */
	oo.print("Test empty table: ");
	if (tab2.isEmpty()) {
		oo.print("Empty");
	}
	else {
		oo.print("Not Empty");
	}
	oo.print("\n"+
"");
	/* inserting */
	tab1.introduce((IntegerPos.make(9)), (Sequence.string("filly")));
	tab1.intIntroduce(0, (Sequence.string("mare")));
	oo.print("Test introduce: ");
	oo.print(tab1);
	oo.print(", table count now: ");
	oo.print(tab1.count());
	oo.print("\n"+
"");
	tab1.intIntroduce(-11, (Sequence.string("colt")));
	oo.print("Test introduce: ");
	oo.print(tab1);
	oo.print(", table count now: ");
	oo.print(tab1.count());
	oo.print("\n"+
"");
	tab1.intIntroduce(47, (Sequence.string("stallion")));
	oo.print("Test introduce: ");
	oo.print(tab1);
	oo.print(", table count now: ");
	oo.print(tab1.count());
	oo.print("\n"+
"");
	try {
		tab1.intIntroduce(1, (Sequence.string("palooka")));
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.ALREADY_IN_TABLE.equals(ex.getMessage())) {
			oo.print("already in table blast caught, table now:\n"+
"");
			oo.print(tab1);
			oo.print("\n"+
"and table count: ");
			oo.print(tab1.count());
			oo.print("\n"+
"");
			return ;
		}
		else {
			throw ex;
		}
	}
	oo.print("Test introduce: \n"+
"");
	oo.print("Testing introduce and fetch boundary conditions\n"+
"");
	for (int i = 5; i <= 9; i ++ ) {
		tab3.intIntroduce(i, IntegerPos.make(i));
		oo.print("fetch of (");
		oo.print(i);
		oo.print(") is ");
		oo.print((tab3.intFetch(i)));
		oo.print("\n"+
"");
	}
	oo.print("table 3 now:\n"+
"");
	oo.print(tab3);
	oo.print("\n"+
"and its count is ");
	oo.print(tab3.count());
	oo.print("\n"+
"");
	tab3.intIntroduce(4, IntegerPos.make(4));
	tab3.intIntroduce(10, IntegerPos.make(10));
	tab3.intIntroduce(11, IntegerPos.make(11));
	oo.print("table 3 now:\n"+
"");
	oo.print(tab3);
	oo.print("\n"+
"and its count is ");
	oo.print(tab3.count());
	oo.print("\n"+
"");
	cleanTable(tab1);
	cleanTable(tab2);
	cleanTable(tab3);
/*
udanax-top.st:59164:IntegerTableTester methodsFor: 'testing'!
{void} test1On: oo {ostream reference} 
	"IntegerTableTester runTest: #test1On:"
	"test creation"
	| tab1 {MuTable} tab2 {MuTable} tab3 {MuTable} |
	oo << 'Create tables with create, create: and create:with:
'.
	tab1 _ IntegerTable make.
	tab2 _ IntegerTable make.IntegerVar: 4.
	tab3 _ IntegerTable make.IntegerVar: 5 with: 9.	"test printing"
	oo << 'Printing tables:
' << tab1 << '
' << tab2 << '
' << tab3 << '
'.	"testing"
	oo << 'Test empty table: '.
	tab2 isEmpty
			ifTrue: [oo << 'Empty']
			ifFalse: [oo << 'Not Empty'].
	oo << '
'.	"inserting"
	tab1 at: (9 integer)
		introduce: (Sequence string: 'filly').
	tab1 atInt: IntegerVar0
		introduce: (Sequence string: 'mare').
	oo << 'Test introduce: ' << tab1 << ', table count now: ' << tab1 count << '
'.
	tab1 atInt: -11
		introduce: (Sequence string: 'colt').
	oo << 'Test introduce: ' << tab1 << ', table count now: ' << tab1 count << '
'.
	tab1 atInt: 47
		introduce: (Sequence string: 'stallion').
	oo << 'Test introduce: ' << tab1 << ', table count now: ' << tab1 count << '
'.
	MuTable problems.AlreadyInTable handle: [:ex | oo << 'already in table blast caught, table now:
' << tab1 << '
and table count: ' << tab1 count << '
'.
		^VOID]
		do: [tab1 atInt: 1
				introduce: (Sequence string: 'palooka')].
	oo << 'Test introduce: 
'.
	oo << 'Testing introduce and fetch boundary conditions
'.
	5 to: 9 do: [:i {IntegerVar} | tab3 atInt: i introduce: i integer.
		oo << 'fetch of (' << i << ') is ' << (tab3 intFetch: i) << '
'].
	oo << 'table 3 now:
' << tab3 << '
and its count is ' << tab3 count << '
'.
	tab3 atInt: 4
		introduce: 4 integer.
	tab3 atInt: 10
		introduce: 10 integer.
	tab3 atInt: 11
		introduce: 11 integer.
	oo << 'table 3 now:
' << tab3 << '
and its count is ' << tab3 count << '
'.
	self cleanTable: tab1.
	self cleanTable: tab2.
	self cleanTable: tab3!
*/
}
/**
 * self runTest: #test2On:
 */
public void test2On(PrintWriter aStream) {
	/* test creation */
	MuTable tab1;
	aStream.print("Create tables.\n"+
"");
	tab1 = IntegerTable.make();
	tab1.intIntroduce(1, (Sequence.string("filly")));
	tab1.intIntroduce(0, (Sequence.string("mare")));
	tab1.intIntroduce(-1, (Sequence.string("colt")));
	tab1.intIntroduce(27, (Sequence.string("stallion")));
	aStream.print("Starting table is:\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
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
	cleanTable(tab1);
/*
udanax-top.st:59231:IntegerTableTester methodsFor: 'testing'!
{void} test2On: aStream {ostream reference} 
	"self runTest: #test2On:"
	"test creation"
	| tab1 {MuTable} |
	aStream << 'Create tables.
'.
	tab1 _ IntegerTable make.
	tab1 atInt: 1
		introduce: (Sequence string: 'filly').
	tab1 atInt: IntegerVar0
		introduce: (Sequence string: 'mare').
	tab1 atInt: -1
		introduce: (Sequence string: 'colt').
	tab1 atInt: 27
		introduce: (Sequence string: 'stallion').
	aStream << 'Starting table is:
' << tab1 << '
'.
	tab1 atInt: 1
		replace: (Sequence string: 'mare').
	aStream << 'after replace:
' << tab1 << ' and table count: ' << tab1 count << '
'.
	aStream << 'Test replace() in unknown territory. 
'.
	(ScruTable problems.NotInTable) handle: [:ex | aStream << 'NotInTable blast caught, table now:
' << tab1 << '
and table count: ' << tab1 count << '
'.
		^VOID]
		do: [tab1 atInt: 2
				replace: (Sequence string: 'palooka')].
	aStream << 'Test replace() with NULL. 
'.
	(MuTable problems.NullInsertion) handle: [:exc | aStream << 'NullInsertion blast caught, table now:
' << tab1 << '
and table count: ' << tab1 count << '
'.
		^VOID]
		do: [tab1 atInt: 1
				replace: NULL.
			aStream << 'Replace(NULL) not caught!!
'].
	self cleanTable: tab1!
*/
}
/**
 * self runTest: #test3On:
 */
public void test3On(PrintWriter aStream) {
	/* test creation */
	MuTable tab1;
	aStream.print("Create tables.\n"+
"");
	tab1 = IntegerTable.make();
	tab1.intIntroduce(1, (Sequence.string("filly")));
	tab1.intIntroduce(0, (Sequence.string("mare")));
	tab1.intIntroduce(-1, (Sequence.string("colt")));
	tab1.intIntroduce(27, (Sequence.string("stallion")));
	aStream.print("Starting table is:\n"+
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
	cleanTable(tab1);
/*
udanax-top.st:59277:IntegerTableTester methodsFor: 'testing'!
{void} test3On: aStream {ostream reference} 
	"self runTest: #test3On:"
	"test creation"
	| tab1 {MuTable} |
	aStream << 'Create tables.
'.
	tab1 _ IntegerTable make.
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
	(ScruTable problems.NotInTable) handle: [:ex | aStream << 'NotInTable blast caught, table now:
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
	(MuTable problems.NullInsertion) handle: [:exc | aStream << 'NullInsertion blast caught, table now:
' << tab1 << '
and table count: ' << tab1 count << '
'.
		^VOID]
		do: [tab1 atInt: 3 store: NULL].
	self cleanTable: tab1!
*/
}
/**
 * self runTest: #test4On:
 */
public void test4On(PrintWriter aStream) {
	/* test creation */
	MuTable tab1;
	aStream.print("Create tables.\n"+
"");
	tab1 = IntegerTable.make();
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
	cleanTable(tab1);
/*
udanax-top.st:59317:IntegerTableTester methodsFor: 'testing'!
{void} test4On: aStream {ostream reference} 
	"self runTest: #test4On:"
	"test creation"
	| tab1 {MuTable} |
	aStream << 'Create tables.
'.
	tab1 _ IntegerTable make.
	tab1 atInt: 1
		introduce: (Sequence string: 'filly').
	tab1 atInt: IntegerVar0
		introduce: (Sequence string: 'mare').
	tab1 atInt: -1
		introduce: (Sequence string: 'colt').
	tab1 atInt: 27
		introduce: (Sequence string: 'stallion').
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
	(ScruTable problems.NotInTable) handle: [:ex | aStream << 'NotInTable blast caught, table now:
' << tab1 << '
and table count: ' << tab1 count << '
'.
		^VOID]
		do: [tab1 intGet:14].
	self cleanTable: tab1!
*/
}
/**
 * self runTest: #test5On:
 */
public void test5On(PrintWriter aStream) {
	/* test creation */
	MuTable tab1;
	aStream.print("Create tables.\n"+
"");
	tab1 = IntegerTable.make();
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
	tab1.intWipe(0);
	aStream.print("Table now:\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"And wipe(0) again: ");
	tab1.intWipe(0);
	aStream.print("Table now:\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"");
	cleanTable(tab1);
/*
udanax-top.st:59352:IntegerTableTester methodsFor: 'testing'!
{void} test5On: aStream {ostream reference} 
	"self runTest: #test5On:"
	"test creation"
	| tab1 {MuTable} |
	aStream << 'Create tables.
'.
	tab1 _ IntegerTable make.
	tab1 atInt: 1
		introduce: (Sequence string: 'filly').
	tab1 atInt: IntegerVar0
		introduce: (Sequence string: 'mare').
	tab1 atInt: -1
		introduce: (Sequence string: 'colt').
	tab1 atInt: 27
		introduce: (Sequence string: 'stallion').
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
	tab1 intWipe: IntegerVar0.
	aStream << 'Table now:
' << tab1 << '
with count ' << tab1 count << '
And wipe(0) again: '.
	tab1 intWipe: IntegerVar0.
	aStream << 'Table now:
' << tab1 << '
with count ' << tab1 count << '
'.
	self cleanTable: tab1!
*/
}
/**
 * self runTest: #test6On:
 */
public void test6On(PrintWriter aStream) {
	/* test creation */
	MuTable tab1;
	ScruTable tab2;
	aStream.print("Create tables.\n"+
"");
	tab1 = IntegerTable.make();
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
"Now, testing subTable(0,40)\n"+
"");
	tab2 = ((IntegerTable) tab1).subTable((IntegerRegion.make(0, 40)));
	aStream.print("Table now:\n"+
"");
	aStream.print(tab1);
	aStream.print("\n"+
"with count ");
	aStream.print(tab1.count());
	aStream.print("\n"+
"and the subtable is\n"+
"");
	aStream.print(tab2);
	aStream.print("\n"+
"and its count is ");
	aStream.print(tab2.count());
	aStream.print(".\n"+
"");
	cleanTable(tab1);
/*
udanax-top.st:59400:IntegerTableTester methodsFor: 'testing'!
{void} test6On: aStream {ostream reference} 
	"self runTest: #test6On:"
	"test creation"
	| tab1 {MuTable} tab2 {ScruTable} |
	aStream << 'Create tables.
'.
	tab1 _ IntegerTable make.
	tab1 atInt: 1
		introduce: (Sequence string: 'filly').
	tab1 atInt: IntegerVar0
		introduce: (Sequence string: 'mare').
	tab1 atInt: -1
		introduce: (Sequence string: 'colt').
	tab1 atInt: 27
		introduce: (Sequence string: 'stallion').
	aStream << 'Starting table is:
' << tab1 << '
with count ' << tab1 count << '
Now, testing subTable(0,40)
'.
	tab2 _ (tab1 cast: IntegerTable)
				subTable: (IntegerRegion make: IntegerVar0
										with: 40).
	aStream << 'Table now:
' << tab1 << '
with count ' << tab1 count << '
and the subtable is
' << tab2 << '
and its count is ' << tab2 count << '.
'.
	self cleanTable: tab1!
*/
}
/**
 * self runTest: #test7On:
 */
public void test7On(PrintWriter aStream) {
	/* runs {Iterator} */
	/* test creation */
	MuTable tab1;
	XnRegion dom;
	aStream.print("Create tables.\n"+
"");
	tab1 = IntegerTable.make();
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
"Now, testing domain\n"+
"");
	dom = tab1.domain();
	aStream.print("And the results (ta ta TUM!) \n"+
"	");
	aStream.print(dom);
	aStream.print("\n"+
"");
	aStream.print("\n"+
"and now, run lengths....\n"+
"");
	aStream.print("tab1 runAt.IntegerVar: -20 ->");
	aStream.print((tab1.runAtInt(-20)));
	aStream.print("\n"+
"tab1 runAt.IntegerVar: -10 ->");
	aStream.print((tab1.runAtInt(-10)));
	aStream.print("\n"+
"tab1 runAt.IntegerVar:  -9 ->");
	aStream.print((tab1.runAtInt(-9)));
	for (int i = -1; i <= 4; i ++ ) {
		aStream.print("\n"+
"tab1 runAt.IntegerVar:  ");
		aStream.print(i);
		aStream.print(" ->");
		aStream.print((tab1.runAtInt(i)));
	}
	aStream.print("\n"+
"tab1 runAt.IntegerVar:  26 ->");
	aStream.print((tab1.runAtInt(26)));
	aStream.print("\n"+
"tab1 runAt.IntegerVar:  27 ->");
	aStream.print((tab1.runAtInt(27)));
	aStream.print("\n"+
"tab1 runAt.IntegerVar:  28 ->");
	aStream.print((tab1.runAtInt(28)));
	aStream.print("\n"+
"tab1 runAt.IntegerVar:  30 ->");
	aStream.print((tab1.runAtInt(30)));
	aStream.print("\n"+
"tab1 runAt.IntegerVar:  31 ->");
	aStream.print((tab1.runAtInt(31)));
	aStream.print("\n"+
"tab1 runAt.IntegerVar:  32 ->");
	aStream.print((tab1.runAtInt(32)));
	aStream.print("\n"+
"");
	cleanTable(tab1);
/*
udanax-top.st:59433:IntegerTableTester methodsFor: 'testing'!
{void} test7On: aStream {ostream reference} 
	"self runTest: #test7On:"
	"runs {Iterator}"
	"test creation"
	| tab1 {MuTable} dom {XnRegion} |
	aStream << 'Create tables.
'.
	tab1 _ IntegerTable make.
	tab1 atInt: 1 introduce: (Sequence string: 'filly').
	tab1 atInt: IntegerVar0 introduce: (Sequence string: 'mare').
	tab1 atInt: -1 introduce: (Sequence string: 'colt').
	tab1 atInt: 27 introduce: (Sequence string: 'stallion').
	aStream << 'Starting table is:
' << tab1 << '
with count ' << tab1 count << '
Now, testing domain
'.		
	dom _ tab1 domain. 
	aStream << 'And the results (ta ta TUM!!) 
	' << dom << '
'.
	aStream << '
and now, run lengths....
'.
	aStream << 'tab1 runAt.IntegerVar: -20 ->' << (tab1 runAtInt: -20).
	aStream << '
tab1 runAt.IntegerVar: -10 ->' << (tab1 runAtInt: -10).
	aStream << '
tab1 runAt.IntegerVar:  -9 ->' << (tab1 runAtInt: -9).
	-1 to: 4 do: [:i {IntegerVar} | aStream << '
tab1 runAt.IntegerVar:  ' << i << ' ->' << (tab1 runAtInt: i)].
	aStream << '
tab1 runAt.IntegerVar:  26 ->' << (tab1 runAtInt: 26).
	aStream << '
tab1 runAt.IntegerVar:  27 ->' << (tab1 runAtInt: 27).
	aStream << '
tab1 runAt.IntegerVar:  28 ->' << (tab1 runAtInt: 28).
	aStream << '
tab1 runAt.IntegerVar:  30 ->' << (tab1 runAtInt: 30).
	aStream << '
tab1 runAt.IntegerVar:  31 ->' << (tab1 runAtInt: 31).
	aStream << '
tab1 runAt.IntegerVar:  32 ->' << (tab1 runAtInt: 32).
	aStream << '
'.
	self cleanTable: tab1!
*/
}
/**
 * IntegerTableTester runTest
 */
public void allTestsOn(PrintWriter aStream) {
	aStream.print("Running all IntegerTable tests.\n"+
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
/*
udanax-top.st:59483:IntegerTableTester methodsFor: 'running tests'!
{void} allTestsOn: aStream {ostream reference} 
	"IntegerTableTester runTest"
	aStream << 'Running all IntegerTable tests.
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
	self test7On: aStream!
*/
}
public IntegerTableTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:59517:IntegerTableTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:59520:IntegerTableTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public IntegerTableTester() {
/*

Generated during transformation
*/
}
}
