/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.tabent;

import info.dgjones.abora.gold.collection.settable.TableEntry;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.unordered.HeaperAsPosition;
import info.dgjones.abora.gold.tabent.TableEntryTester;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

/**
 * test entries in isolation just for fun
 */
public class TableEntryTester extends Tester {

/*
udanax-top.st:61918:
Tester subclass: #TableEntryTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-tabent'!
*/
/*
udanax-top.st:61922:
TableEntryTester comment:
'test entries in isolation just for fun'!
*/
/*
udanax-top.st:61924:
(TableEntryTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TableEntryTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
/**
 * A regression test is run by calling this method. What the tester writes to 'oo' is
 * actually written to file *o.txt and compared against an approved reference
 * file (*r.txt) of what this tester once used to output. If they match exactly,
 * then the test is passed. Otherwise, someone needs to manually understand why
 * they're different. The diff is in file *d.txt.
 * It is strongly recommended (in order to avoid regression errors) that when a
 * tester is extended to test something new that its output also be extended with
 * some result of the new test. The extended test will then fail the first time. The
 * programmer should verify that the reason for failure is exactly that the
 * tester now additionally outputs the correct results of the new test, in which
 * case this output should be made into the new reference output and the test run
 * again.
 */
public void allTestsOn(PrintWriter oo) {
	test1on(oo);
	test2on(oo);
	test3on(oo);
/*
udanax-top.st:61929:TableEntryTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference} 
	"A regression test is run by calling this method. What the tester writes to 'oo' is 
	actually written to file *o.txt and compared against an approved reference 
	file (*r.txt) of what this tester once used to output. If they match exactly, 
	then the test is passed. Otherwise, someone needs to manually understand why 
	they're different. The diff is in file *d.txt. 
	
	It is strongly recommended (in order to avoid regression errors) that when a 
	tester is extended to test something new that its output also be extended with 
	some result of the new test. The extended test will then fail the first time. The 
	programmer should verify that the reason for failure is exactly that the 
	tester now additionally outputs the correct results of the new test, in which 
	case this output should be made into the new reference output and the test run 
	again."
	self test1on: oo.
	self test2on: oo.
	self test3on: oo.!
*/
}
public void test1on(PrintWriter oo) {
	TableEntry ent1;
	oo.print("start of test 1 - basic stuff\n"+
"");
	ent1 = TableEntry.make((Sequence.string("one")), (Sequence.string("two")));
	oo.print("\n"+
"ent1 == ");
	oo.print(ent1);
	oo.print("; key is ");
	oo.print(ent1.position());
	oo.print("; and value is ");
	oo.print(ent1.value());
	oo.print("; next is ");
	oo.print(ent1.fetchNext());
	oo.print("\n"+
"");
	oo.print("\n"+
"test match of ent1 with ");
	oo.print((Sequence.string("one")));
	oo.print(": ");
	if (ent1.match((Sequence.string("one")))) {
		oo.print("TRUE");
	}
	else {
		oo.print("FALSE");
	}
	oo.print("\n"+
"test match of ent1 with ");
	oo.print((Sequence.string("two")));
	oo.print(": ");
	if (ent1.match((Sequence.string("two")))) {
		oo.print("TRUE");
	}
	else {
		oo.print("FALSE");
	}
	oo.print("\n"+
"test matchValue of ent1 with ");
	oo.print((Sequence.string("one")));
	oo.print(": ");
	if (ent1.matchValue((Sequence.string("one")))) {
		oo.print("TRUE");
	}
	else {
		oo.print("FALSE");
	}
	oo.print("\n"+
"test matchValue of ent1 with ");
	oo.print((Sequence.string("two")));
	oo.print(": ");
	if (ent1.matchValue((Sequence.string("two")))) {
		oo.print("TRUE");
	}
	else {
		oo.print("FALSE");
	}
	oo.print("\n"+
"end of test one\n"+
"\n"+
"");
/*
udanax-top.st:61948:TableEntryTester methodsFor: 'testing'!
{void} test1on: oo {ostream reference}
	| ent1 {TableEntry} |
	oo << 'start of test 1 - basic stuff
'.
	ent1 _ TableEntry make: (Sequence string: 'one') with: (Sequence string: 'two').
	oo << '
ent1 == ' << ent1 << '; key is ' << ent1 position << '; and value is ' << ent1 value <<
	'; next is ' << ent1 fetchNext << '
'.
	oo << '
test match of ent1 with ' << (Sequence string: 'one') << ': '.
	(ent1 match: (Sequence string: 'one'))
		ifTrue: [oo << 'TRUE']
		ifFalse: [oo << 'FALSE'].
	oo << '
test match of ent1 with ' << (Sequence string: 'two') << ': '.
	(ent1 match: (Sequence string: 'two'))
		ifTrue: [oo << 'TRUE']
		ifFalse: [oo << 'FALSE'].
	oo << '
test matchValue of ent1 with ' << (Sequence string: 'one') << ': '.
	(ent1 matchValue: (Sequence string: 'one'))
		ifTrue: [oo << 'TRUE']
		ifFalse: [oo << 'FALSE'].
	oo << '
test matchValue of ent1 with ' << (Sequence string: 'two') << ': '.
	(ent1 matchValue: (Sequence string: 'two'))
		ifTrue: [oo << 'TRUE']
		ifFalse: [oo << 'FALSE'].
	oo << '
end of test one
'!
*/
}
public void test2on(PrintWriter oo) {
	TableEntry ent1;
	TableEntry ent2;
	TableEntry ent3;
	oo.print("start of test 2 - linking stuff\n"+
"");
	ent1 = TableEntry.make((Sequence.string("one")), (Sequence.string("value")));
	oo.print("\n"+
"ent1 == ");
	oo.print(ent1);
	oo.print("; key is ");
	oo.print(ent1.position());
	oo.print("; and value is ");
	oo.print(ent1.value());
	oo.print("; next is ");
	oo.print(ent1.fetchNext());
	oo.print("\n"+
"");
	ent2 = TableEntry.make((Sequence.string("two")), (Sequence.string("value")));
	oo.print("\n"+
"ent2 == ");
	oo.print(ent2);
	oo.print("; key is ");
	oo.print(ent2.position());
	oo.print("; and value is ");
	oo.print(ent2.value());
	oo.print("; next is ");
	oo.print(ent2.fetchNext());
	oo.print("\n"+
"");
	ent3 = TableEntry.make((Sequence.string("three")), (Sequence.string("value")));
	oo.print("\n"+
"ent3 == ");
	oo.print(ent3);
	oo.print("; key is ");
	oo.print(ent3.position());
	oo.print("; and value is ");
	oo.print(ent3.value());
	oo.print("; next is ");
	oo.print(ent3.fetchNext());
	oo.print("\n"+
"");
	ent1.setNext(ent2);
	ent2.setNext(ent3);
	oo.print("ent1 next now: ");
	oo.print(ent1.fetchNext());
	oo.print("\n"+
"ent2 next now: ");
	oo.print(ent2.fetchNext());
	oo.print("\n"+
"ent3 next now: ");
	oo.print(ent3.fetchNext());
	oo.print("\n"+
"");
	/* oo << 'step over chain:
'.
	ent1 stepper forEach: [:ent {TableEntry} |
		oo << 'entry is ' << ent << '
']. */
/*
udanax-top.st:61983:TableEntryTester methodsFor: 'testing'!
{void} test2on: oo {ostream reference}
	| ent1 {TableEntry} ent2 {TableEntry} ent3 {TableEntry} |
	oo << 'start of test 2 - linking stuff
'.
	ent1 _ TableEntry make: (Sequence string: 'one') with: (Sequence string: 'value').
	oo << '
ent1 == ' << ent1 << '; key is ' << ent1 position << '; and value is ' << ent1 value <<
	'; next is ' << ent1 fetchNext << '
'.
	ent2 _ TableEntry make: (Sequence string: 'two') with: (Sequence string: 'value').
	oo << '
ent2 == ' << ent2 << '; key is ' << ent2 position << '; and value is ' << ent2 value <<
	'; next is ' << ent2 fetchNext << '
'.
	ent3 _ TableEntry make: (Sequence string: 'three') with: (Sequence string: 'value').
	oo << '
ent3 == ' << ent3 << '; key is ' << ent3 position << '; and value is ' << ent3 value <<
	'; next is ' << ent3 fetchNext << '
'.
	ent1 setNext: ent2.
	ent2 setNext: ent3.
	oo << 'ent1 next now: ' << ent1 fetchNext.
	oo << '
ent2 next now: ' << ent2 fetchNext.
	oo << '
ent3 next now: ' << ent3 fetchNext << '
'.
	"oo << 'step over chain:
'.
	ent1 stepper forEach: [:ent {TableEntry} |
		oo << 'entry is ' << ent << '
']."!
*/
}
public void test3on(PrintWriter oo) {
	TableEntry ent1;
	TableEntry ent2;
	TableEntry ent3;
	TableEntry ent4;
	oo.print("start of test 2 - different entry types\n"+
"");
	ent1 = TableEntry.make((Sequence.string("one")), (Sequence.string("value")));
	oo.print("\n"+
"ent1 == ");
	oo.print(ent1);
	oo.print("; key is ");
	oo.print(ent1.position());
	oo.print("; and value is ");
	oo.print(ent1.value());
	oo.print("; next is ");
	oo.print(ent1.fetchNext());
	oo.print("\n"+
"");
	ent2 = TableEntry.make((IntegerPos.make(1)), (Sequence.string("value")));
	oo.print("\n"+
"ent2 == ");
	oo.print(ent2);
	oo.print("; key is ");
	oo.print(ent2.position());
	oo.print("; and value is ");
	oo.print(ent2.value());
	oo.print("; next is ");
	oo.print(ent2.fetchNext());
	oo.print("\n"+
"");
	ent2 = TableEntry.makeIntegerVar(1, (Sequence.string("value")));
	oo.print("\n"+
"ent2 == ");
	oo.print(ent2);
	oo.print("; key is ");
	oo.print(ent2.position());
	oo.print("; and value is ");
	oo.print(ent2.value());
	oo.print("; next is ");
	oo.print(ent2.fetchNext());
	oo.print("\n"+
"");
	ent3 = TableEntry.make((HeaperAsPosition.make((Sequence.string("three")))), (Sequence.string("three")));
	oo.print("\n"+
"ent3 == ");
	oo.print(ent3);
	oo.print("; key is ");
	oo.print(ent3.position());
	oo.print("; and value is ");
	oo.print(ent3.value());
	oo.print("; next is ");
	oo.print(ent3.fetchNext());
	oo.print("\n"+
"");
	ent4 = TableEntry.make(IntegerPos.make((Sequence.string("value")).hashForEqual()), (Sequence.string("value")));
	oo.print("\n"+
"ent4 == ");
	oo.print(ent4);
	oo.print("; key is ");
	oo.print(ent4.position());
	oo.print("; and value is ");
	oo.print(ent4.value());
	oo.print("; next is ");
	oo.print(ent4.fetchNext());
	oo.print("\n"+
"");
	ent4 = TableEntry.makeIntegerVar((Sequence.string("value")).hashForEqual(), (Sequence.string("value")));
	oo.print("\n"+
"ent4 == ");
	oo.print(ent4);
	oo.print("; key is ");
	oo.print(ent4.position());
	oo.print("; and value is ");
	oo.print(ent4.value());
	oo.print("; next is ");
	oo.print(ent4.fetchNext());
	oo.print("\n"+
"");
/*
udanax-top.st:62017:TableEntryTester methodsFor: 'testing'!
{void} test3on: oo {ostream reference}
	| ent1 {TableEntry} ent2 {TableEntry} ent3 {TableEntry} ent4 {TableEntry} |
	[IntegerPos] USES.
	oo << 'start of test 2 - different entry types
'.
	ent1 _ TableEntry make: (Sequence string: 'one') with: (Sequence string: 'value').
	oo << '
ent1 == ' << ent1 << '; key is ' << ent1 position << '; and value is ' << ent1 value <<
	'; next is ' << ent1 fetchNext << '
'.
	ent2 _ TableEntry make: (1 integer) with: (Sequence string: 'value').
	oo << '
ent2 == ' << ent2 << '; key is ' << ent2 position << '; and value is ' << ent2 value <<
	'; next is ' << ent2 fetchNext << '
'.
	ent2 _ TableEntry make.IntegerVar: 1 with: (Sequence string: 'value').
	oo << '
ent2 == ' << ent2 << '; key is ' << ent2 position << '; and value is ' << ent2 value <<
	'; next is ' << ent2 fetchNext << '
'.
	ent3 _ TableEntry make: (HeaperAsPosition make: (Sequence string: 'three')) with: (Sequence string: 'three').
	oo << '
ent3 == ' << ent3 << '; key is ' << ent3 position << '; and value is ' << ent3 value <<
	'; next is ' << ent3 fetchNext << '
'.
	ent4 _ TableEntry make: (Sequence string: 'value') hashForEqual integer with: (Sequence string: 'value').
	oo << '
ent4 == ' << ent4 << '; key is ' << ent4 position << '; and value is ' << ent4 value <<
	'; next is ' << ent4 fetchNext << '
'.
	ent4 _ TableEntry make.IntegerVar: (Sequence string: 'value') hashForEqual with: (Sequence string: 'value').
	oo << '
ent4 == ' << ent4 << '; key is ' << ent4 position << '; and value is ' << ent4 value <<
	'; next is ' << ent4 fetchNext << '
'.!
*/
}
public TableEntryTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:62056:TableEntryTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:62059:TableEntryTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public TableEntryTester() {
/*

Generated during transformation
*/
}
}
