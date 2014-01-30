/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.testing;

import info.dgjones.abora.gold.collection.sets.ActualHashSet;
import info.dgjones.abora.gold.collection.sets.HashSet;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.set.SHTO;
import info.dgjones.abora.gold.testing.HashSetTester;
import info.dgjones.abora.gold.testing.MuSetTester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class HashSetTester extends MuSetTester {

/*
udanax-top.st:60681:
MuSetTester subclass: #HashSetTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Testing'!
*/
/*
udanax-top.st:60685:
(HashSetTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HashSetTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public void basicTestsOn(PrintWriter oo) {
	HashSet set1;
	HashSet set2;
	HashSet set3;
	HashSet set4;
	HashSet set5;
	oo.print("\n"+
"basic tests -- start with creation.\n"+
"");
	set1 = (HashSet) HashSet.make();
	set2 = (HashSet) HashSet.makeHeaper((SHTO.make("heaper", 1193046)));
	set3 = (HashSet) HashSet.makeIntegerVar(4);
	set4 = (HashSet) HashSet.makeIntegerVar(19);
	set5 = (HashSet) HashSet.makeIntegerVar(0);
	oo.print("basic hash set is: ");
	oo.print(set1);
	oo.print(" internals:\n"+
"");
	set1.printInternals(oo);
	oo.print("\n"+
"basic set on heaper is: ");
	oo.print(set2);
	oo.print(" internals:\n"+
"");
	set2.printInternals(oo);
	oo.print("\n"+
"basic set make(4) is: ");
	oo.print(set3);
	oo.print(" internals:\n"+
"");
	set3.printInternals(oo);
	oo.print("\n"+
"basic set make(19) is: ");
	oo.print(set4);
	oo.print(" internals:\n"+
"");
	set4.printInternals(oo);
	oo.print("\n"+
"basic set make(0) is: ");
	oo.print(set5);
	oo.print(" internals:\n"+
"");
	set5.printInternals(oo);
	set1.destroy();
	set3.destroy();
	set4.destroy();
	set5.destroy();
	set1 = (HashSet) (set2.copy());
	oo.print("copy of ");
	oo.print(set2);
	oo.print(" is ");
	oo.print(set1);
	oo.print("\n"+
"internals of source:\n"+
"");
	set2.printInternals(oo);
	oo.print("\n"+
"internals of copy:\n"+
"");
	set1.printInternals(oo);
	oo.print("\n"+
"\n"+
"End of basic tests\n"+
"");
/*
udanax-top.st:60690:HashSetTester methodsFor: 'tests'!
{void} basicTestsOn: oo {ostream reference} 
	| set1 {HashSet} set2 {HashSet} set3 {HashSet} set4 {HashSet}
	 set5 {HashSet} |
	oo << '
basic tests -- start with creation.
'.
	set1 _ HashSet make.
	set2 _ HashSet make.Heaper: (SHTO make: 'heaper' with:   1193046).
	set3 _ HashSet make.IntegerVar: 4.
	set4 _ HashSet make.IntegerVar: 19.
	set5 _ HashSet make.IntegerVar: IntegerVar0.
	oo << 'basic hash set is: ' << set1 << ' internals:
'.
	set1 printInternals: oo.
	oo << '
basic set on heaper is: ' << set2 << ' internals:
'.
	set2 printInternals: oo.
	oo << '
basic set make(4) is: ' << set3 << ' internals:
'.
	set3 printInternals: oo.
	oo << '
basic set make(19) is: ' << set4 << ' internals:
'.
	set4 printInternals: oo.
	oo << '
basic set make(0) is: ' << set5 << ' internals:
'.
	set5 printInternals: oo.
	set1 destroy.
	set3 destroy.
	set4 destroy.
	set5 destroy.
	
	set1 _ (set2 copy) cast: HashSet.
	oo << 'copy of ' << set2 << ' is ' << set1.
	oo << '
internals of source:
'.
	set2 printInternals: oo.
	oo << '
internals of copy:
'.
	set1 printInternals: oo.
	oo << '
End of basic tests
'!
*/
}
public void doIntroduceTestsOn(PrintWriter oo) {
	HashSet set1;
	oo.print("\n"+
"introduce tests\n"+
"");
	introduceTestsOn(oo, HashSet.make(), (SHTO.make("first", 0)));
	introduceTestsOn(oo, HashSet.make(), (SHTO.make("second", 1)));
	introduceTestsOn(oo, HashSet.make(), (SHTO.make("third", 6)));
	introduceTestsOn(oo, HashSet.make(), (SHTO.make("fourth", 7)));
	set1 = (HashSet) HashSet.make();
	introduceTestsOn(oo, set1, (SHTO.make("one", 1)));
	introduceTestsOn(oo, set1, (SHTO.make("two", 1)));
	introduceTestsOn(oo, set1, (SHTO.make("three", 1)));
	introduceTestsOn(oo, set1, (SHTO.make("one", 1)));
	set1.destroy();
	set1 = (HashSet) HashSet.make();
	introduceTestsOn(oo, set1, (SHTO.make("one", 1)));
	introduceTestsOn(oo, set1, (SHTO.make("two", 1)));
	introduceTestsOn(oo, set1, (SHTO.make("three", 1)));
	introduceTestsOn(oo, set1, (SHTO.make("fower", 2)));
	set1.destroy();
	oo.print("\n"+
"\n"+
"End of introduce tests\n"+
"");
/*
udanax-top.st:60743:HashSetTester methodsFor: 'tests'!
{void} doIntroduceTestsOn: oo {ostream reference} 
	| set1 {HashSet} |
	oo << '
introduce tests
'.
	self introduceTestsOn: oo with: HashSet make with: (SHTO make: 'first' with: Int32Zero).
	self introduceTestsOn: oo with: HashSet make with: (SHTO make: 'second' with: 1).
	self introduceTestsOn: oo with: HashSet make with: (SHTO make: 'third' with: 6).
	self introduceTestsOn: oo with: HashSet make with: (SHTO make: 'fourth' with: 7).
	
	set1 _ HashSet make.
	self introduceTestsOn: oo with: set1 with: (SHTO make: 'one' with: 1).
	self introduceTestsOn: oo with: set1 with: (SHTO make: 'two' with: 1).
	self introduceTestsOn: oo with: set1 with: (SHTO make: 'three' with: 1).
	self introduceTestsOn: oo with: set1 with: (SHTO make: 'one' with: 1).
	
	set1 destroy.
	set1 _ HashSet make.
	self introduceTestsOn: oo with: set1 with: (SHTO make: 'one' with: 1).
	self introduceTestsOn: oo with: set1 with: (SHTO make: 'two' with: 1).
	self introduceTestsOn: oo with: set1 with: (SHTO make: 'three' with: 1).
	self introduceTestsOn: oo with: set1 with: (SHTO make: 'fower' with: 2).
	set1 destroy.
	
	oo << '
End of introduce tests
'!
*/
}
public void doStoreTestsOn(PrintWriter oo) {
	HashSet set1;
	HashSet set2;
	oo.print("\n"+
"store tests\n"+
"");
	storeTestsOn(oo, HashSet.make(), (SHTO.make("first", 0)));
	storeTestsOn(oo, HashSet.make(), (SHTO.make("second", 1)));
	storeTestsOn(oo, HashSet.make(), (SHTO.make("third", 6)));
	storeTestsOn(oo, HashSet.make(), (SHTO.make("fourth", 7)));
	set1 = (HashSet) HashSet.make();
	storeTestsOn(oo, set1, (SHTO.make("one", 1)));
	storeTestsOn(oo, set1, (SHTO.make("two", 1)));
	storeTestsOn(oo, set1, (SHTO.make("three", 1)));
	storeTestsOn(oo, set1, (SHTO.make("one", 1)));
	oo.print("testing storeAll\n"+
"\n"+
"");
	set2 = (HashSet) HashSet.make();
	set2.store((SHTO.make("duble", 2)));
	set2.store((SHTO.make("triple", 3)));
	storeTestsOn(oo, set2, (SHTO.make("quadle", 4)));
	storeTestsOn(oo, set1, (SHTO.make("onele", 1)));
	set1.storeAll(set2);
	oo.print("after storeAll, set1 now:\n"+
"");
	set1.printInternals(oo);
	oo.print("after storeAll, set2 now:\n"+
"");
	set2.printInternals(oo);
	oo.print("\n"+
"\n"+
"End of store tests\n"+
"");
/*
udanax-top.st:60773:HashSetTester methodsFor: 'tests'!
{void} doStoreTestsOn: oo {ostream reference} 
	| set1 {HashSet} set2 {HashSet} |
	oo << '
store tests
'.
	self storeTestsOn: oo with: HashSet make with: (SHTO make: 'first' with: Int32Zero).
	self storeTestsOn: oo with: HashSet make with: (SHTO make: 'second' with: 1).
	self storeTestsOn: oo with: HashSet make with: (SHTO make: 'third' with: 6).
	self storeTestsOn: oo with: HashSet make with: (SHTO make: 'fourth' with: 7).
	
	set1 _ HashSet make.
	self storeTestsOn: oo with: set1 with: (SHTO make: 'one' with: 1).
	self storeTestsOn: oo with: set1 with: (SHTO make: 'two' with: 1).
	self storeTestsOn: oo with: set1 with: (SHTO make: 'three' with: 1).
	self storeTestsOn: oo with: set1 with: (SHTO make: 'one' with: 1).
	
	oo << 'testing storeAll
'.
	set2 _ HashSet make.
	set2 store: (SHTO make: 'duble' with: 2).
	set2 store: (SHTO make: 'triple' with: 3).
	self storeTestsOn: oo with: set2 with: (SHTO make: 'quadle' with: 4).
	self storeTestsOn: oo with: set1 with: (SHTO make: 'onele' with: 1).
	set1 storeAll: set2.
	oo << 'after storeAll, set1 now:
'.
	set1 printInternals: oo.
	oo << 'after storeAll, set2 now:
'.
	set2 printInternals: oo.
	
	oo << '
End of store tests
'!
*/
}
public void doWipeTestsOn(PrintWriter oo) {
	HashSet set1;
	HashSet set2;
	oo.print("\n"+
"wipe tests\n"+
"");
	set1 = (HashSet) HashSet.make();
	set1.store((SHTO.make("one", 0)));
	wipeTestsOn(oo, set1, (SHTO.make("one", 0)));
	set1.destroy();
	set1 = (HashSet) HashSet.make();
	set1.store((SHTO.make("two", 1)));
	wipeTestsOn(oo, set1, (SHTO.make("two", 1)));
	set1.destroy();
	set1 = (HashSet) HashSet.make();
	set1.store((SHTO.make("three", 6)));
	wipeTestsOn(oo, set1, (SHTO.make("three", 6)));
	set1.destroy();
	set1 = (HashSet) HashSet.make();
	set1.store((SHTO.make("four", 7)));
	wipeTestsOn(oo, set1, (SHTO.make("four", 7)));
	set1.destroy();
	set1 = (HashSet) HashSet.make();
	set1.store((SHTO.make("one", 1)));
	set1.store((SHTO.make("two", 1)));
	set1.store((SHTO.make("three", 1)));
	set1.store((SHTO.make("fower", 2)));
	set2 = (HashSet) (set1.copy());
	wipeTestsOn(oo, set2, (SHTO.make("one", 1)));
	wipeTestsOn(oo, set2, (SHTO.make("two", 1)));
	wipeTestsOn(oo, set2, (SHTO.make("three", 1)));
	wipeTestsOn(oo, set2, (SHTO.make("fower", 2)));
	set2.destroy();
	set2 = (HashSet) (set1.copy());
	wipeTestsOn(oo, set2, (SHTO.make("three", 1)));
	wipeTestsOn(oo, set2, (SHTO.make("two", 1)));
	wipeTestsOn(oo, set2, (SHTO.make("one", 1)));
	wipeTestsOn(oo, set2, (SHTO.make("fower", 2)));
	set2.destroy();
	set2 = (HashSet) (set1.copy());
	wipeTestsOn(oo, set2, (SHTO.make("fower", 2)));
	wipeTestsOn(oo, set2, (SHTO.make("three", 1)));
	wipeTestsOn(oo, set2, (SHTO.make("two", 1)));
	wipeTestsOn(oo, set2, (SHTO.make("one", 1)));
	set2.destroy();
	oo.print("\n"+
"\n"+
"End of wipe tests\n"+
"");
/*
udanax-top.st:60811:HashSetTester methodsFor: 'tests'!
{void} doWipeTestsOn: oo {ostream reference} 
	| set1 {HashSet} set2 {HashSet} |
	oo << '
wipe tests
'.
	set1 _ HashSet make.
	set1 store: (SHTO make: 'one' with: Int32Zero).
	self wipeTestsOn: oo with: set1 with: (SHTO make: 'one' with: Int32Zero).
	set1 destroy.
	
	set1 _ HashSet make.
	set1 store: (SHTO make: 'two' with: 1).
	self wipeTestsOn: oo with: set1 with: (SHTO make: 'two' with: 1).
	set1 destroy.
	
	set1 _ HashSet make.
	set1 store: (SHTO make: 'three' with: 6).
	self wipeTestsOn: oo with: set1 with: (SHTO make: 'three' with: 6).
	set1 destroy.
	
	set1 _ HashSet make.
	set1 store: (SHTO make: 'four' with: 7).
	self wipeTestsOn: oo with: set1 with: (SHTO make: 'four' with: 7).
	set1 destroy.
	
	set1 _ HashSet make.
	set1 store: (SHTO make: 'one' with: 1).
	set1 store: (SHTO make: 'two' with: 1).
	set1 store: (SHTO make: 'three' with: 1).
	set1 store: (SHTO make: 'fower' with: 2).
	
	set2 _ (set1 copy) cast: HashSet.
	self wipeTestsOn: oo with: set2 with: (SHTO make: 'one' with: 1).
	self wipeTestsOn: oo with: set2 with: (SHTO make: 'two' with: 1).
	self wipeTestsOn: oo with: set2 with: (SHTO make: 'three' with: 1).
	self wipeTestsOn: oo with: set2 with: (SHTO make: 'fower' with: 2).
	set2 destroy.
	set2 _ (set1 copy) cast: HashSet.
	self wipeTestsOn: oo with: set2 with: (SHTO make: 'three' with: 1).
	self wipeTestsOn: oo with: set2 with: (SHTO make: 'two' with: 1).
	self wipeTestsOn: oo with: set2 with: (SHTO make: 'one' with: 1).
	self wipeTestsOn: oo with: set2 with: (SHTO make: 'fower' with: 2).
	set2 destroy.
	set2 _ (set1 copy) cast: HashSet.
	self wipeTestsOn: oo with: set2 with: (SHTO make: 'fower' with: 2).
	self wipeTestsOn: oo with: set2 with: (SHTO make: 'three' with: 1).
	self wipeTestsOn: oo with: set2 with: (SHTO make: 'two' with: 1).
	self wipeTestsOn: oo with: set2 with: (SHTO make: 'one' with: 1).
	set2 destroy.
	
	oo << '
End of wipe tests
'!
*/
}
public void testBig(PrintWriter oo) {
	HashSet big1;
	HashSet big2;
	HashSet big3;
	oo.print("\n"+
"	\n"+
"start of big testing.\n"+
" \n"+
"");
	big1 = (HashSet) HashSet.make();
	for (int i = 10000; i >= 1; i -= 1 ) {
		big1.introduce((SHTO.make("some object", i)));
	}
	oo.print("new big set (count ");
	oo.print(big1.count());
	oo.print(") is:\n"+
"");
	oo.print(big1);
	oo.print("\n"+
"");
	big2 = (HashSet) (big1.copy());
	oo.print("\n"+
"	\n"+
"\n"+
"big2 is a copy of big1 (count ");
	oo.print(big2.count());
	oo.print(")\n"+
"\n"+
"");
	if (big2.isSubsetOf(big1)) {
		oo.print("big2 is a subset of big1\n"+
"");
	}
	else {
		oo.print("big2 is NOT a subset of big1\n"+
"");
	}
	if (big1.isSubsetOf(big2)) {
		oo.print("big1 is a subset of big2\n"+
"");
	}
	else {
		oo.print("big1 is NOT a subset of big2\n"+
"");
	}
	for (int j = 3; j <= 9999; j += 3 ) {
		big2.remove((SHTO.make("some object", j)));
	}
	oo.print("\n"+
"big2 now has every third element removed (count ");
	oo.print(big2.count());
	oo.print(")\n"+
"\n"+
"");
	if (big2.isSubsetOf(big1)) {
		oo.print("big2 is a subset of big1\n"+
"");
	}
	else {
		oo.print("big2 is NOT a subset of big1\n"+
"");
	}
	if (big1.isSubsetOf(big2)) {
		oo.print("big1 is a subset of big2\n"+
"");
	}
	else {
		oo.print("big1 is NOT a subset of big2\n"+
"");
	}
	big3 = (HashSet) big1.copy();
	big3.wipeAll(big2);
	oo.print("big3 is big1-big2 - count ");
	oo.print(big3.count());
	oo.print("\n"+
"\n"+
"");
	if (big3.isSubsetOf(big1)) {
		oo.print("big3 is a subset of big1\n"+
"");
	}
	else {
		oo.print("big3 is NOT a subset of big1\n"+
"");
	}
	if (big1.isSubsetOf(big3)) {
		oo.print("big1 is a subset of big3\n"+
"");
	}
	else {
		oo.print("big1 is NOT a subset of big3\n"+
"");
	}
	if (big3.isSubsetOf(big2)) {
		oo.print("big3 is a subset of big2\n"+
"");
	}
	else {
		oo.print("big3 is NOT a subset of big2\n"+
"");
	}
	if (big2.isSubsetOf(big3)) {
		oo.print("big2 is a subset of big3\n"+
"");
	}
	else {
		oo.print("big2 is NOT a subset of big3\n"+
"");
	}
	oo.print("\n"+
"\n"+
"end of bigset testing\n"+
"");
	big1.destroy();
	big2.destroy();
	big3.destroy();
/*
udanax-top.st:60869:HashSetTester methodsFor: 'tests'!
{void} testBig: oo {ostream reference}
	| big1 {HashSet} big2 {HashSet} big3 {HashSet} |
	
	oo << '
	
start of big testing.
 
'. 
	big1 _ HashSet make.
	10000 downTo: 1 do: [:i {UInt32} | 
		big1 introduce: (SHTO make: 'some object' with: i)].
	oo << 'new big set (count ' << big1 count << ') is:
' << big1 << '
'.
	big2 _ (big1 copy) cast: HashSet.
	
	oo << '
	
big2 is a copy of big1 (count ' << big2 count << ')
'.
	(big2 isSubsetOf: big1)
		ifTrue: [oo << 'big2 is a subset of big1
']
		ifFalse: [oo << 'big2 is NOT a subset of big1
'].
	(big1 isSubsetOf: big2)
		ifTrue: [oo << 'big1 is a subset of big2
']
		ifFalse: [oo << 'big1 is NOT a subset of big2
'].
	3 to: 9999 by: 3 do: [:j {UInt32} |
		big2 remove: (SHTO make: 'some object' with: j)].
	oo << '
big2 now has every third element removed (count ' << big2 count << ')
'.
	(big2 isSubsetOf: big1)
		ifTrue: [oo << 'big2 is a subset of big1
']
		ifFalse: [oo << 'big2 is NOT a subset of big1
'].
	(big1 isSubsetOf: big2)
		ifTrue: [oo << 'big1 is a subset of big2
']
		ifFalse: [oo << 'big1 is NOT a subset of big2
'].
	big3 _ big1 copy cast: HashSet.
	big3 wipeAll: big2.
	oo << 'big3 is big1-big2 - count ' << big3 count << '
'.
	(big3 isSubsetOf: big1)
		ifTrue: [oo << 'big3 is a subset of big1
']
		ifFalse: [oo << 'big3 is NOT a subset of big1
'].
	(big1 isSubsetOf: big3)
		ifTrue: [oo << 'big1 is a subset of big3
']
		ifFalse: [oo << 'big1 is NOT a subset of big3
'].
	(big3 isSubsetOf: big2)
		ifTrue: [oo << 'big3 is a subset of big2
']
		ifFalse: [oo << 'big3 is NOT a subset of big2
'].
	(big2 isSubsetOf: big3)
		ifTrue: [oo << 'big2 is a subset of big3
']
		ifFalse: [oo << 'big2 is NOT a subset of big3
'].
	oo << '
end of bigset testing
'.
	big1 destroy.
	big2 destroy.
	big3 destroy!
*/
}
/**
 * HashSetTester runTest
 */
public void allTestsOn(PrintWriter oo) {
	oo.print("HashSet testing\n"+
"");
	super.allTestsOn(oo);
	basicTestsOn(oo);
	doIntroduceTestsOn(oo);
	doStoreTestsOn(oo);
	doWipeTestsOn(oo);
	testBig(oo);
	oo.print("\n"+
"End of black box testing - now for the White!\n"+
"\n"+
"");
	oo.print("\n"+
"End of HashSet testing\n"+
"\n"+
"");
/*
udanax-top.st:60954:HashSetTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference} 
	"HashSetTester runTest"
	oo << 'HashSet testing
'.
	super allTestsOn: oo.
	self basicTestsOn: oo.
	self doIntroduceTestsOn: oo.
	self doStoreTestsOn: oo.
	self doWipeTestsOn: oo.
	self testBig: oo.
	oo << '
End of black box testing - now for the White!!
'.
	oo << '
End of HashSet testing
'!
*/
}
public void introduceTestsOn(PrintWriter oo, HashSet set1, SHTO object) {
	oo.print("set1 starts as: ");
	oo.print(set1);
	oo.print(" internals:\n"+
"");
	set1.printInternals(oo);
	oo.print("set1 introduce: ");
	oo.print(object);
	oo.print("\n"+
"");
	try {
		set1.introduce(object);
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.ALREADY_IN_SET.equals(ex.getMessage())) {
			oo.print("\n"+
"PROBLEM: ");
			oo.print(object);
			oo.print(" is already in set!\n"+
"");
			return ;
		}
		else {
			throw ex;
		}
	}
	oo.print("set1 now: ");
	oo.print(set1);
	oo.print(" internals:\n"+
"");
	set1.printInternals(oo);
	Stepper stomper = set1.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Heaper item = (Heaper) stomper.fetch();
		if (item == null) {
			continue ;
		}
		if ( ! (set1.hasMember(item))) {
			oo.print("PROBLEM: item ");
			oo.print(item);
			oo.print(" was not found in set!\n"+
"");
			return ;
		}
	}
	stomper.destroy();
/*
udanax-top.st:60976:HashSetTester methodsFor: 'test support'!
{void} introduceTestsOn: oo {ostream reference} with: set1 {HashSet} with: object {SHTO}
	oo << 'set1 starts as: ' << set1 << ' internals:
'.
	set1 printInternals: oo.
	oo << 'set1 introduce: ' << object << '
'.
	MuSet problems.AlreadyInSet handle: [:ex | oo << '
PROBLEM: ' << object << ' is already in set!!
'. ^VOID]
		do: [set1 introduce: object].
		
	oo << 'set1 now: ' << set1 << ' internals:
'.
	set1 printInternals: oo.
	set1 stepper forEach: [:item {Heaper} |
		(set1 hasMember: item)
			ifFalse: [oo << 'PROBLEM: item ' << item << ' was not found in set!!
'. ^ VOID]]!
*/
}
public void removeTestsOn(PrintWriter oo, HashSet set1, SHTO object) {
	oo.print("set1 starts as: ");
	oo.print(set1);
	oo.print(" internals:\n"+
"");
	set1.printInternals(oo);
	oo.print("set1 remove: ");
	oo.print(object);
	oo.print("\n"+
"");
	try {
		set1.remove(object);
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.NOT_IN_SET.equals(ex.getMessage())) {
			oo.print("\n"+
"PROBLEM: ");
			oo.print(object);
			oo.print(" is not in the set!\n"+
"");
			return ;
		}
		else {
			throw ex;
		}
	}
	oo.print("set1 now: ");
	oo.print(set1);
	oo.print(" internals:\n"+
"");
	set1.printInternals(oo);
	Stepper stomper = set1.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Heaper item = (Heaper) stomper.fetch();
		if (item == null) {
			continue ;
		}
		if ( ! (set1.hasMember(item))) {
			oo.print("PROBLEM: item ");
			oo.print(item);
			oo.print(" was not found in set!\n"+
"");
			return ;
		}
	}
	stomper.destroy();
/*
udanax-top.st:60997:HashSetTester methodsFor: 'test support'!
{void} removeTestsOn: oo {ostream reference} with: set1 {HashSet} with: object {SHTO}
	oo << 'set1 starts as: ' << set1 << ' internals:
'.
	set1 printInternals: oo.
	oo << 'set1 remove: ' << object << '
'.
	MuSet problems.NotInSet handle: [:ex | oo << '
PROBLEM: ' << object << ' is not in the set!!
'. ^ VOID]
		do: [set1 remove: object].
		
	oo << 'set1 now: ' << set1 << ' internals:
'.
	set1 printInternals: oo.
	set1 stepper forEach: [:item {Heaper} |
		(set1 hasMember: item)
			ifFalse: [oo << 'PROBLEM: item ' << item << ' was not found in set!!
'. ^ VOID]]!
*/
}
public void storeTestsOn(PrintWriter oo, HashSet set1, SHTO object) {
	oo.print("set1 starts as: ");
	oo.print(set1);
	oo.print(" internals:\n"+
"");
	set1.printInternals(oo);
	oo.print("set1 store: ");
	oo.print(object);
	oo.print("\n"+
"");
	set1.store(object);
	oo.print("set1 now: ");
	oo.print(set1);
	oo.print(" internals:\n"+
"");
	set1.printInternals(oo);
	Stepper stomper = set1.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Heaper item = (Heaper) stomper.fetch();
		if (item == null) {
			continue ;
		}
		if ( ! (set1.hasMember(item))) {
			oo.print("PROBLEM: item ");
			oo.print(item);
			oo.print(" was not found in set!\n"+
"");
			return ;
		}
	}
	stomper.destroy();
/*
udanax-top.st:61018:HashSetTester methodsFor: 'test support'!
{void} storeTestsOn: oo {ostream reference} with: set1 {HashSet} with: object {SHTO}
	oo << 'set1 starts as: ' << set1 << ' internals:
'.
	set1 printInternals: oo.
	oo << 'set1 store: ' << object << '
'.
	set1 store: object.
		
	oo << 'set1 now: ' << set1 << ' internals:
'.
	set1 printInternals: oo.
	set1 stepper forEach: [:item {Heaper} |
		(set1 hasMember: item)
			ifFalse: [oo << 'PROBLEM: item ' << item << ' was not found in set!!
'. ^ VOID]]!
*/
}
public void wipeTestsOn(PrintWriter oo, HashSet set1, SHTO object) {
	oo.print("set1 starts as: ");
	oo.print(set1);
	oo.print(" internals:\n"+
"");
	set1.printInternals(oo);
	oo.print("set1 wipe: ");
	oo.print(object);
	oo.print("\n"+
"");
	set1.wipe(object);
	oo.print("set1 now: ");
	oo.print(set1);
	oo.print(" internals:\n"+
"");
	set1.printInternals(oo);
	Stepper stomper = set1.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Heaper item = (Heaper) stomper.fetch();
		if (item == null) {
			continue ;
		}
		if ( ! (set1.hasMember(item))) {
			oo.print("PROBLEM: item ");
			oo.print(item);
			oo.print(" was not found in set!\n"+
"");
			return ;
		}
	}
	stomper.destroy();
/*
udanax-top.st:61036:HashSetTester methodsFor: 'test support'!
{void} wipeTestsOn: oo {ostream reference} with: set1 {HashSet} with: object {SHTO}
	oo << 'set1 starts as: ' << set1 << ' internals:
'.
	set1 printInternals: oo.
	oo << 'set1 wipe: ' << object << '
'.
	set1 wipe: object.
		
	oo << 'set1 now: ' << set1 << ' internals:
'.
	set1 printInternals: oo.
	set1 stepper forEach: [:item {Heaper} |
		(set1 hasMember: item)
			ifFalse: [oo << 'PROBLEM: item ' << item << ' was not found in set!!
'. ^ VOID]]!
*/
}
/**
 * ^ MuSet make		No - we're testing ActualHashSets, not MuSets.
 */
public ScruSet generateSet() {
	return ActualHashSet.make();
/*
udanax-top.st:61056:HashSetTester methodsFor: 'accessing'!
{ScruSet} generateSet
	"^ MuSet make		No - we're testing ActualHashSets, not MuSets."
	
	^ ActualHashSet make!
*/
}
public ScruSet generateSetContaining(Stepper stuff) {
	MuSet t;
	/* t _ MuSet make.			No - we're testing ActualHashSets, not MuSets. */
	t = ActualHashSet.make();
	Stepper stomper = stuff;
	for (; stomper.hasValue(); stomper.step()) {
		Heaper e = (Heaper) stomper.fetch();
		if (e == null) {
			continue ;
		}
		t.store(e);
	}
	stomper.destroy();
	return t;
/*
udanax-top.st:61062:HashSetTester methodsFor: 'accessing'!
{ScruSet} generateSetContaining: stuff {Stepper}
	| t {MuSet} |
	
	"t _ MuSet make.			No - we're testing ActualHashSets, not MuSets."
	t _ ActualHashSet make.
	stuff forEach: [:e {Heaper} |
		t store: e].
	^ t!
*/
}
/**
 * HashSetTester runTest
 */
public void oldTestsOn(PrintWriter oo) {
	oo.print("HashSet testing\n"+
"");
	super.allTestsOn(oo);
	test1On(oo);
	test2On(oo);
	test3On(oo);
	test4On(oo);
	testCollisions(oo);
	/* self testBig: oo.	 */
	/* self testOrderedDelete: oo. */
	/* Turned off until regression sets match */
	oo.print("End of HashSet testing\n"+
"");
/*
udanax-top.st:61073:HashSetTester methodsFor: 'smalltalk: obsolete tests'!
{void} oldTestsOn: oo {ostream reference} 
	"HashSetTester runTest"
	oo << 'HashSet testing
'.
	super allTestsOn: oo.
	self test1On: oo.
	self test2On: oo.
	self test3On: oo.
	self test4On: oo.
	self testCollisions: oo.
	"self testBig: oo.	"
	"self testOrderedDelete: oo."		"Turned off until regression sets match"
	oo << 'End of HashSet testing
'!
*/
}
public void printRemoveOfInOn(SHTO pb, MuSet set1, PrintWriter oo) {
	int setSize;
	set1.remove(pb);
	/* oo << 'remove of ' << pb << '
set 1 (count ' << set1 count << ') now : ' << set1 << '
' */
	setSize = ((ActualHashSet) set1).entryTableSize();
	oo.print("hash of ");
	oo.print(pb);
	oo.print(" mod ");
	oo.print(setSize);
	oo.print(" == ");
	oo.print((AboraSupport.modulo(pb.hashForEqual(), setSize)));
	oo.print("\n"+
"");
	oo.print("set 1 now : ");
	oo.print(set1);
	oo.print("\n"+
"internals:	");
	((HashSet) set1).printInternals(oo);
/*
udanax-top.st:61089:HashSetTester methodsFor: 'smalltalk: obsolete tests'!
{void} printRemoveOf: pb {SHTO} in: set1 {MuSet} on: oo {ostream reference}
	| setSize {UInt32} |
	
	set1 remove: pb.
	"oo << 'remove of ' << pb << '
set 1 (count ' << set1 count << ') now : ' << set1 << '
'"
	setSize _ (set1  cast: ActualHashSet) entryTableSize.
	oo << 'hash of ' << pb << ' mod ' << setSize.
	oo << ' == ' << (pb hashForEqual \\ setSize) << '
'.
	oo << 'set 1 now : ' << set1 << '
internals:	'.
	(set1 cast: HashSet) printInternals: oo.!
*/
}
public void printStoreOfInOn(SHTO pb, MuSet set1, PrintWriter oo) {
	int setSize;
	set1.store(pb);
	/* oo << 'store of ' << pb << '
set 1 (count ' << set1 count << ') now : ' << set1 << '
'.  !!!!!!!! */
	setSize = ((ActualHashSet) set1).entryTableSize();
	oo.print("hash of ");
	oo.print(pb);
	oo.print(" mod ");
	oo.print(setSize);
	oo.print(" == ");
	oo.print((AboraSupport.modulo(pb.hashForEqual(), setSize)));
	oo.print("\n"+
"");
	oo.print("set 1 now : ");
	oo.print(set1);
	oo.print("\n"+
"internals:	");
	((HashSet) set1).printInternals(oo);
/*
udanax-top.st:61106:HashSetTester methodsFor: 'smalltalk: obsolete tests'!
{void} printStoreOf: pb {SHTO} in: set1 {MuSet} on: oo {ostream reference}
	| setSize {UInt32} |
	set1 store: pb.
	
	"oo << 'store of ' << pb << '
set 1 (count ' << set1 count << ') now : ' << set1 << '
'.  !!!!!!!!"
	setSize _ (set1 cast: ActualHashSet) entryTableSize.
	oo << 'hash of ' << pb << ' mod ' << setSize. 
	oo << ' == ' << (pb hashForEqual \\ setSize) << '
'.
	oo << 'set 1 now : ' << set1 << '
internals:	'.
	(set1 cast: HashSet) printInternals: oo.!
*/
}
/**
 * HashSetTester runTest: #test1On:
 */
public void test1On(PrintWriter oo) {
	HashSet set1;
	HashSet set2;
	oo.print("start of test 1\n"+
"");
	/* create a set */
	set1 = (HashSet) HashSet.make();
	/* We're testing HashSets, not MuSets */
	oo.print("set 1 == ");
	oo.print(set1);
	oo.print("\n"+
"internals:	");
	set1.printInternals(oo);
	oo.print("\n"+
"");
	printStoreOfInOn((SHTO.make("element one", 360500835)), set1, oo);
	printStoreOfInOn((SHTO.make("element two")), set1, oo);
	printStoreOfInOn((SHTO.make("element three")), set1, oo);
	printStoreOfInOn((SHTO.make("elemeno one")), set1, oo);
	printStoreOfInOn((SHTO.make("elemano one")), set1, oo);
	printStoreOfInOn((SHTO.make("element four")), set1, oo);
	printStoreOfInOn((SHTO.make("element five")), set1, oo);
	printStoreOfInOn((SHTO.make("element six")), set1, oo);
	set2 = (HashSet) set1.copy();
	oo.print("set1 copy == set2 == ");
	oo.print(set2);
	oo.print("\n"+
"internals:	");
	set2.printInternals(oo);
	oo.print("\n"+
"\n"+
"compare sets, set1 isSubsetOf: set2 ");
	oo.print(((set1.isSubsetOf(set2)) ? "TRUE" : "FALSE"));
	oo.print("\n"+
"\n"+
" - now remove 'elemano one' from set1:\n"+
" ");
	set1.remove((SHTO.make("elemano one")));
	oo.print("set 1 now : ");
	oo.print(set1);
	oo.print("\n"+
"internals:	");
	set1.printInternals(oo);
	oo.print("\n"+
"compare sets, set1 isSubsetOf: set2 ");
	oo.print(((set1.isSubsetOf(set2)) ? "TRUE" : "FALSE"));
	oo.print("\n"+
"compare sets, set2 isSubsetOf: set1 ");
	oo.print(((set2.isSubsetOf(set1)) ? "TRUE" : "FALSE"));
	oo.print("\n"+
"");
	/* 
Remove elements three and one from set1.
'.
	set1 remove: (SHTO make: 'element three').
	set1 remove: (SHTO make: 'element one').
	oo << 'set 1 now : ' << set1 << '
' */
	set1.destroy();
	set2.destroy();
/*
udanax-top.st:61122:HashSetTester methodsFor: 'smalltalk: obsolete tests'!
{void} test1On: oo {ostream reference} 
	"HashSetTester runTest: #test1On:"
	| set1 {HashSet} set2 {HashSet} |
	oo << 'start of test 1
'.	"create a set"
	set1 _ HashSet make.		"We're testing HashSets, not MuSets"
	oo << 'set 1 == ' << set1 << '
internals:	'.
	set1 printInternals: oo.
	oo << '
'.
	self printStoreOf: (SHTO make: 'element one' with: 360500835) in: set1 on: oo.
	self printStoreOf: (SHTO make: 'element two') in: set1 on: oo.
	self printStoreOf: (SHTO make: 'element three') in: set1 on: oo.
	self printStoreOf: (SHTO make: 'elemeno one') in: set1 on: oo.
	self printStoreOf: (SHTO make: 'elemano one') in: set1 on: oo.
	self printStoreOf: (SHTO make: 'element four') in: set1 on: oo.
	self printStoreOf: (SHTO make: 'element five') in: set1 on: oo.
	self printStoreOf: (SHTO make: 'element six') in: set1 on: oo.
	set2 _ set1 copy cast: HashSet.
	oo << 'set1 copy == set2 == ' << set2 << '
internals:	'.
	set2 printInternals: oo.
	oo << '
compare sets, set1 isSubsetOf: set2 ' << ((set1 isSubsetOf: set2)
			ifTrue: ['TRUE']
			ifFalse: ['FALSE']) << '
 - now remove ''elemano one'' from set1:
 '.
	set1 remove: (SHTO make: 'elemano one').
	oo << 'set 1 now : ' << set1 << '
internals:	'.
	set1 printInternals: oo.
	oo << '
compare sets, set1 isSubsetOf: set2 ' << ((set1 isSubsetOf: set2)
			ifTrue: ['TRUE']
			ifFalse: ['FALSE']) << '
compare sets, set2 isSubsetOf: set1 ' << ((set2 isSubsetOf: set1)
			ifTrue: ['TRUE']
			ifFalse: ['FALSE']) << '
'."
Remove elements three and one from set1.
'.
	set1 remove: (SHTO make: 'element three').
	set1 remove: (SHTO make: 'element one').
	oo << 'set 1 now : ' << set1 << '
'"
	set1 destroy.
	set2 destroy.!
*/
}
/**
 * HashSetTester runTest: #test2On:
 */
public void test2On(PrintWriter oo) {
	/* second test suite for HashSet */
	HashSet set1;
	Stepper stomp;
	set1 = (HashSet) HashSet.make();
	set1.introduce((SHTO.make("element One")));
	set1.introduce((SHTO.make("element Two")));
	set1.introduce((SHTO.make("element Three")));
	oo.print("Start of test 2, set1 starts as:\n"+
"");
	oo.print(set1);
	oo.print("\n"+
"");
	oo.print("set1 hasMember: ('element One') == ");
	if (set1.hasMember((SHTO.make("element One")))) {
		oo.print("TRUE");
	}
	else {
		oo.print("FALSE");
	}
	oo.print("\n"+
"");
	oo.print("set1 hasMember: ('element Five') == ");
	if (set1.hasMember((SHTO.make("element Five")))) {
		oo.print("TRUE");
	}
	else {
		oo.print("FALSE");
	}
	oo.print("\n"+
"");
	oo.print("set1->count() == ");
	oo.print(set1.count());
	oo.print("\n"+
"");
	oo.print("set1->stepper() produces: ");
	stomp = set1.stepper();
	oo.print(stomp);
	oo.print("\n"+
"and stepping produces elements:\n"+
"");
	while (stomp.hasValue()) {
		oo.print(stomp.fetch());
		oo.print("\n"+
"");
		stomp.step();
	}
	oo.print("\n"+
"");
	set1.destroy();
/*
udanax-top.st:61176:HashSetTester methodsFor: 'smalltalk: obsolete tests'!
{void} test2On: oo {ostream reference} 
	"HashSetTester runTest: #test2On:"
	"second test suite for HashSet"
	| set1 {HashSet} stomp {Stepper} |
	set1 _ HashSet make.
	set1 introduce: (SHTO make: 'element One').
	set1 introduce: (SHTO make: 'element Two').
	set1 introduce: (SHTO make: 'element Three').
	oo << 'Start of test 2, set1 starts as:
' << set1 << '
'.
	oo << 'set1 hasMember: (''element One'') == '.
	(set1 hasMember: (SHTO make: 'element One'))
		ifTrue: [oo << 'TRUE']
		ifFalse: ['FALSE'].
	oo << '
'.
	oo << 'set1 hasMember: (''element Five'') == '.
	(set1 hasMember: (SHTO make: 'element Five'))
		ifTrue: [oo << 'TRUE']
		ifFalse: [oo << 'FALSE'].
	oo << '
'.
	oo << 'set1->count() == ' << set1 count << '
'.
	oo << 'set1->stepper() produces: '.
	stomp _ set1 stepper.
	oo << stomp << '
and stepping produces elements:
'.
	[stomp hasValue]
		whileTrue: [oo << stomp fetch << '
'.
			stomp step].
	oo << '
'.
	set1 destroy!
*/
}
/**
 * HashSetTester runTest: #test3On:
 */
public void test3On(PrintWriter oo) {
	HashSet set1;
	HashSet set2;
	HashSet set3;
	set1 = (HashSet) HashSet.make();
	set2 = (HashSet) HashSet.make();
	set1.introduce((SHTO.make("element One")));
	set1.introduce((SHTO.make("element Two")));
	set1.introduce((SHTO.make("element Three")));
	set2.introduce((SHTO.make("element One")));
	set2.introduce((SHTO.make("element Five")));
	oo.print("Start of test 3, set1 (copied to set3) starts as:\n"+
"\n"+
"");
	oo.print(set1);
	oo.print("\n"+
"\n"+
"and set2 starts as:\n"+
"\n"+
"");
	oo.print(set2);
	oo.print("\n"+
"\n"+
"");
	set3 = (HashSet) set1.copy();
	set3.restrictTo(set2);
	oo.print("set3->restrict(set2) == \n"+
"\n"+
"");
	oo.print(set3);
	oo.print("\n"+
"\n"+
"");
	set3.destroy();
	set3 = (HashSet) set1.copy();
	set3.restrictTo(HashSet.make());
	oo.print("set3->restrict({nullSet}) == \n"+
"\n"+
"");
	oo.print(set3);
	oo.print("\n"+
"\n"+
"");
	set3.destroy();
	set3 = (HashSet) HashSet.make();
	set3.restrictTo(set2);
	oo.print("set3({nullSet})->restrict(set2) == \n"+
"\n"+
"");
	oo.print(set3);
	oo.print("\n"+
"\n"+
"");
	set3.destroy();
	set3 = (HashSet) set1.copy();
	set3.storeAll(set2);
	oo.print("set3->storeAll(set2) == \n"+
"\n"+
"");
	oo.print(set3);
	oo.print("\n"+
"\n"+
"");
	set3.destroy();
	set3 = (HashSet) set1.copy();
	set3.wipeAll(set2);
	oo.print("set3->wipeAll(set2) == \n"+
"\n"+
"");
	oo.print(set3);
	oo.print("\n"+
"\n"+
"");
	set1.destroy();
	set2.destroy();
	set3.destroy();
/*
udanax-top.st:61215:HashSetTester methodsFor: 'smalltalk: obsolete tests'!
{void} test3On: oo {ostream reference} 
	"HashSetTester runTest: #test3On:"
	| set1 {HashSet} set2 {HashSet} set3 {HashSet} |
	set1 _ HashSet make.
	set2 _ HashSet make.
	set1 introduce: (SHTO make: 'element One').
	set1 introduce: (SHTO make: 'element Two').
	set1 introduce: (SHTO make: 'element Three').
	set2 introduce: (SHTO make: 'element One').
	set2 introduce: (SHTO make: 'element Five').
	oo << 'Start of test 3, set1 (copied to set3) starts as:
' << set1 << '
and set2 starts as:
' << set2 << '
'.
	set3 _ set1 copy cast: HashSet.
	set3 restrictTo: set2.
	oo << 'set3->restrict(set2) == 
' << set3 << '
'.
	set3 destroy.
	set3 _ set1 copy cast: HashSet.
	set3 restrictTo: HashSet make.
	oo << 'set3->restrict({nullSet}) == 
' << set3 << '
'.
	set3 destroy.
	set3 _ HashSet make.
	set3 restrictTo: set2.
	oo << 'set3({nullSet})->restrict(set2) == 
' << set3 << '
'.
	set3 destroy.
	set3 _ set1 copy cast: HashSet.
	set3 storeAll: set2.
	oo << 'set3->storeAll(set2) == 
' << set3 << '
'.
	set3 destroy.
	set3 _ set1 copy cast: HashSet.
	set3 wipeAll: set2.
	oo << 'set3->wipeAll(set2) == 
' << set3 << '
'.
	set1 destroy.
	set2 destroy.
	set3 destroy.!
*/
}
/**
 * HashSetTester runTest: #test4On:
 */
public void test4On(PrintWriter oo) {
	HashSet set1;
	HashSet set3;
	set1 = (HashSet) HashSet.make();
	set1.introduce((SHTO.make("One")));
	set1.introduce((SHTO.make("Two")));
	set1.introduce((SHTO.make("Three")));
	oo.print("Start of test 4, set1 (copied to set3) starts as:\n"+
"\n"+
"");
	oo.print(set1);
	oo.print("\n"+
"internals:\n"+
"");
	set1.printInternals(oo);
	oo.print("\n"+
"\n"+
"set1 hasMember: (SHTO make: 'One')");
	oo.print(((set1.hasMember((SHTO.make("One")))) ? "TRUE" : "FALSE"));
	oo.print(".\n"+
"set1 hasMember: (SHTO make: 'Two')");
	oo.print(((set1.hasMember((SHTO.make("Two")))) ? "TRUE" : "FALSE"));
	oo.print(".\n"+
"set1 hasMember: (SHTO make: 'Three')");
	oo.print(((set1.hasMember((SHTO.make("Three")))) ? "TRUE" : "FALSE"));
	oo.print(".\n"+
"");
	set3 = (HashSet) set1.copy();
	set3.store((SHTO.make("Three")));
	oo.print("set3->store ('Three') == \n"+
"\n"+
"");
	oo.print(set3);
	oo.print("\n"+
"\n"+
"");
	set3.destroy();
	set3 = (HashSet) set1.copy();
	set3.store((SHTO.make("Five")));
	oo.print("set3->store ('Five') == \n"+
"\n"+
"");
	oo.print(set3);
	oo.print("\n"+
"\n"+
"");
	set3.destroy();
	set3 = (HashSet) set1.copy();
	set3.wipe((SHTO.make("Three")));
	oo.print("set3->wipe ('Three') == \n"+
"\n"+
"");
	oo.print(set3);
	oo.print("\n"+
"\n"+
"");
	set3.destroy();
	set3 = (HashSet) set1.copy();
	set3.wipe((SHTO.make("Five")));
	oo.print("set3->wipe ('Five') == \n"+
"\n"+
"");
	oo.print(set3);
	oo.print("\n"+
"\n"+
"");
	set3.destroy();
	set3 = (HashSet) set1.copy();
	oo.print("set3->remove ('Ten') == \n"+
"\n"+
"");
	try {
		set3.remove((SHTO.make("Ten")));
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.NOT_IN_SET.equals(ex.getMessage())) {
			oo.print("BLAST(NotInSet)");
			return ;
		}
		else {
			throw ex;
		}
	}
	oo.print(set3);
	oo.print("\n"+
"\n"+
"");
/*
udanax-top.st:61278:HashSetTester methodsFor: 'smalltalk: obsolete tests'!
{void} test4On: oo {ostream reference} 
	"HashSetTester runTest: #test4On:"
	| set1 {HashSet} set3 {HashSet} |
	set1 _ HashSet make.
	set1 introduce: (SHTO make: 'One').
	set1 introduce: (SHTO make: 'Two').
	set1 introduce: (SHTO make: 'Three').
	oo << 'Start of test 4, set1 (copied to set3) starts as:
' << set1 << '
internals:
'.
set1 printInternals: oo.
	oo << '
set1 hasMember: (SHTO make: ''One'')' << ((set1 hasMember: (SHTO make: 'One'))
			ifTrue: ['TRUE']
			ifFalse: ['FALSE']) << '.
set1 hasMember: (SHTO make: ''Two'')' << ((set1 hasMember: (SHTO make: 'Two'))
			ifTrue: ['TRUE']
			ifFalse: ['FALSE']) << '.
set1 hasMember: (SHTO make: ''Three'')' << ((set1 hasMember: (SHTO make: 'Three'))
			ifTrue: ['TRUE']
			ifFalse: ['FALSE']) << '.
'.
	set3 _ set1 copy cast: HashSet.
	set3 store: (SHTO make: 'Three').
	oo << 'set3->store (''Three'') == 
' << set3 << '
'.
	set3 destroy.
	set3 _ set1 copy cast: HashSet.
	set3 store: (SHTO make: 'Five').
	oo << 'set3->store (''Five'') == 
' << set3 << '
'.
	set3 destroy.
	set3 _ set1 copy cast: HashSet.
	set3 wipe: (SHTO make: 'Three').
	oo << 'set3->wipe (''Three'') == 
' << set3 << '
'.
	set3 destroy.
	set3 _ set1 copy cast: HashSet.
	set3 wipe: (SHTO make: 'Five').
	oo << 'set3->wipe (''Five'') == 
' << set3 << '
'.
	set3 destroy.
	set3 _ set1 copy cast: HashSet.
	oo << 'set3->remove (''Ten'') == 
'.
	ScruSet problems.NotInSet handle: [:ex | oo << 'BLAST(NotInSet)'.
		^VOID]
		do: [set3 remove: (SHTO make: 'Ten')].
	oo << set3 << '
'!
*/
}
public void testCollisions(PrintWriter oo) {
	HashSet set1;
	set1 = (HashSet) HashSet.make();
	oo.print("\n"+
"\n"+
"collision testing\n"+
"");
	printStoreOfInOn((SHTO.make("tast", 11)), set1, oo);
	printStoreOfInOn((SHTO.make("tbst", 11)), set1, oo);
	printStoreOfInOn((SHTO.make("tcst", 11)), set1, oo);
	printStoreOfInOn((SHTO.make("tdst", 11)), set1, oo);
	printStoreOfInOn((SHTO.make("test", 11)), set1, oo);
	printStoreOfInOn((SHTO.make("tfst", 11)), set1, oo);
	printStoreOfInOn((SHTO.make("tgst", 11)), set1, oo);
	printStoreOfInOn((SHTO.make("thst", 11)), set1, oo);
	printStoreOfInOn((SHTO.make("tist", 11)), set1, oo);
	printStoreOfInOn((SHTO.make("tjst", 11)), set1, oo);
	printStoreOfInOn((SHTO.make("tkst", 11)), set1, oo);
	oo.print("storing elements already present\n"+
"");
	printStoreOfInOn((SHTO.make("test", 11)), set1, oo);
	printStoreOfInOn((SHTO.make("tast", 11)), set1, oo);
	printStoreOfInOn((SHTO.make("tgst", 11)), set1, oo);
	oo.print("storing new element (tlst)\n"+
"");
	printStoreOfInOn((SHTO.make("tlst", 11)), set1, oo);
	printRemoveOfInOn((SHTO.make("tast", 11)), set1, oo);
	printRemoveOfInOn((SHTO.make("tdst", 11)), set1, oo);
	set1.destroy();
/*
udanax-top.st:61348:HashSetTester methodsFor: 'smalltalk: obsolete tests'!
{void} testCollisions: oo {ostream reference}
	| set1 {HashSet} |
	set1 _ HashSet make.
	oo << '
collision testing
'.
	self printStoreOf: (SHTO make: 'tast' with: 11) in: set1 on: oo.
	self printStoreOf: (SHTO make: 'tbst' with: 11) in: set1 on: oo.
	self printStoreOf: (SHTO make: 'tcst' with: 11) in: set1 on: oo.
	self printStoreOf: (SHTO make: 'tdst' with: 11) in: set1 on: oo.
	self printStoreOf: (SHTO make: 'test' with: 11) in: set1 on: oo.
	self printStoreOf: (SHTO make: 'tfst' with: 11) in: set1 on: oo.
	self printStoreOf: (SHTO make: 'tgst' with: 11) in: set1 on: oo.
	self printStoreOf: (SHTO make: 'thst' with: 11) in: set1 on: oo.
	self printStoreOf: (SHTO make: 'tist' with: 11) in: set1 on: oo.
	self printStoreOf: (SHTO make: 'tjst' with: 11) in: set1 on: oo.
	self printStoreOf: (SHTO make: 'tkst' with: 11) in: set1 on: oo.
oo << 'storing elements already present
'.
	self printStoreOf: (SHTO make: 'test' with: 11) in: set1 on: oo.
	self printStoreOf: (SHTO make: 'tast' with: 11) in: set1 on: oo.
	self printStoreOf: (SHTO make: 'tgst' with: 11) in: set1 on: oo.
oo << 'storing new element (tlst)
'.
	self printStoreOf: (SHTO make: 'tlst' with: 11) in: set1 on: oo.
	self printRemoveOf: (SHTO make: 'tast' with: 11) in: set1 on: oo.
	self printRemoveOf: (SHTO make: 'tdst' with: 11) in: set1 on: oo.
	set1 destroy!
*/
}
/**
 * 0 to: 40 do: [:n | ((((n*7)+1) \\ 5) == 3) ifTrue: [Transcript show: 'another is '; print:
 * ((n*7)+1); cr; endEntry]].
 */
public void testOrderedDelete(PrintWriter oo) {
	HashSet testSet;
	Heaper h1;
	Heaper h2;
	Heaper h3;
	Heaper h4;
	oo.print("\n"+
"testing ordered hash chaining.\n"+
"");
	testSet = (HashSet) HashSet.make();
	testSet.introduce((h1 = SHTO.make("seventy-eight", 78)));
	testSet.introduce((h2 = SHTO.make("forty-three", 43)));
	testSet.introduce((h3 = SHTO.make("eight", 8)));
	oo.print("testSet now ");
	oo.print(testSet);
	oo.print("\n"+
"");
	if ((testSet.hasMember(h1)) && ((testSet.hasMember(h2)) && (testSet.hasMember(h3)))) {
		oo.print("all members found\n"+
"");
	}
	else {
		oo.print("ERROR - not all members found\n"+
"");
	}
	testSet.remove(h2);
	oo.print("remove ");
	oo.print(h2);
	oo.print(" , testSet now ");
	oo.print(testSet);
	oo.print("\n"+
"");
	if (testSet.hasMember(h1)) {
		oo.print("set still professes to contain ");
		oo.print(h1);
		oo.print("\n"+
"");
	}
	else {
		oo.print("ERROR - set does not profess to contain ");
		oo.print(h1);
		oo.print("\n"+
"");
	}
	if (testSet.hasMember(h3)) {
		oo.print("set still professes to contain ");
		oo.print(h3);
		oo.print("\n"+
"");
	}
	else {
		oo.print("ERROR - set does not profess to contain ");
		oo.print(h3);
		oo.print("\n"+
"");
	}
	testSet.introduce((h4 = SHTO.make("one-hundred-thirteen", 113)));
	oo.print("introduce ");
	oo.print(h4);
	oo.print(", testSet now ");
	oo.print(testSet);
	oo.print("\n"+
"");
	if (testSet.hasMember(h1)) {
		oo.print("set still professes to contain ");
		oo.print(h1);
		oo.print("\n"+
"");
	}
	else {
		oo.print("ERROR - set does not profess to contain ");
		oo.print(h1);
		oo.print("\n"+
"");
	}
	if (testSet.hasMember(h3)) {
		oo.print("set still professes to contain ");
		oo.print(h3);
		oo.print("\n"+
"");
	}
	else {
		oo.print("ERROR - set does not profess to contain ");
		oo.print(h3);
		oo.print("\n"+
"");
	}
/*
udanax-top.st:61379:HashSetTester methodsFor: 'smalltalk: obsolete tests'!
{void} testOrderedDelete: oo {ostream reference}
	"0 to: 40 do: [:n | ((((n*7)+1) \\ 5) == 3) ifTrue: [Transcript show: 'another is '; print: ((n*7)+1); cr; endEntry]]."
	| testSet {HashSet} h1 {Heaper} h2 {Heaper} h3 {Heaper} h4 {Heaper} |
	
	oo << '
testing ordered hash chaining.
'.
	testSet _ HashSet make.
	testSet introduce: (h1 _ SHTO make: 'seventy-eight' with: 78).
	testSet introduce: (h2 _ SHTO make: 'forty-three' with: 43).
	testSet introduce: (h3 _ SHTO make: 'eight' with: 8).
	oo << 'testSet now ' << testSet << '
'.
	((testSet hasMember: h1) and: [(testSet hasMember: h2) and: [testSet hasMember: h3]])
		ifTrue: [oo << 'all members found
']
		ifFalse: [ oo << 'ERROR - not all members found
'].
	testSet remove: h2.
	oo << 'remove ' << h2 << ' , testSet now ' << testSet << '
'.
	(testSet hasMember: h1)
		ifTrue: [oo << 'set still professes to contain ' << h1 << '
']
		ifFalse: [oo << 'ERROR - set does not profess to contain ' << h1 << '
'].
	(testSet hasMember: h3)
		ifTrue: [oo << 'set still professes to contain ' << h3 << '
']
		ifFalse: [oo << 'ERROR - set does not profess to contain ' << h3 << '
'].
	testSet introduce: (h4 _ SHTO make: 'one-hundred-thirteen' with: 113).
	oo << 'introduce ' << h4 << ', testSet now ' << testSet << '
'.
	(testSet hasMember: h1)
		ifTrue: [oo << 'set still professes to contain ' << h1 << '
']
		ifFalse: [oo << 'ERROR - set does not profess to contain ' << h1 << '
'].
	(testSet hasMember: h3)
		ifTrue: [oo << 'set still professes to contain ' << h3 << '
']
		ifFalse: [oo << 'ERROR - set does not profess to contain ' << h3 << '
'].!
*/
}
public HashSetTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:61429:HashSetTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:61432:HashSetTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public HashSetTester() {
/*

Generated during transformation
*/
}
public void introduceTestsOn(PrintWriter oo, MuSet set, SHTO shto) {
	introduceTestsOn(oo, (HashSet) set, shto);
/*

Generated during transformation: AddMethod
*/
}
public void storeTestsOn(PrintWriter oo, MuSet set, SHTO shto) {
	storeTestsOn(oo, (HashSet) set, shto);
/*

Generated during transformation: AddMethod
*/
}
}
