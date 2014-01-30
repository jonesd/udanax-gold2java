/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.settable;

import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.settable.SetTable;
import info.dgjones.abora.gold.collection.settable.SetTableTester;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class SetTableTester extends Tester {

/*
udanax-top.st:61435:
Tester subclass: #SetTableTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-SetTable'!
*/
/*
udanax-top.st:61439:
(SetTableTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SetTableTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void allTestsOn(PrintWriter oo) {
	simpleAccess(oo);
	test1on(oo);
	growTestOn(oo);
	growTest2On(oo);
	stepTestOn(oo);
/*
udanax-top.st:61444:SetTableTester methodsFor: 'tests'!
{void} allTestsOn: oo {ostream reference}
	self simpleAccess: oo.
	self test1on: oo.
	self growTestOn: oo.
	self growTest2On: oo.
	self stepTestOn: oo.!
*/
}
public void growTest2On(PrintWriter oo) {
	SetTable tab;
	ScruSet keyPile;
	ScruSet valuePile;
	int oc;
	keyPile = testKeys();
	valuePile = testValues();
	oo.print("start of grow test 2, add key->value associations in a different order\n"+
"");
	tab = SetTable.make(testCS());
	oc = 0;
	Stepper stomper = valuePile.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Heaper val = (Heaper) stomper.fetch();
		if (val == null) {
			continue ;
		}
		Stepper stomper2 = keyPile.stepper();
		for (; stomper2.hasValue(); stomper2.step()) {
			Position key = (Position) stomper2.fetch();
			if (key == null) {
				continue ;
			}
			if ( ! (tab.count() == oc)) {
				oo.print("table count wrong before store! ");
				oo.print(tab);
				oo.print("\n"+
"");
			}
			tab.introduce(key, val);
			oc = oc + 1;
			if ( ! (tab.count() == oc)) {
				oo.print("table count wrong after store!  ");
				oo.print(tab);
				oo.print("\n"+
"");
			}
			if ( ! (tab.count() == (manualCount(tab)))) {
				oo.print("manual count doesn't match count!  ");
				oo.print(tab);
				oo.print("\n"+
"");
			}
		}
		stomper2.destroy();
	}
	stomper.destroy();
	oo.print("end of grow test 2, table now:\n"+
"");
	oo.print(tab);
	oo.print("\n"+
"\n"+
"now - remove all those entries, use a different order than the introduce order\n"+
"");
	Stepper stomper3 = keyPile.stepper();
	for (; stomper3.hasValue(); stomper3.step()) {
		Position key2 = (Position) stomper3.fetch();
		if (key2 == null) {
			continue ;
		}
		Stepper stomper4 = valuePile.stepper();
		for (; stomper4.hasValue(); stomper4.step()) {
			Heaper val2 = (Heaper) stomper4.fetch();
			if (val2 == null) {
				continue ;
			}
			if ( ! (tab.count() == oc)) {
				oo.print("table count wrong before remove! ");
				oo.print(tab);
				oo.print("\n"+
"");
			}
			tab.remove(key2, val2);
			oc = oc - 1;
			if ( ! (tab.count() == oc)) {
				oo.print("table count wrong after remove!  ");
				oo.print(tab);
				oo.print("\n"+
"");
			}
			if ( ! (tab.count() == (manualCount(tab)))) {
				oo.print("manual count doesn't match count!  ");
				oo.print(tab);
				oo.print("\n"+
"");
			}
		}
		stomper4.destroy();
	}
	stomper3.destroy();
	oo.print("\n"+
"end of remove test. ta ta!\n"+
"");
/*
udanax-top.st:61451:SetTableTester methodsFor: 'tests'!
{void} growTest2On: oo {ostream reference}
	| tab {SetTable} keyPile {ScruSet} valuePile {ScruSet} oc {IntegerVar} |
	
	keyPile _ self testKeys.
	valuePile _ self testValues.
	oo << 'start of grow test 2, add key->value associations in a different order
'.
	
	tab _ SetTable make: self testCS.
	oc _ IntegerVar0.
	valuePile stepper forEach: [:val {Heaper} |
		keyPile stepper forEach: [:key {Position} |
			tab count = oc ifFalse: [oo << 'table count wrong before store!! ' << tab << '
'].
			tab at: key introduce: val.
			oc _ oc + 1.
			tab count = oc ifFalse: [oo << 'table count wrong after store!!  ' << tab << '
'].
			(tab count = (self manualCount: tab)) 
				ifFalse: [oo << 'manual count doesn''t match count!!  ' << tab << '
']]].
	oo << 'end of grow test 2, table now:
' << tab << '
now - remove all those entries, use a different order than the introduce order
'.
	keyPile stepper forEach: [:key2 {Position} |
		valuePile stepper forEach: [:val2 {Heaper} |
			tab count = oc ifFalse: [oo << 'table count wrong before remove!! ' << tab << '
'].
			tab remove: key2 with: val2.
			oc _ oc - 1.
			tab count = oc ifFalse: [oo << 'table count wrong after remove!!  ' << tab << '
'].
			(tab count = (self manualCount: tab)) 
				ifFalse: [oo << 'manual count doesn''t match count!!  ' << tab << '
']]].
	oo << '
end of remove test. ta ta!!
'!
*/
}
public void growTestOn(PrintWriter oo) {
	SetTable tab;
	ScruSet keyPile;
	ScruSet valuePile;
	int oc;
	keyPile = testKeys();
	valuePile = testValues();
	oo.print("start of grow test, keys =\n"+
"");
	oo.print(keyPile);
	oo.print("\n"+
"and values = ");
	oo.print(valuePile);
	oo.print("\n"+
"");
	tab = SetTable.make(testCS());
	oc = 0;
	Stepper stomper = keyPile.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Position key = (Position) stomper.fetch();
		if (key == null) {
			continue ;
		}
		Stepper stomper2 = valuePile.stepper();
		for (; stomper2.hasValue(); stomper2.step()) {
			Heaper val = (Heaper) stomper2.fetch();
			if (val == null) {
				continue ;
			}
			if ( ! (tab.count() == oc)) {
				oo.print("table count wrong before store! ");
				oo.print(tab);
				oo.print("\n"+
"");
			}
			tab.introduce(key, val);
			oc = oc + 1;
			if ( ! (tab.count() == oc)) {
				oo.print("table count wrong after store!  ");
				oo.print(tab);
				oo.print("\n"+
"");
			}
			if ( ! (tab.count() == (manualCount(tab)))) {
				oo.print("manual count doesn't match count!  ");
				oo.print(tab);
				oo.print("\n"+
"");
			}
		}
		stomper2.destroy();
	}
	stomper.destroy();
	oo.print("end of grow test, table now:\n"+
"");
	oo.print(tab);
	oo.print("\n"+
"\n"+
"and the domain is: ");
	oo.print(tab.domain());
	oo.print("\n"+
"\n"+
"now - remove all those entries!\n"+
"");
	Stepper stomper3 = keyPile.stepper();
	for (; stomper3.hasValue(); stomper3.step()) {
		Position key2 = (Position) stomper3.fetch();
		if (key2 == null) {
			continue ;
		}
		Stepper stomper4 = valuePile.stepper();
		for (; stomper4.hasValue(); stomper4.step()) {
			Heaper val2 = (Heaper) stomper4.fetch();
			if (val2 == null) {
				continue ;
			}
			if ( ! (tab.count() == oc)) {
				oo.print("table count wrong before remove! ");
				oo.print(tab);
				oo.print("\n"+
"");
			}
			tab.remove(key2, val2);
			oc = oc - 1;
			if ( ! (tab.count() == oc)) {
				oo.print("table count wrong after remove!  ");
				oo.print(tab);
				oo.print("\n"+
"");
			}
			if ( ! (tab.count() == (manualCount(tab)))) {
				oo.print("manual count doesn't match count!  ");
				oo.print(tab);
				oo.print("\n"+
"");
			}
		}
		stomper4.destroy();
	}
	stomper3.destroy();
	oo.print("\n"+
"end of remove test. ta ta!\n"+
"");
/*
udanax-top.st:61495:SetTableTester methodsFor: 'tests'!
{void} growTestOn: oo {ostream reference}
	| tab {SetTable} keyPile {ScruSet} valuePile {ScruSet} oc {IntegerVar} |
	
	keyPile _ self testKeys.
	valuePile _ self testValues.
	oo << 'start of grow test, keys =
' << keyPile << '
and values = ' << valuePile << '
'.
	
	tab _ SetTable make: self testCS.
	oc _ IntegerVar0.
	keyPile stepper forEach: [:key {Position} |
		valuePile stepper forEach: [:val {Heaper} |
			tab count = oc ifFalse: [oo << 'table count wrong before store!! ' << tab << '
'].
			tab at: key introduce: val.
			oc _ oc + 1.
			tab count = oc ifFalse: [oo << 'table count wrong after store!!  ' << tab << '
'].
			(tab count = (self manualCount: tab)) 
				ifFalse: [oo << 'manual count doesn''t match count!!  ' << tab << '
']]].
	oo << 'end of grow test, table now:
' << tab << '
and the domain is: ' << tab domain << '
now - remove all those entries!!
'.
	keyPile stepper forEach: [:key2 {Position} |
		valuePile stepper forEach: [:val2 {Heaper} |
			tab count = oc ifFalse: [oo << 'table count wrong before remove!! ' << tab << '
'].
			tab remove: key2 with: val2.
			oc _ oc - 1.
			tab count = oc ifFalse: [oo << 'table count wrong after remove!!  ' << tab << '
'].
			(tab count = (self manualCount: tab)) 
				ifFalse: [oo << 'manual count doesn''t match count!!  ' << tab << '
']]].
	oo << '
end of remove test. ta ta!!
'!
*/
}
public void simpleAccess(PrintWriter oo) {
	SetTable tab;
	tab = SetTable.make(IntegerSpace.make());
	oo.print("Introduce I(2) at I(1).\n"+
"");
	tab.introduce(IntegerPos.make(1), IntegerPos.make(2));
	oo.print("Retrieve all from 1: \n"+
"");
	Stepper stomper = (tab.stepperAtInt(1));
	for (; stomper.hasValue(); stomper.step()) {
		Heaper elem = (Heaper) stomper.fetch();
		if (elem == null) {
			continue ;
		}
		oo.print("	");
		oo.print(elem);
		oo.print("\n"+
"");
	}
	stomper.destroy();
	oo.print("\n"+
"");
	oo.print("Introduce I(2) at I(1) again and catch the blast.\n"+
"");
	try {
		tab.introduce(IntegerPos.make(1), IntegerPos.make(2));
		oo.print("Should have blasted on second introduce.\n"+
"");
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.ALREADY_IN_TABLE.equals(ex.getMessage())) {
			oo.print("Blasted while introducing.\n"+
"\n"+
"");
		}
		else {
			throw ex;
		}
	}
	oo.print("Store I(3) at 1.\n"+
"");
	tab.intStore(1, IntegerPos.make(3));
	oo.print("Retrieve all from I(1): \n"+
"");
	Stepper stomper2 = (tab.stepperAt(IntegerPos.make(1)));
	for (; stomper2.hasValue(); stomper2.step()) {
		Heaper elem2 = (Heaper) stomper2.fetch();
		if (elem2 == null) {
			continue ;
		}
		oo.print("	");
		oo.print(elem2);
		oo.print("\n"+
"");
	}
	stomper2.destroy();
	oo.print("\n"+
"");
	oo.print("Store I(4) at 1.\n"+
"");
	tab.intStore(1, IntegerPos.make(4));
	oo.print("Retrieve all from 1: \n"+
"");
	Stepper stomper3 = (tab.stepperAtInt(1));
	for (; stomper3.hasValue(); stomper3.step()) {
		Heaper elem3 = (Heaper) stomper3.fetch();
		if (elem3 == null) {
			continue ;
		}
		oo.print("	");
		oo.print(elem3);
		oo.print("\n"+
"");
	}
	stomper3.destroy();
	oo.print("\n"+
"");
	oo.print("Table is now: ");
	oo.print(tab);
	oo.print("\n"+
"Remove I(3) at I(1).\n"+
"");
	tab.remove(IntegerPos.make(1), IntegerPos.make(3));
	oo.print("Table is now: ");
	oo.print(tab);
	oo.print("\n"+
"\n"+
"");
/*
udanax-top.st:61543:SetTableTester methodsFor: 'tests'!
{void} simpleAccess: oo {ostream reference}
	| tab {SetTable} |
	tab _ SetTable make: IntegerSpace make.
	oo << 'Introduce I(2) at I(1).
'.
	tab at: 1 integer introduce: 2 integer.
	oo << 'Retrieve all from 1: 
'.
	(tab stepperAtInt: 1) forEach:
		[:elem {Heaper} | oo << '	' << elem << '
'].
	oo << '
'.
	oo << 'Introduce I(2) at I(1) again and catch the blast.
'.
	(MuTable problems.AlreadyInTable)
		handle: [:ex | oo << 'Blasted while introducing.
'.				ex return]
		do: [tab at: 1 integer introduce: 2 integer.
			oo << 'Should have blasted on second introduce.
'].
	oo << 'Store I(3) at 1.
'.
	tab atInt: 1 store: 3 integer.
	oo << 'Retrieve all from I(1): 
'.
	(tab stepperAt: 1 integer) forEach:
		[:elem2 {Heaper} | oo << '	' << elem2 << '
'].
	oo << '
'.
	oo << 'Store I(4) at 1.
'.
	tab atInt: 1 store: 4 integer.
	oo << 'Retrieve all from 1: 
'.
	(tab stepperAtInt: 1) forEach:
		[:elem3 {Heaper} | oo << '	' << elem3 << '
'].
	oo << '
'.
	oo << 'Table is now: ' << tab << '
Remove I(3) at I(1).
'.
	tab remove: 1 integer with: 3 integer.
	oo << 'Table is now: ' << tab << '
'.!
*/
}
public void stepTestOn(PrintWriter oo) {
	Stepper stepr;
	ScruSet keyPile;
	ScruSet valuePile;
	SetTable tab;
	oo.print("Test fetching of steppers (stepper at a key).\n"+
"");
	keyPile = testKeys();
	valuePile = testValues();
	tab = SetTable.make(testCS());
	Stepper stomper = keyPile.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Position key = (Position) stomper.fetch();
		if (key == null) {
			continue ;
		}
		Stepper stomper2 = valuePile.stepper();
		for (; stomper2.hasValue(); stomper2.step()) {
			Heaper val = (Heaper) stomper2.fetch();
			if (val == null) {
				continue ;
			}
			tab.introduce(key, val);
		}
		stomper2.destroy();
	}
	stomper.destroy();
	Stepper stomper3 = keyPile.stepper();
	for (; stomper3.hasValue(); stomper3.step()) {
		Position key2 = (Position) stomper3.fetch();
		if (key2 == null) {
			continue ;
		}
		MuSet valSet;
		valSet = testValues().asMuSet();
		stepr = tab.stepperAt(key2);
		oo.print("stepper for key ");
		oo.print(key2);
		oo.print(" is ");
		oo.print(stepr);
		oo.print("\n"+
"");
		Stepper stomper4 = stepr;
		for (; stomper4.hasValue(); stomper4.step()) {
			Heaper val3 = (Heaper) stomper4.fetch();
			if (val3 == null) {
				continue ;
			}
			valSet.remove(val3);
		}
		stomper4.destroy();
		if ( ! valSet.isEmpty()) {
			oo.print("valSet contains ");
			oo.print(valSet);
			oo.print("\n"+
"");
		}
	}
	stomper3.destroy();
	oo.print("end of stepperAt: test\n"+
"\n"+
"");
/*
udanax-top.st:61594:SetTableTester methodsFor: 'tests'!
{void} stepTestOn: oo {ostream reference}
	| stepr {Stepper} keyPile {ScruSet of: Position} valuePile {ScruSet of: Heaper} 
	 tab {SetTable} |
	oo << 'Test fetching of steppers (stepper at a key).
'.
	keyPile _ self testKeys.
	valuePile _ self testValues.
	tab _ SetTable make: self testCS.
	keyPile stepper forEach: [:key {Position} |
		valuePile stepper forEach: [:val {Heaper} |
			tab at: key introduce: val]].
	keyPile stepper forEach: [:key2 {Position} | | valSet {MuSet} |
		valSet _ self testValues asMuSet.
		stepr _ tab stepperAt: key2.
		oo << 'stepper for key ' << key2 << ' is ' << stepr << '
'.
		stepr forEach: [:val3 {Heaper} |
			valSet remove: val3].
		valSet isEmpty not ifTrue: [oo << 'valSet contains ' << valSet << '
']].
	oo << 'end of stepperAt: test
'.!
*/
}
public void test1on(PrintWriter oo) {
	SetTable tab1;
	TableStepper stp;
	tab1 = SetTable.make(IntegerSpace.make());
	oo.print("table is now ");
	oo.print(tab1);
	oo.print("\n"+
"");
	tab1.store(IntegerPos.make(1), (Sequence.string("abcd")));
	tab1.store(IntegerPos.make(1), (Sequence.string("abce")));
	tab1.store(IntegerPos.make(1), (Sequence.string("abcf")));
	tab1.store(IntegerPos.make(1), (Sequence.string("abcg")));
	tab1.store(IntegerPos.make(1), (Sequence.string("abch")));
	tab1.store(IntegerPos.make(1), (Sequence.string("abci")));
	oo.print("tab1 is now ");
	oo.print(tab1);
	oo.print("\n"+
"");
	tab1.store(IntegerPos.make(2), (Sequence.string("abcd")));
	tab1.store(IntegerPos.make(2), (Sequence.string("abce")));
	tab1.store(IntegerPos.make(2), (Sequence.string("abcf")));
	tab1.store(IntegerPos.make(2), (Sequence.string("abcg")));
	tab1.store(IntegerPos.make(2), (Sequence.string("abch")));
	tab1.store(IntegerPos.make(2), (Sequence.string("abci")));
	oo.print("tab1 is now ");
	oo.print(tab1);
	oo.print("\n"+
"");
	tab1.store(IntegerPos.make(3), (Sequence.string("abcd")));
	tab1.store(IntegerPos.make(3), (Sequence.string("abce")));
	tab1.store(IntegerPos.make(3), (Sequence.string("abcf")));
	tab1.store(IntegerPos.make(3), (Sequence.string("abcg")));
	tab1.store(IntegerPos.make(3), (Sequence.string("abch")));
	tab1.store(IntegerPos.make(3), (Sequence.string("abci")));
	oo.print("tab1 is now ");
	oo.print(tab1);
	oo.print("\n"+
"");
	oo.print("\n"+
"contents of table are:\n"+
"");
	Stepper stomper = (stp = tab1.stepper());
	for (; stomper.hasValue(); stomper.step()) {
		Heaper elem = (Heaper) stomper.fetch();
		if (elem == null) {
			continue ;
		}
		oo.print("tab1 fetch: ");
		oo.print(stp.position());
		oo.print(" == ");
		oo.print(elem);
		oo.print("\n"+
"");
	}
	stomper.destroy();
/*
udanax-top.st:61619:SetTableTester methodsFor: 'tests'!
{void} test1on: oo {ostream reference}
	| tab1 {SetTable} stp {TableStepper} |
	
	tab1 _ SetTable make: IntegerSpace make.
	oo << 'table is now ' << tab1 << '
'.
	tab1 at: 1 integer store: (Sequence string: 'abcd').
	tab1 at: 1 integer store: (Sequence string: 'abce').
	tab1 at: 1 integer store: (Sequence string: 'abcf').
	tab1 at: 1 integer store: (Sequence string: 'abcg').
	tab1 at: 1 integer store: (Sequence string: 'abch').
	tab1 at: 1 integer store: (Sequence string: 'abci').
	oo << 'tab1 is now ' << tab1 << '
'.
	tab1 at: 2 integer store: (Sequence string: 'abcd').
	tab1 at: 2 integer store: (Sequence string: 'abce').
	tab1 at: 2 integer store: (Sequence string: 'abcf').
	tab1 at: 2 integer store: (Sequence string: 'abcg').
	tab1 at: 2 integer store: (Sequence string: 'abch').
	tab1 at: 2 integer store: (Sequence string: 'abci').
	oo << 'tab1 is now ' << tab1 << '
'.
	tab1 at: 3 integer store: (Sequence string: 'abcd').
	tab1 at: 3 integer store: (Sequence string: 'abce').
	tab1 at: 3 integer store: (Sequence string: 'abcf').
	tab1 at: 3 integer store: (Sequence string: 'abcg').
	tab1 at: 3 integer store: (Sequence string: 'abch').
	tab1 at: 3 integer store: (Sequence string: 'abci').
	oo << 'tab1 is now ' << tab1 << '
'.
	oo << '
contents of table are:
'.
	(stp _ tab1 stepper) forEach: [:elem {Heaper} | 
		oo << 'tab1 fetch: ' << stp position << ' == ' << elem << '
']!
*/
}
public int lastTestValue() {
	return 9;
/*
udanax-top.st:61659:SetTableTester methodsFor: 'private: testing'!
{IntegerVar} lastTestValue
	^ 9!
*/
}
public int manualCount(SetTable table) {
	int cnt;
	cnt = 0;
	Stepper stomper = table.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Heaper elem = (Heaper) stomper.fetch();
		if (elem == null) {
			continue ;
		}
		if (elem != null) {
			cnt = cnt + 1;
		}
	}
	stomper.destroy();
	/* kill st80 'elem not used' msg */
	return cnt;
/*
udanax-top.st:61663:SetTableTester methodsFor: 'private: testing'!
{IntegerVar} manualCount: table {SetTable}
	| cnt {IntegerVar} |
	cnt _ IntegerVar0.
	
	table stepper forEach: [:elem {Heaper} |
		elem ~~ NULL ifTrue: [cnt _ cnt + 1]]. "kill st80 'elem not used' msg"
	^ cnt!
*/
}
public CoordinateSpace testCS() {
	return SequenceSpace.make();
/*
udanax-top.st:61672:SetTableTester methodsFor: 'private: testing'!
{CoordinateSpace} testCS
	^ SequenceSpace make!
*/
}
public ScruSet testKeys() {
	MuSet keys;
	keys = MuSet.make();
	keys.introduce((Sequence.string("fghijklmna")));
	keys.introduce((Sequence.string("fghijklmnb")));
	keys.introduce((Sequence.string("fghijklmnc")));
	keys.introduce((Sequence.string("fghijklmnd")));
	keys.introduce((Sequence.string("fghijklmne")));
	keys.introduce((Sequence.string("fghijklmao")));
	keys.introduce((Sequence.string("fghijklmbo")));
	keys.introduce((Sequence.string("fghijklmco")));
	keys.introduce((Sequence.string("fghijklmdo")));
	keys.introduce((Sequence.string("fghijklmeo")));
	keys.introduce((Sequence.string("fghijklano")));
	keys.introduce((Sequence.string("fghijklbno")));
	keys.introduce((Sequence.string("fghijklcno")));
	keys.introduce((Sequence.string("fghijkldno")));
	keys.introduce((Sequence.string("fghijkleno")));
	keys.introduce((Sequence.string("fghijkamno")));
	keys.introduce((Sequence.string("fghijkbmno")));
	keys.introduce((Sequence.string("fghijkcmno")));
	keys.introduce((Sequence.string("fghijkdmno")));
	keys.introduce((Sequence.string("fghijkemno")));
	keys.introduce((Sequence.string("fghijalmno")));
	keys.introduce((Sequence.string("fghijblmno")));
	keys.introduce((Sequence.string("fghijclmno")));
	keys.introduce((Sequence.string("fghijdlmno")));
	keys.introduce((Sequence.string("fghijelmno")));
	keys.introduce((Sequence.string("fghiaklmno")));
	keys.introduce((Sequence.string("fghibklmno")));
	keys.introduce((Sequence.string("fghicklmno")));
	keys.introduce((Sequence.string("fghidklmno")));
	keys.introduce((Sequence.string("fghieklmno")));
	keys.introduce((Sequence.string("fghajklmno")));
	keys.introduce((Sequence.string("fghbjklmno")));
	keys.introduce((Sequence.string("fghcjklmno")));
	keys.introduce((Sequence.string("fghdjklmno")));
	keys.introduce((Sequence.string("fghejklmno")));
	keys.introduce((Sequence.string("fgaijklmno")));
	keys.introduce((Sequence.string("fgbijklmno")));
	keys.introduce((Sequence.string("fgcijklmno")));
	keys.introduce((Sequence.string("fgdijklmno")));
	keys.introduce((Sequence.string("fgeijklmno")));
	keys.introduce((Sequence.string("fahijklmno")));
	keys.introduce((Sequence.string("fbhijklmno")));
	keys.introduce((Sequence.string("fchijklmno")));
	keys.introduce((Sequence.string("fdhijklmno")));
	keys.introduce((Sequence.string("fehijklmno")));
	keys.introduce((Sequence.string("aghijklmno")));
	keys.introduce((Sequence.string("bghijklmno")));
	keys.introduce((Sequence.string("cghijklmno")));
	keys.introduce((Sequence.string("dghijklmno")));
	keys.introduce((Sequence.string("eghijklmno")));
	return keys;
/*
udanax-top.st:61675:SetTableTester methodsFor: 'private: testing'!
{ScruSet of: Position} testKeys
	| keys {MuSet of: Position} |
	
	keys _ MuSet make.
	keys introduce: (Sequence string: 'fghijklmna').
	keys introduce: (Sequence string: 'fghijklmnb').
	keys introduce: (Sequence string: 'fghijklmnc').
	keys introduce: (Sequence string: 'fghijklmnd').
	keys introduce: (Sequence string: 'fghijklmne').
	keys introduce: (Sequence string: 'fghijklmao').
	keys introduce: (Sequence string: 'fghijklmbo').
	keys introduce: (Sequence string: 'fghijklmco').
	keys introduce: (Sequence string: 'fghijklmdo').
	keys introduce: (Sequence string: 'fghijklmeo').
	keys introduce: (Sequence string: 'fghijklano').
	keys introduce: (Sequence string: 'fghijklbno').
	keys introduce: (Sequence string: 'fghijklcno').
	keys introduce: (Sequence string: 'fghijkldno').
	keys introduce: (Sequence string: 'fghijkleno').
	keys introduce: (Sequence string: 'fghijkamno').
	keys introduce: (Sequence string: 'fghijkbmno').
	keys introduce: (Sequence string: 'fghijkcmno').
	keys introduce: (Sequence string: 'fghijkdmno').
	keys introduce: (Sequence string: 'fghijkemno').
	keys introduce: (Sequence string: 'fghijalmno').
	keys introduce: (Sequence string: 'fghijblmno').
	keys introduce: (Sequence string: 'fghijclmno').
	keys introduce: (Sequence string: 'fghijdlmno').
	keys introduce: (Sequence string: 'fghijelmno').
	keys introduce: (Sequence string: 'fghiaklmno').
	keys introduce: (Sequence string: 'fghibklmno').
	keys introduce: (Sequence string: 'fghicklmno').
	keys introduce: (Sequence string: 'fghidklmno').
	keys introduce: (Sequence string: 'fghieklmno').
	keys introduce: (Sequence string: 'fghajklmno').
	keys introduce: (Sequence string: 'fghbjklmno').
	keys introduce: (Sequence string: 'fghcjklmno').
	keys introduce: (Sequence string: 'fghdjklmno').
	keys introduce: (Sequence string: 'fghejklmno').
	keys introduce: (Sequence string: 'fgaijklmno').
	keys introduce: (Sequence string: 'fgbijklmno').
	keys introduce: (Sequence string: 'fgcijklmno').
	keys introduce: (Sequence string: 'fgdijklmno').
	keys introduce: (Sequence string: 'fgeijklmno').
	keys introduce: (Sequence string: 'fahijklmno').
	keys introduce: (Sequence string: 'fbhijklmno').
	keys introduce: (Sequence string: 'fchijklmno').
	keys introduce: (Sequence string: 'fdhijklmno').
	keys introduce: (Sequence string: 'fehijklmno').
	keys introduce: (Sequence string: 'aghijklmno').
	keys introduce: (Sequence string: 'bghijklmno').
	keys introduce: (Sequence string: 'cghijklmno').
	keys introduce: (Sequence string: 'dghijklmno').
	keys introduce: (Sequence string: 'eghijklmno').
	^ keys!
*/
}
public ScruSet testValues() {
	MuSet vals;
	vals = MuSet.make();
	for (int ti = 0; ti <= lastTestValue(); ti ++ ) {
		vals.introduce((IntegerPos.make(ti)));
	}
	return vals;
/*
udanax-top.st:61742:SetTableTester methodsFor: 'private: testing'!
{ScruSet of: Heaper} testValues
	| vals {MuSet of: IntegerPos} |
	
	vals _ MuSet make.
	
	IntegerVar0 to: self lastTestValue do: [:ti {IntegerVar} |
		vals introduce: (ti integer)].
	^ vals!
*/
}
public SetTableTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:61754:SetTableTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:61757:SetTableTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public SetTableTester() {
/*

Generated during transformation
*/
}
}
