/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.sheph;

import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.counter.Counter;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimPtrTable;
import info.dgjones.abora.gold.sheph.ShepherdLockTester;
import info.dgjones.abora.gold.sheph.ShepherdLocked;
import info.dgjones.abora.gold.stacker.StackExaminer;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

public class ShepherdLockTester extends Tester {

/*
udanax-top.st:61785:
Tester subclass: #ShepherdLockTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-sheph'!
*/
/*
udanax-top.st:61789:
(ShepherdLockTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ShepherdLockTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
/**
 * ShepherdLockTester runTest
 */
public void allTestsOn(PrintWriter oo) {
	Connection conn;
	conn = Connection.make(AboraSupport.findCategory(Counter.class));
	test1On(oo);
	conn.destroy();
/*
udanax-top.st:61794:ShepherdLockTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference} 
	"ShepherdLockTester runTest"
	| conn {Connection} |
	conn _ Connection make: Counter.
	self test1On: oo.
	conn destroy!
*/
}
/**
 * ShepherdLockTester runTest: #test1On:
 */
public void test1On(PrintWriter oo) {
	ShepherdLocked aLocked;
	ShepherdLocked anUnlocked;
	PrimPtrTable stackPtrs;
	stackPtrs = StackExaminer.pointersOnStack();
	aLocked = ShepherdLocked.makeLocked();
	anUnlocked = ShepherdLocked.makeUnlocked();
	oo.print("aLocked Shepherd ");
	AboraSupport.smalltalkOnly();
	{
		if ( ! ((stackPtrs.fetch(aLocked.asOop())) == null)) {
			oo.print("is locked");
		}
		else {
			oo.print("is not locked");
		}
	}
	AboraSupport.translateOnly();
	{
		/* if (stackPtrs->fetch((Int32)(void*)aLocked) == NULL) {
		oo << \"is locked\";
	} else {
		oo << \"is not locked\";
	} */
	}
	if ( ! (aLocked.isReallyUnlocked())) {
		oo.print("; is really locked");
	}
	else {
		oo.print("; is really not locked");
	}
	oo.print("\n"+
"anUnlocked Shepherd ");
	AboraSupport.smalltalkOnly();
	{
		if ( ! ((stackPtrs.fetch(anUnlocked.asOop())) == null)) {
			oo.print("is locked");
		}
		else {
			oo.print("is not locked");
		}
	}
	AboraSupport.translateOnly();
	{
		/* if (stackPtrs->fetch((Int32)(void*)anUnlocked) == NULL) {
		oo << \"is locked\";
	} else {
		oo << \"is not locked\";
	} */
	}
	if ( ! (anUnlocked.isReallyUnlocked())) {
		oo.print("; is really locked");
	}
	else {
		oo.print("; is really not locked");
	}
	oo.print("\n"+
"");
/*
udanax-top.st:61802:ShepherdLockTester methodsFor: 'testing'!
{void} test1On: oo {ostream reference} 
	"ShepherdLockTester runTest: #test1On:"
	| aLocked {ShepherdLocked} anUnlocked {ShepherdLocked} stackPtrs {PrimPtrTable} |
	stackPtrs _ StackExaminer pointersOnStack.
	aLocked _ ShepherdLocked makeLocked.
	anUnlocked _ ShepherdLocked makeUnlocked.
	oo << 'aLocked Shepherd '.
	[(stackPtrs fetch: aLocked asOop) == NULL
		ifFalse: [oo << 'is locked']
		ifTrue: [oo << 'is not locked']] smalltalkOnly.
	'if (stackPtrs->fetch((Int32)(void*)aLocked) == NULL) {
		oo << "is locked";
	} else {
		oo << "is not locked";
	}' translateOnly.
	aLocked isReallyUnlocked
		ifFalse: [oo << '; is really locked']
		ifTrue: [oo << '; is really not locked'].
	oo << '
anUnlocked Shepherd '.
	[(stackPtrs fetch: anUnlocked asOop) == NULL
		ifFalse: [oo << 'is locked']
		ifTrue: [oo << 'is not locked']] smalltalkOnly.
	'if (stackPtrs->fetch((Int32)(void*)anUnlocked) == NULL) {
		oo << "is locked";
	} else {
		oo << "is not locked";
	}' translateOnly.
	anUnlocked isReallyUnlocked
		ifFalse: [oo << '; is really locked']
		ifTrue: [oo << '; is really not locked'].
	oo << '
'.!
*/
}
public ShepherdLockTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:61838:ShepherdLockTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:61841:ShepherdLockTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public ShepherdLockTester() {
/*

Generated during transformation
*/
}
}
