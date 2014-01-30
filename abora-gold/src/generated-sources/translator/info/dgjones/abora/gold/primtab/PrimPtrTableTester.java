/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.primtab;

import info.dgjones.abora.gold.cobbler.BootMaker;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimIndexTable;
import info.dgjones.abora.gold.primtab.PrimIndexTableTester;
import info.dgjones.abora.gold.primtab.PrimPtrTable;
import info.dgjones.abora.gold.primtab.PrimPtrTableTester;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class PrimPtrTableTester extends Tester {

/*
udanax-top.st:59626:
Tester subclass: #PrimPtrTableTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-primtab'!
*/
/*
udanax-top.st:59630:
(PrimPtrTableTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimPtrTableTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public void accessTestOn(PrintWriter oo) {
	PrimPtrTable tab;
	/* For this tests, I use as keys category pointers from the minimal xpp set */
	tab = PrimPtrTable.make(7);
	/* first test a few introduces */
	tab.introduce(1, AboraSupport.findCategory(Heaper.class));
	tab.introduce(2, AboraSupport.findCategory(Category.class));
	tab.introduce(3, AboraSupport.findCategory(PrimIndexTable.class));
	if ((tab.get(1)) != AboraSupport.findCategory(Heaper.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(2)) != AboraSupport.findCategory(Category.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(3)) != AboraSupport.findCategory(PrimIndexTable.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	/* now do some more to cause a grow. */
	tab.introduce(4, AboraSupport.findCategory(Tester.class));
	tab.introduce(5, AboraSupport.findCategory(PrimIndexTableTester.class));
	tab.introduce(7, AboraSupport.findCategory(Recipe.class));
	tab.introduce(8, AboraSupport.findCategory(BootMaker.class));
	if ((tab.get(1)) != AboraSupport.findCategory(Heaper.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(2)) != AboraSupport.findCategory(Category.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(3)) != AboraSupport.findCategory(PrimIndexTable.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(4)) != AboraSupport.findCategory(Tester.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(5)) != AboraSupport.findCategory(PrimIndexTableTester.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(7)) != AboraSupport.findCategory(Recipe.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(8)) != AboraSupport.findCategory(BootMaker.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	/* Now remove some stuff. */
	tab.remove(7);
	if ((tab.get(1)) != AboraSupport.findCategory(Heaper.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.REMOVE_FOULED);
	}
	if ((tab.get(2)) != AboraSupport.findCategory(Category.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.REMOVE_FOULED);
	}
	if ((tab.get(3)) != AboraSupport.findCategory(PrimIndexTable.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.REMOVE_FOULED);
	}
	if ((tab.get(4)) != AboraSupport.findCategory(Tester.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.REMOVE_FOULED);
	}
	if ((tab.get(5)) != AboraSupport.findCategory(PrimIndexTableTester.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.REMOVE_FOULED);
	}
	if ((tab.get(8)) != AboraSupport.findCategory(BootMaker.class)) {
		throw new AboraRuntimeException(AboraRuntimeException.REMOVE_FOULED);
	}
/*
udanax-top.st:59635:PrimPtrTableTester methodsFor: 'tests'!
{void} accessTestOn: oo {ostream reference}
	| tab {PrimPtrTable} |
	"For this tests, I use as keys category pointers from the minimal xpp set"
	tab := PrimPtrTable make: 7.
	"first test a few introduces"
	tab at: 1 introduce: Heaper.
	tab at: 2 introduce: Category.
	tab at: 3 introduce: PrimIndexTable.
	(tab get: 1) ~~ Heaper ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: 2) ~~ Category ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: 3) ~~ PrimIndexTable ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	"now do some more to cause a grow."
	tab at: 4 introduce: Tester.
	tab at: 5 introduce: PrimIndexTableTester.
	tab at: 7 introduce: Recipe.
	tab at: 8 introduce: BootMaker.
	(tab get: 1) ~~ Heaper ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: 2) ~~ Category ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: 3) ~~ PrimIndexTable ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: 4) ~~ Tester ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: 5) ~~ PrimIndexTableTester ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: 7) ~~ Recipe ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: 8) ~~ BootMaker ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	"Now remove some stuff."
	tab remove: 7.
	(tab get: 1) ~~ Heaper ifTrue: [ Heaper BLAST: #RemoveFouled ].
	(tab get: 2) ~~ Category ifTrue: [ Heaper BLAST: #RemoveFouled ].
	(tab get: 3) ~~ PrimIndexTable ifTrue: [ Heaper BLAST: #RemoveFouled ].
	(tab get: 4) ~~ Tester ifTrue: [ Heaper BLAST: #RemoveFouled ].
	(tab get: 5) ~~ PrimIndexTableTester ifTrue: [ Heaper BLAST: #RemoveFouled ].
	(tab get: 8) ~~ BootMaker ifTrue: [ Heaper BLAST: #RemoveFouled ].!
*/
}
public void allTestsOn(PrintWriter oo) {
	accessTestOn(oo);
/*
udanax-top.st:59669:PrimPtrTableTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference} 
	self accessTestOn: oo!
*/
}
public PrimPtrTableTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:59675:PrimPtrTableTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:59678:PrimPtrTableTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public PrimPtrTableTester() {
/*

Generated during transformation
*/
}
}
