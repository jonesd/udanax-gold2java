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
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class PrimIndexTableTester extends Tester {

/*
udanax-top.st:59571:
Tester subclass: #PrimIndexTableTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-primtab'!
*/
/*
udanax-top.st:59575:
(PrimIndexTableTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimIndexTableTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public void accessTestOn(PrintWriter oo) {
	PrimIndexTable tab;
	/* For this tests, I use as keys category pointers from the minimal xpp set */
	tab = PrimIndexTable.make(7);
	/* first test a few introduces */
	tab.introduce(AboraSupport.findCategory(Heaper.class), 1);
	tab.introduce(AboraSupport.findCategory(Category.class), 2);
	tab.introduce(AboraSupport.findCategory(PrimIndexTable.class), 3);
	if ((tab.get(AboraSupport.findCategory(Heaper.class))) != 1) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(AboraSupport.findCategory(Category.class))) != 2) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(AboraSupport.findCategory(PrimIndexTable.class))) != 3) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	/* now do some more to cause a grow. */
	tab.introduce(AboraSupport.findCategory(Tester.class), 4);
	tab.introduce(AboraSupport.findCategory(PrimIndexTableTester.class), 5);
	tab.introduce(AboraSupport.findCategory(Recipe.class), 7);
	tab.introduce(AboraSupport.findCategory(BootMaker.class), 8);
	if ((tab.get(AboraSupport.findCategory(Heaper.class))) != 1) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(AboraSupport.findCategory(Category.class))) != 2) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(AboraSupport.findCategory(PrimIndexTable.class))) != 3) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(AboraSupport.findCategory(Tester.class))) != 4) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(AboraSupport.findCategory(PrimIndexTableTester.class))) != 5) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(AboraSupport.findCategory(Recipe.class))) != 7) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(AboraSupport.findCategory(BootMaker.class))) != 8) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	/* Now remove some stuff. */
	tab.remove(AboraSupport.findCategory(Recipe.class));
	if ((tab.get(AboraSupport.findCategory(Heaper.class))) != 1) {
		throw new AboraRuntimeException(AboraRuntimeException.REMOVE_FOULED);
	}
	if ((tab.get(AboraSupport.findCategory(Category.class))) != 2) {
		throw new AboraRuntimeException(AboraRuntimeException.REMOVE_FOULED);
	}
	if ((tab.get(AboraSupport.findCategory(PrimIndexTable.class))) != 3) {
		throw new AboraRuntimeException(AboraRuntimeException.REMOVE_FOULED);
	}
	if ((tab.get(AboraSupport.findCategory(Tester.class))) != 4) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(AboraSupport.findCategory(PrimIndexTableTester.class))) != 5) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
	if ((tab.get(AboraSupport.findCategory(BootMaker.class))) != 8) {
		throw new AboraRuntimeException(AboraRuntimeException.INTRODUCE_FAILED);
	}
/*
udanax-top.st:59580:PrimIndexTableTester methodsFor: 'tests'!
{void} accessTestOn: oo {ostream reference}
	| tab {PrimIndexTable} |
	"For this tests, I use as keys category pointers from the minimal xpp set"
	tab := PrimIndexTable make: 7.
	"first test a few introduces"
	tab at: Heaper introduce: 1.
	tab at: Category introduce: 2.
	tab at: PrimIndexTable introduce: 3.
	(tab get: Heaper) ~~ 1 ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: Category) ~~ 2 ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: PrimIndexTable) ~~ 3 ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	"now do some more to cause a grow."
	tab at: Tester introduce: 4.
	tab at: PrimIndexTableTester introduce: 5.
	tab at: Recipe introduce: 7.
	tab at: BootMaker introduce: 8.
	(tab get: Heaper) ~~ 1 ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: Category) ~~ 2 ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: PrimIndexTable) ~~ 3 ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: Tester) ~~ 4 ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: PrimIndexTableTester) ~~ 5 ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: Recipe) ~~ 7 ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: BootMaker) ~~ 8 ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	"Now remove some stuff."
	tab remove: Recipe.
	(tab get: Heaper) ~~ 1 ifTrue: [ Heaper BLAST: #RemoveFouled ].
	(tab get: Category) ~~ 2 ifTrue: [ Heaper BLAST: #RemoveFouled ].
	(tab get: PrimIndexTable) ~~ 3 ifTrue: [ Heaper BLAST: #RemoveFouled ].
	(tab get: Tester) ~~ 4 ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: PrimIndexTableTester) ~~ 5 ifTrue: [ Heaper BLAST: #IntroduceFailed ].
	(tab get: BootMaker) ~~ 8 ifTrue: [ Heaper BLAST: #IntroduceFailed ].!
*/
}
public void allTestsOn(PrintWriter oo) {
	accessTestOn(oo);
/*
udanax-top.st:59614:PrimIndexTableTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference} 
	self accessTestOn: oo!
*/
}
public PrimIndexTableTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:59620:PrimIndexTableTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:59623:PrimIndexTableTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public PrimIndexTableTester() {
/*

Generated during transformation
*/
}
}
