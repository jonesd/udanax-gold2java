/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.testing;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.testing.HashSetTester;
import info.dgjones.abora.gold.testing.ImmuSetTester;
import info.dgjones.abora.gold.testing.SetTester;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

public class SetTester extends Tester {

/*
udanax-top.st:61760:
Tester subclass: #SetTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Testing'!
*/
/*
udanax-top.st:61764:
(SetTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SetTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public void allTestsOn(PrintWriter oo) {
	Tester aTester;
	aTester = new HashSetTester();
	aTester.allTestsOn(oo);
	aTester = new ImmuSetTester();
	aTester.allTestsOn(oo);
/*
udanax-top.st:61769:SetTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference} 
	| aTester {Tester} |
	aTester _ HashSetTester create.
	aTester allTestsOn: oo.
	aTester _ ImmuSetTester create.
	aTester allTestsOn: oo!
*/
}
public SetTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:61779:SetTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:61782:SetTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public SetTester() {
/*

Generated during transformation
*/
}
}
