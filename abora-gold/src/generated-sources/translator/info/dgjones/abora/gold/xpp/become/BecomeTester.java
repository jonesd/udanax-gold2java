/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.become;

import info.dgjones.abora.gold.chameleon.Chameleon;
import info.dgjones.abora.gold.chameleon.Moth;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.become.BecomeTester;
import java.io.PrintWriter;

public class BecomeTester extends Tester {

/*
Xanadu-Xpp-Become.st:0:
Tester subclass: #BecomeTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Become'!
*/
/*
Xanadu-Xpp-Become.st:4:
(BecomeTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BecomeTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
/**
 * BecomeTester runTest
 */
public void allTestsOn(PrintWriter oo) {
	test1On(oo);
/*
Xanadu-Xpp-Become.st:9:BecomeTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference} 
	"BecomeTester runTest"
	self test1On: oo.!
*/
}
/**
 * BecomeTester runTest: #test1On:
 */
public void test1On(PrintWriter oo) {
	Chameleon cham;
	cham = Moth.make();
	cham.explain(oo);
	((Moth) cham).molt();
	cham.explain(oo);
/*
Xanadu-Xpp-Become.st:14:BecomeTester methodsFor: 'testing'!
{void} test1On: oo {ostream reference} 
	"BecomeTester runTest: #test1On:"
	| cham {Chameleon} |
	cham _ Moth make.
	cham explain: oo.
	(cham cast: Moth) molt.
	cham explain: oo.!
*/
}
public BecomeTester(Rcvr receiver) {
	super(receiver);
/*
Xanadu-Xpp-Become.st:24:BecomeTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
Xanadu-Xpp-Become.st:27:BecomeTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public BecomeTester() {
/*

Generated during transformation
*/
}
}
