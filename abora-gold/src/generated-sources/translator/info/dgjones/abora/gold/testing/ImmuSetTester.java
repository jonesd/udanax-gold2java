/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.testing;

import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.testing.ImmuSetTester;
import info.dgjones.abora.gold.testing.ScruSetTester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class ImmuSetTester extends ScruSetTester {

/*
udanax-top.st:60502:
ScruSetTester subclass: #ImmuSetTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Testing'!
*/
/*
udanax-top.st:60506:
(ImmuSetTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ImmuSetTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public void allTestsOn(PrintWriter oo) {
	oo.print("ImmuSet testing\n"+
"");
	super.allTestsOn(oo);
	oo.print("End of ImmuSet testing\n"+
"");
/*
udanax-top.st:60511:ImmuSetTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference} 
	oo << 'ImmuSet testing
'.
	super allTestsOn: oo.
	oo << 'End of ImmuSet testing
'!
*/
}
public ScruSet generateSet() {
	return ImmuSet.make();
/*
udanax-top.st:60520:ImmuSetTester methodsFor: 'accessing'!
{ScruSet} generateSet
	^ ImmuSet make!
*/
}
public ScruSet generateSetContaining(Stepper stuff) {
	MuSet t;
	t = MuSet.make();
	Stepper stomper = stuff;
	for (; stomper.hasValue(); stomper.step()) {
		Heaper e = (Heaper) stomper.fetch();
		if (e == null) {
			continue ;
		}
		t.store(e);
	}
	stomper.destroy();
	return t.asImmuSet();
/*
udanax-top.st:60524:ImmuSetTester methodsFor: 'accessing'!
{ScruSet} generateSetContaining: stuff {Stepper}
	| t {MuSet} |
	t _ MuSet make.
	stuff forEach: [:e {Heaper} |
		t store: e].
	^ t asImmuSet!
*/
}
public ImmuSetTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:60534:ImmuSetTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:60537:ImmuSetTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public ImmuSetTester() {
/*

Generated during transformation
*/
}
}
