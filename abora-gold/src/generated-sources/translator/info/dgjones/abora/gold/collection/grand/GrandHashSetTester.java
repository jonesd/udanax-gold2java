/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.grand;

import info.dgjones.abora.gold.collection.grand.GrandHashSet;
import info.dgjones.abora.gold.collection.grand.GrandHashSetTester;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.testing.MuSetTester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class GrandHashSetTester extends MuSetTester {

/*
udanax-top.st:60643:
MuSetTester subclass: #GrandHashSetTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Grand'!
*/
/*
udanax-top.st:60647:
(GrandHashSetTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandHashSetTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public void allTestsOn(PrintWriter oo) {
	oo.print("GrandHashSet testing\n"+
"");
	super.allTestsOn(oo);
	oo.print("End of GrandHashSet testing\n"+
"");
/*
udanax-top.st:60652:GrandHashSetTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference} 
	
	oo << 'GrandHashSet testing
'.
	super allTestsOn: oo.
	oo << 'End of GrandHashSet testing
'!
*/
}
public ScruSet generateSet() {
	return GrandHashSet.make(2);
/*
udanax-top.st:60662:GrandHashSetTester methodsFor: 'accessing'!
{ScruSet} generateSet
	^ GrandHashSet make: 2!
*/
}
public ScruSet generateSetContaining(Stepper stuff) {
	MuSet t;
	t = GrandHashSet.make(2);
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
udanax-top.st:60666:GrandHashSetTester methodsFor: 'accessing'!
{ScruSet} generateSetContaining: stuff {Stepper}
	| t {MuSet} |
	t _ GrandHashSet make: 2.
	stuff forEach: [:e {Heaper} |
		t store: e].
	^ t!
*/
}
public GrandHashSetTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:60675:GrandHashSetTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:60678:GrandHashSetTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public GrandHashSetTester() {
/*

Generated during transformation
*/
}
}
