/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.integers;

import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.SetAccumulator;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerRegionTester;
import info.dgjones.abora.gold.spaces.integers.RegionTester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class IntegerRegionTester extends RegionTester {

/*
udanax-top.st:60072:
RegionTester subclass: #IntegerRegionTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Integers'!
*/
/*
udanax-top.st:60076:
(IntegerRegionTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IntegerRegionTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
/**
 * IntegerRegionTester runTest
 */
public ImmuSet initExamples() {
	SetAccumulator acc;
	acc = SetAccumulator.make();
	acc.step(IntegerRegion.make());
	acc.step(IntegerRegion.make().complement());
	acc.step((IntegerRegion.make(3, 7)));
	acc.step((IntegerRegion.make(3, 7)).complement());
	acc.step((IntegerRegion.after(5)));
	acc.step((IntegerRegion.before(5)));
	return (ImmuSet) acc.value();
/*
udanax-top.st:60081:IntegerRegionTester methodsFor: 'init'!
{ImmuSet of: XnRegion} initExamples
	"IntegerRegionTester runTest"
	| acc {SetAccumulator of: XnRegion} |
	acc _ SetAccumulator make.
	acc step: IntegerRegion make.
	acc step: IntegerRegion make complement.
	acc step: (IntegerRegion make: 3 with: 7).
	acc step: (IntegerRegion make: 3 with: 7) complement.
	acc step: (IntegerRegion after: 5).
	acc step: (IntegerRegion before: 5).
	^acc value cast: ImmuSet!
*/
}
public IntegerRegionTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:60095:IntegerRegionTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:60098:IntegerRegionTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public IntegerRegionTester() {
/*

Generated during transformation
*/
}
}
