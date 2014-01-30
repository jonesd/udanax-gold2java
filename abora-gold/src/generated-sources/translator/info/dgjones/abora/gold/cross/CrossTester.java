/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cross;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.SetAccumulator;
import info.dgjones.abora.gold.cross.CrossTester;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.cross.CrossSpace;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.spaces.integers.RegionTester;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class CrossTester extends RegionTester {

/*
udanax-top.st:59804:
RegionTester subclass: #CrossTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cross'!
*/
/*
udanax-top.st:59808:
(CrossTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CrossTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public ImmuSet initExamples() {
	SetAccumulator result;
	CrossSpace space;
	PtrArray regions;
	PtrArray crosses;
	result = SetAccumulator.make();
	space = CrossSpace.make(((PtrArray) (PrimSpec.pointer().arrayWithThree(IntegerSpace.make(), IntegerSpace.make(), IntegerSpace.make()))));
	regions = PtrArray.nulls(3);
	regions.store(0, (IntegerRegion.make(0, 10)));
	regions.store(1, (IntegerRegion.make(5, 15)));
	regions.store(2, (IntegerRegion.make(10, 20)));
	crosses = PtrArray.nulls(3);
	for (int i = 0; i < 27; i ++ ) {
		crosses.store(0, (regions.fetch(AboraSupport.modulo(i, 3))));
		crosses.store(1, (regions.fetch(AboraSupport.modulo(i / 3, 3))));
		crosses.store(2, (regions.fetch(i / 9)));
		result.step((space.crossOfRegions(crosses)));
	}
	return (ImmuSet) result.value();
/*
udanax-top.st:59813:CrossTester methodsFor: 'init'!
{ImmuSet of: XnRegion} initExamples
	| result {SetAccumulator} space {CrossSpace}
	  regions {PtrArray of: XnRegion} crosses {PtrArray of: XnRegion} |
	result := SetAccumulator make.
	space := CrossSpace make: ((PrimSpec pointer
		arrayWithThree: IntegerSpace make
		with: IntegerSpace make
		with: IntegerSpace make) cast: PtrArray).
	regions := PtrArray nulls: 3.
	regions at: Int32Zero store: (IntegerRegion make: Int32Zero with: 10).
	regions at: 1 store: (IntegerRegion make: 5 with: 15).
	regions at: 2 store: (IntegerRegion make: 10 with: 20).
	crosses := PtrArray nulls: 3.
	Int32Zero almostTo: 27 do: [ :i {Int32} |
		crosses at: Int32Zero store: (regions fetch: i \\ 3).
		crosses at: 1 store: (regions fetch: i // 3 \\ 3).
		crosses at: 2 store: (regions fetch: i // 9).
		result step: (space crossOfRegions: crosses)].
	^result value cast: ImmuSet!
*/
}
public CrossTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:59836:CrossTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:59839:CrossTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public CrossTester() {
/*

Generated during transformation
*/
}
}
