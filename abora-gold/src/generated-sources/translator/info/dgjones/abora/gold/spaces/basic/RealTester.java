/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.basic;

import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.sets.SetAccumulator;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.RealTester;
import info.dgjones.abora.gold.spaces.integers.RegionTester;
import info.dgjones.abora.gold.tumbler.RealPos;
import info.dgjones.abora.gold.tumbler.RealRegion;
import info.dgjones.abora.gold.tumbler.RealSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class RealTester extends RegionTester {

/*
udanax-top.st:60101:
RegionTester subclass: #RealTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Basic'!
*/
/*
udanax-top.st:60105:
(RealTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RealTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public ImmuSet initExamples() {
	SetAccumulator reals;
	SetAccumulator result;
	ImmuSet base;
	reals = SetAccumulator.make();
	reals.step((RealPos.make(0.0)));
	reals.step((RealPos.make(1.0)));
	reals.step((RealPos.make(2.0)));
	result = SetAccumulator.make();
	Stepper stomper = ((ScruSet) reals.value()).stepper();
	for (; stomper.hasValue(); stomper.step()) {
		RealPos real = (RealPos) stomper.fetch();
		if (real == null) {
			continue ;
		}
		result.step((RealSpace.make().above(real, true)));
		result.step((RealSpace.make().above(real, false)));
		result.step((RealSpace.make().below(real, true)));
		result.step((RealSpace.make().below(real, false)));
	}
	stomper.destroy();
	base = (ImmuSet) result.value();
	Stepper stomper2 = base.stepper();
	for (; stomper2.hasValue(); stomper2.step()) {
		RealRegion r = (RealRegion) stomper2.fetch();
		if (r == null) {
			continue ;
		}
		Stepper stomper3 = base.stepper();
		for (; stomper3.hasValue(); stomper3.step()) {
			RealRegion r2 = (RealRegion) stomper3.fetch();
			if (r2 == null) {
				continue ;
			}
			if (r.hashForEqual() < r2.hashForEqual()) {
				result.step((r.unionWith(r2)));
				result.step((r.intersect(r2)));
			}
		}
		stomper3.destroy();
	}
	stomper2.destroy();
	return (ImmuSet) result.value();
/*
udanax-top.st:60110:RealTester methodsFor: 'deferred: init'!
{ImmuSet of: XnRegion} initExamples
	| reals {SetAccumulator} result {SetAccumulator} base {ImmuSet} |
	reals := SetAccumulator make.
	reals step: (RealPos make: 0.0).
	reals step: (RealPos make: 1.0).
	reals step: (RealPos make: 2.0).
	result := SetAccumulator make.
	(reals value cast: ScruSet) stepper forEach: [ :real {RealPos} |
		result step: (RealSpace make above: real with: true).
		result step: (RealSpace make above: real with: false).
		result step: (RealSpace make below: real with: true).
		result step: (RealSpace make below: real with: false)].
	base := result value cast: ImmuSet.
	base stepper forEach: [ :r {RealRegion} |
		base stepper forEach: [ :r2 {RealRegion} |
			r hashForEqual < r2 hashForEqual ifTrue:
				[result step: (r unionWith: r2).
				result step: (r intersect: r2)]]].
	^result value cast: ImmuSet!
*/
}
public RealTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:60132:RealTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:60135:RealTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public RealTester() {
/*

Generated during transformation
*/
}
}
