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
import info.dgjones.abora.gold.collection.sets.SetAccumulator;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.filter.FilterSpace;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.FilterTester;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.spaces.integers.RegionTester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class FilterTester extends RegionTester {

	protected ImmuSet myBaseRegions;
/*
udanax-top.st:59842:
RegionTester subclass: #FilterTester
	instanceVariableNames: 'myBaseRegions {ImmuSet NOCOPY of: XnRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Basic'!
*/
/*
udanax-top.st:59846:
(FilterTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FilterTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public FilterTester() {
	super();
	myBaseRegions = null;
/*
udanax-top.st:59851:FilterTester methodsFor: 'creation'!
create
	super create.
	myBaseRegions _ NULL.!
*/
}
public ImmuSet initExamples() {
	SetAccumulator acc;
	SetAccumulator result;
	FilterSpace space;
	ImmuSet simple;
	acc = SetAccumulator.make();
	acc.step((IntegerRegion.make()));
	acc.step((IntegerRegion.make(3)));
	acc.step((IntegerRegion.make(1, 7)));
	acc.step((IntegerRegion.make(5, 7)));
	acc.step((IntegerRegion.make(5, 9)));
	Stepper stomper = ((ImmuSet) acc.value()).stepper();
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion each = (XnRegion) stomper.fetch();
		if (each == null) {
			continue ;
		}
		acc.step(each.complement());
	}
	stomper.destroy();
	myBaseRegions = ((ImmuSet) acc.value());
	result = SetAccumulator.make();
	space = FilterSpace.make(IntegerSpace.make());
	Stepper stomper2 = myBaseRegions.stepper();
	for (; stomper2.hasValue(); stomper2.step()) {
		XnRegion region = (XnRegion) stomper2.fetch();
		if (region == null) {
			continue ;
		}
		result.step((Filter.subsetFilter(space, region)));
		result.step((Filter.notSubsetFilter(space, region)));
		result.step((Filter.supersetFilter(space, region)));
		result.step((Filter.notSupersetFilter(space, region)));
	}
	stomper2.destroy();
	simple = ((ImmuSet) result.value());
	Stepper stomper3 = simple.stepper();
	for (; stomper3.hasValue(); stomper3.step()) {
		Filter one = (Filter) stomper3.fetch();
		if (one == null) {
			continue ;
		}
		Stepper stomper4 = simple.stepper();
		for (; stomper4.hasValue(); stomper4.step()) {
			Filter two = (Filter) stomper4.fetch();
			if (two == null) {
				continue ;
			}
			if (one.hashForEqual() < two.hashForEqual()) {
				result.step((one.unionWith(two)));
				result.step((one.intersect(two)));
			}
		}
		stomper4.destroy();
	}
	stomper3.destroy();
	return ((ImmuSet) result.value());
/*
udanax-top.st:59857:FilterTester methodsFor: 'init'!
{ImmuSet of: XnRegion} initExamples
	| acc {SetAccumulator of: XnRegion} 
	  result {SetAccumulator of: Filter} 
	  space {FilterSpace} 
	  simple {ImmuSet} |
	acc _ SetAccumulator make.
	acc step: (IntegerRegion make).
	acc step: (IntegerRegion make: 3).
	acc step: (IntegerRegion make: 1 with: 7).
	acc step: (IntegerRegion make: 5 with: 7).
	acc step: (IntegerRegion make: 5 with: 9).
	(acc value cast: ImmuSet) stepper forEach: [ :each {XnRegion} |
		acc step: each complement].
	myBaseRegions _ (acc value cast: ImmuSet).
	result _ SetAccumulator make.
	space _ FilterSpace make: IntegerSpace make.
	myBaseRegions stepper forEach: [ :region {XnRegion} |
		result step: (Filter subsetFilter: space with: region).
		result step: (Filter notSubsetFilter: space with: region).
		result step: (Filter supersetFilter: space with: region).
		result step: (Filter notSupersetFilter: space with: region)].
	simple _ (result value cast: ImmuSet).
	simple stepper forEach: [ :one {Filter} |
		simple stepper forEach: [ :two {Filter} |
			one hashForEqual < two hashForEqual ifTrue:
				[result step: (one unionWith: two).
				result step: (one intersect: two)]]].
	^(result value cast: ImmuSet)!
*/
}
public void binaryCheck(XnRegion a, XnRegion b) {
	Filter af;
	Filter bf;
	af = (Filter) a;
	bf = (Filter) b;
	super.binaryCheck(af, bf);
	if (af.isSubsetOf(bf)) {
		Stepper stomper = myBaseRegions.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			XnRegion each = (XnRegion) stomper.fetch();
			if (each == null) {
				continue ;
			}
			if ( ! ( ! (af.match(each)) || (bf.match(each)))) {
				throw new AboraAssertionException("subset/match test failed");
			}
		}
		stomper.destroy();
	}
/*
udanax-top.st:59888:FilterTester methodsFor: 'testing'!
{void} binaryCheck: a {XnRegion} with: b {XnRegion}
	| af {Filter} bf {Filter} |
	af _ a cast: Filter.
	bf _ b cast: Filter.
	super binaryCheck: af with: bf.
	(af isSubsetOf: bf) ifTrue:
		[myBaseRegions stepper forEach: [ :each {XnRegion} |
			((af match: each) not or: [bf match: each]) assert: 'subset/match test failed']]!
*/
}
public void unaryCheck(XnRegion a) {
	super.unaryCheck(a);
	Stepper stomper = myBaseRegions.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion each = (XnRegion) stomper.fetch();
		if (each == null) {
			continue ;
		}
		if ( ! ((((Filter) a).match(each)) != (((Filter) a.complement()).match(each)))) {
			throw new AboraAssertionException("complement/match test failed");
		}
	}
	stomper.destroy();
/*
udanax-top.st:59898:FilterTester methodsFor: 'testing'!
{void} unaryCheck: a {XnRegion}
	super unaryCheck: a.
	myBaseRegions stepper forEach: [ :each {XnRegion} |
		(((a cast: Filter) match: each) ~~ ((a complement cast: Filter) match: each)) assert: 'complement/match test failed']!
*/
}
public void restartFilterTester(Rcvr rcvr) {
	myBaseRegions = null;
/*
udanax-top.st:59906:FilterTester methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartFilterTester: rcvr {Rcvr unused default: NULL}
	myBaseRegions _ NULL.!
*/
}
public FilterTester(Rcvr receiver) {
	super(receiver);
	restartFilterTester(receiver);
/*
udanax-top.st:59911:FilterTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	self restartFilterTester: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:59915:FilterTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
}
