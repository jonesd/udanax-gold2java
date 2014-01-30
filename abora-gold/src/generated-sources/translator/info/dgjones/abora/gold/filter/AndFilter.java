/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.filter;

import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.filter.AndFilter;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.filter.FilterSpace;
import info.dgjones.abora.gold.filter.Joint;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class AndFilter extends Filter {

	protected ImmuSet mySubFilters;
/*
udanax-top.st:66642:
Filter subclass: #AndFilter
	instanceVariableNames: 'mySubFilters {ImmuSet of: Filter}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Filter'!
*/
/*
udanax-top.st:66646:
(AndFilter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
/*
udanax-top.st:66769:
AndFilter class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:66772:
(AndFilter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(AndFilter.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public AndFilter(FilterSpace cs, ImmuSet subs) {
	super(cs);
	mySubFilters = subs;
/*
udanax-top.st:66651:AndFilter methodsFor: 'creation'!
create: cs {FilterSpace} with: subs {ImmuSet of: Filter}
	super create: cs.
	mySubFilters _ subs!
*/
}
/**
 * tell whether a region passes this filter
 */
public boolean match(XnRegion region) {
	Stepper stomper = subFilters().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter sub = (Filter) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		if ( ! (sub.match(region))) {
			return false;
		}
	}
	stomper.destroy();
	return true;
/*
udanax-top.st:66657:AndFilter methodsFor: 'filtering'!
{BooleanVar} match: region {XnRegion}
	"tell whether a region passes this filter"
	self subFilters stepper forEach: [ :sub {Filter} |
		(sub match: region)
			ifFalse: [^false]].
	^true!
*/
}
/**
 * return the simplest filter for looking at the children
 */
public Filter pass(Joint parent) {
	XnRegion result;
	result = Filter.openFilter(coordinateSpace());
	Stepper stomper = subFilters().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter sub = (Filter) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		result = result.intersect((sub.pass(parent)));
	}
	stomper.destroy();
	return (Filter) result;
/*
udanax-top.st:66664:AndFilter methodsFor: 'filtering'!
{Filter} pass: parent {Joint}
	"return the simplest filter for looking at the children"
	| result {XnRegion} |
	result _ Filter openFilter: self coordinateSpace.
	self subFilters stepper forEach: [ :sub {Filter} |
		result _ result intersect: (sub pass: parent)].
	^result cast: Filter!
*/
}
public ImmuSet subFilters() {
	return mySubFilters;
/*
udanax-top.st:66672:AndFilter methodsFor: 'filtering'!
{ImmuSet of: Filter} subFilters
	^mySubFilters!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	subFilters().printOnWithSimpleSyntax(oo, "(", " && ", ")");
/*
udanax-top.st:66677:AndFilter methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name.
	self subFilters printOnWithSimpleSyntax: oo with: '(' with: ' && ' with: ')'!
*/
}
public int actualHashForEqual() {
	return (coordinateSpace().hashForEqual() ^ mySubFilters.hashForEqual()) ^ HashHelper.hashForEqual(this.getClass());
/*
udanax-top.st:66683:AndFilter methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^(self coordinateSpace hashForEqual bitXor: mySubFilters hashForEqual)
		bitXor: #cat.U.AndFilter hashForEqual!
*/
}
public boolean isAllFilter() {
	return false;
/*
udanax-top.st:66687:AndFilter methodsFor: 'testing'!
{BooleanVar} isAllFilter
	^false!
*/
}
public boolean isAnyFilter() {
	return false;
/*
udanax-top.st:66691:AndFilter methodsFor: 'testing'!
{BooleanVar} isAnyFilter
	
	^false!
*/
}
public boolean isEmpty() {
	return false;
/*
udanax-top.st:66695:AndFilter methodsFor: 'testing'!
{BooleanVar} isEmpty
	^false!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof AndFilter) {
		AndFilter af = (AndFilter) other;
		return af.subFilters().isEqual(subFilters());
	}
	else {
		return false;
	}
/*
udanax-top.st:66698:AndFilter methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: AndFilter into: [:af |
			^af subFilters isEqual: self subFilters]
		others: [^false].
	^false "fodder"!
*/
}
public boolean isFull() {
	return false;
/*
udanax-top.st:66706:AndFilter methodsFor: 'testing'!
{BooleanVar} isFull	
	^false!
*/
}
public XnRegion complement() {
	XnRegion result;
	result = Filter.closedFilter(coordinateSpace());
	Stepper stomper = subFilters().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		XnRegion sub = (XnRegion) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		result = result.unionWith(sub.complement());
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:66712:AndFilter methodsFor: 'operations'!
{XnRegion} complement
	| result {XnRegion} |
	result _ Filter closedFilter: self coordinateSpace.
	self subFilters stepper forEach: [ :sub {XnRegion} |
		result _ result unionWith: sub complement].
	^result!
*/
}
/**
 * return self or other if one is clearly a subset of the other, else NULL
 */
public XnRegion fetchSpecialSubset(XnRegion other) {
	Filter filter;
	XnRegion defaultRegion;
	filter = (Filter) other;
	defaultRegion = other;
	Stepper stomper = subFilters().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter subfilter = (Filter) stomper.fetch();
		if (subfilter == null) {
			continue ;
		}
		XnRegion a;
		XnRegion b;
		if ((a = subfilter.fetchSpecialSubset(filter)) == subfilter) {
			return this;
		}
		if ((b = filter.fetchSpecialSubset(subfilter)) == subfilter) {
			return this;
		}
		if ( ! ((a) == other || ((b) == other))) {
			defaultRegion = null;
		}
	}
	stomper.destroy();
	return defaultRegion;
/*
udanax-top.st:66722:AndFilter methodsFor: 'protected operations'!
{XnRegion} fetchSpecialSubset: other {XnRegion}
	"return self or other if one is clearly a subset of the other, else NULL"
	| filter {Filter} defaultRegion {XnRegion} |
	filter _ other cast: Filter.
	defaultRegion _ other.
	self subFilters stepper forEach: [ :subfilter {Filter} | | a {XnRegion} b {XnRegion} |
		(a _ subfilter fetchSpecialSubset: filter) == subfilter ifTrue: [^self].
		(b _ filter fetchSpecialSubset: subfilter) == subfilter ifTrue: [^self].
		((a basicCast: Heaper star) == other or: [(b basicCast: Heaper star) == other]) ifFalse: [defaultRegion _ NULL]].
	^defaultRegion!
*/
}
public Stepper intersectedFilters() {
	return mySubFilters.stepper();
/*
udanax-top.st:66735:AndFilter methodsFor: 'enumerating'!
{Stepper of: Filter} intersectedFilters
	^mySubFilters stepper!
*/
}
public Stepper unionedFilters() {
	return Stepper.itemStepper(this);
/*
udanax-top.st:66739:AndFilter methodsFor: 'enumerating'!
{Stepper of: Filter} unionedFilters
	^Stepper itemStepper: self!
*/
}
public XnRegion baseRegion() {
	throw new AboraRuntimeException(AboraRuntimeException.NOT_SIMPLE_ENOUGH);
/*
udanax-top.st:66745:AndFilter methodsFor: 'accessing'!
{XnRegion} baseRegion
	Heaper BLAST: #NotSimpleEnough.
	^NULL!
*/
}
public XnRegion relevantRegion() {
	XnRegion result;
	result = filterSpace().baseSpace().emptyRegion();
	Stepper stomper = mySubFilters.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Filter sub = (Filter) stomper.fetch();
		if (sub == null) {
			continue ;
		}
		result = result.unionWith(sub.relevantRegion());
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:66750:AndFilter methodsFor: 'accessing'!
{XnRegion} relevantRegion
	| result {XnRegion} |
	result := self filterSpace baseSpace emptyRegion.
	mySubFilters stepper forEach: [ :sub {Filter} |
		result := result unionWith: sub relevantRegion].
	^result!
*/
}
public AndFilter(Rcvr receiver) {
	super(receiver);
	mySubFilters = (ImmuSet) receiver.receiveHeaper();
/*
udanax-top.st:66760:AndFilter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	mySubFilters _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(mySubFilters);
/*
udanax-top.st:66764:AndFilter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: mySubFilters.!
*/
}
/**
 * assumes that the interactions between elements have already been removed
 */
public static Filter make(FilterSpace cs, ImmuSet subs) {
	if (subs.isEmpty()) {
		return (Filter) cs.fullRegion();
	}
	if (subs.count() == 1) {
		return (Filter) subs.theOne();
	}
	return new AndFilter(cs, subs);
/*
udanax-top.st:66777:AndFilter class methodsFor: 'pseudo constructors'!
{Filter} make: cs {FilterSpace} with: subs {ImmuSet of: Filter}
	"assumes that the interactions between elements have already been removed"
	
	subs isEmpty ifTrue: [^cs fullRegion cast: Filter].
	subs count = 1 ifTrue: [^subs theOne cast: Filter].
	^self create: cs with: subs!
*/
}
public AndFilter() {
/*

Generated during transformation
*/
}
}
