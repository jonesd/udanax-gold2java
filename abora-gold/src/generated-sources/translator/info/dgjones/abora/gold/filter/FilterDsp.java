/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.filter;

import info.dgjones.abora.gold.filter.FilterDsp;
import info.dgjones.abora.gold.filter.FilterSpace;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.unordered.IdentityDsp;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * There are no non-trivial Dsps currently defined on a FilterSpace.
 * It would be possible to define them with reference to a Dsp in the baseSpace, as
 * filterDsp->of(filter)->match(R) iff filter->match(filterDsp->baseDsp()->inverseOf(R))
 * for all R in the base space.
 * However, we have not yet found a use for them.
 */
public class FilterDsp extends IdentityDsp {

	protected FilterSpace myCS;
/*
udanax-top.st:29569:
IdentityDsp subclass: #FilterDsp
	instanceVariableNames: 'myCS {FilterSpace}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Filter'!
*/
/*
udanax-top.st:29573:
FilterDsp comment:
'There are no non-trivial Dsps currently defined on a FilterSpace.
It would be possible to define them with reference to a Dsp in the baseSpace, as
	filterDsp->of(filter)->match(R) iff filter->match(filterDsp->baseDsp()->inverseOf(R))
		for all R in the base space.
However, we have not yet found a use for them.'!
*/
/*
udanax-top.st:29580:
(FilterDsp getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
/*
udanax-top.st:29619:
FilterDsp class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:29622:
(FilterDsp getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FilterDsp.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public FilterDsp(CoordinateSpace cs) {
	super();
	myCS = (FilterSpace) cs;
/*
udanax-top.st:29585:FilterDsp methodsFor: 'creation'!
create: cs {CoordinateSpace}
	super create.
	myCS _ cs cast: FilterSpace.!
*/
}
public int actualHashForEqual() {
	return myCS.hashForEqual() + HashHelper.hashForEqual(this.getClass());
/*
udanax-top.st:29591:FilterDsp methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^myCS hashForEqual + #cat.U.FilterDsp hashForEqual!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof FilterDsp) {
		FilterDsp fd = (FilterDsp) other;
		return fd.coordinateSpace().isEqual(myCS);
	}
	else {
		return false;
	}
/*
udanax-top.st:29594:FilterDsp methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: FilterDsp into: [:fd |
			^fd coordinateSpace isEqual: myCS]
		others: [^false].
	^false "fodder"!
*/
}
public CoordinateSpace coordinateSpace() {
	return myCS;
/*
udanax-top.st:29604:FilterDsp methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^myCS!
*/
}
public FilterDsp(Rcvr receiver) {
	super(receiver);
	myCS = (FilterSpace) receiver.receiveHeaper();
/*
udanax-top.st:29610:FilterDsp methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCS _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myCS);
/*
udanax-top.st:29614:FilterDsp methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCS.!
*/
}
/**
 * An identity Dsp on the given FilterSpace.
 */
public static FilterDsp make(FilterSpace cs) {
	return new FilterDsp(cs);
/*
udanax-top.st:29627:FilterDsp class methodsFor: 'pseudo constructors'!
make: cs {FilterSpace}
	"An identity Dsp on the given FilterSpace."
	
	^FilterDsp create: cs!
*/
}
public static void suppressInitTimeInherited() {
/*
udanax-top.st:29634:FilterDsp class methodsFor: 'smalltalk: initialization'!
suppressInitTimeInherited!
*/
}
public static void suppressLinkTimeInherited() {
/*
udanax-top.st:29636:FilterDsp class methodsFor: 'smalltalk: initialization'!
suppressLinkTimeInherited!
*/
}
public FilterDsp() {
/*

Generated during transformation
*/
}
}
