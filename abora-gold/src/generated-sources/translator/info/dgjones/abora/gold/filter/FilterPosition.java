/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.filter;

import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.filter.FilterPosition;
import info.dgjones.abora.gold.filter.FilterSpace;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Encapsulates a Region in the baseSpace into a Position so that it can be treated as one
 * for polymorphism. See Filter.
 */
public class FilterPosition extends Position {

	protected XnRegion myRegion;
/*
udanax-top.st:31529:
Position subclass: #FilterPosition
	instanceVariableNames: 'myRegion {XnRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-filter'!
*/
/*
udanax-top.st:31533:
FilterPosition comment:
'Encapsulates a Region in the baseSpace into a Position so that it can be treated as one for polymorphism. See Filter.'!
*/
/*
udanax-top.st:31535:
(FilterPosition getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
*/
/*
udanax-top.st:31592:
FilterPosition class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:31595:
(FilterPosition getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FilterPosition.class).setAttributes( new Set().add("CONCRETE").add("ONCLIENT").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	return myRegion.hashForEqual() + 1;
/*
udanax-top.st:31540:FilterPosition methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^myRegion hashForEqual + 1!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof FilterPosition) {
		FilterPosition rap = (FilterPosition) other;
		return rap.baseRegion().isEqual(myRegion);
	}
	else {
		return false;
	}
/*
udanax-top.st:31544:FilterPosition methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper} 
	other 
		cast: FilterPosition into: [:rap |
			^rap baseRegion isEqual: myRegion]
		others: [^false].
	^false "fodder"!
*/
}
public XnRegion asRegion() {
	return (Filter.subsetFilter(coordinateSpace(), myRegion)).intersect((Filter.supersetFilter(coordinateSpace(), myRegion)));
/*
udanax-top.st:31554:FilterPosition methodsFor: 'accessing'!
{XnRegion} asRegion
	^(Filter subsetFilter: self coordinateSpace
			with: myRegion)
		intersect: (Filter supersetFilter: self coordinateSpace
			with: myRegion)!
*/
}
/**
 * Essential. The region in the base space which I represent.
 */
public XnRegion baseRegion() {
	return myRegion;
/*
udanax-top.st:31561:FilterPosition methodsFor: 'accessing'!
{XnRegion CLIENT INLINE} baseRegion
	"Essential. The region in the base space which I represent."
	
	^myRegion!
*/
}
public CoordinateSpace coordinateSpace() {
	return FilterSpace.make(myRegion.coordinateSpace());
/*
udanax-top.st:31566:FilterPosition methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^FilterSpace make: myRegion coordinateSpace!
*/
}
public FilterPosition(XnRegion region) {
	super();
	myRegion = region;
/*
udanax-top.st:31571:FilterPosition methodsFor: 'instance creation'!
create: region {XnRegion}
	super create.
	myRegion _ region.!
*/
}
/**
 * @deprecated
 */
public XnRegion region() {
	throw new PasseException();
/*
udanax-top.st:31577:FilterPosition methodsFor: 'smalltalk: passe'!
{XnRegion} region
	self passe!
*/
}
public FilterPosition(Rcvr receiver) {
	super(receiver);
	myRegion = (XnRegion) receiver.receiveHeaper();
/*
udanax-top.st:31583:FilterPosition methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myRegion _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myRegion);
/*
udanax-top.st:31587:FilterPosition methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myRegion.!
*/
}
/**
 * A position containing the given region.
 */
public static FilterPosition make(XnRegion region) {
	return new FilterPosition(region);
/*
udanax-top.st:31600:FilterPosition class methodsFor: 'pseudo constructors'!
make: region {XnRegion}
	"A position containing the given region."
	
	^FilterPosition create: region!
*/
}
/**
 * {XnRegion CLIENT} baseRegion
 */
public static void infostProtocol() {
/*
udanax-top.st:31607:FilterPosition class methodsFor: 'smalltalk: system'!
info.stProtocol
"{XnRegion CLIENT} baseRegion
"!
*/
}
public FilterPosition() {
/*

Generated during transformation
*/
}
}
