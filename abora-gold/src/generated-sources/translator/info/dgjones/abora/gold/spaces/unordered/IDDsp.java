/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.unordered;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.unordered.IDDsp;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.spaces.unordered.IdentityDsp;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * There are no non-trivial Dsps on IDs.
 */
public class IDDsp extends IdentityDsp {

	protected IDSpace mySpace;
/*
udanax-top.st:29676:
IdentityDsp subclass: #IDDsp
	instanceVariableNames: 'mySpace {IDSpace}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Unordered'!
*/
/*
udanax-top.st:29680:
IDDsp comment:
'There are no non-trivial Dsps on IDs.'!
*/
/*
udanax-top.st:29682:
(IDDsp getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #PSEUDO.COPY; yourself)!
*/
/*
udanax-top.st:29706:
IDDsp class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:29709:
(IDDsp getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #PSEUDO.COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IDDsp.class).setAttributes( new Set().add("CONCRETE").add("PSEUDOCOPY"));
/*

Generated during transformation: AddMethod
*/
}
public CoordinateSpace coordinateSpace() {
	return mySpace;
/*
udanax-top.st:29687:IDDsp methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^mySpace!
*/
}
public IDDsp() {
	super();
/*
udanax-top.st:29693:IDDsp methodsFor: 'creation'!
create
	super create!
*/
}
public IDDsp(IDSpace space) {
	super();
	mySpace = space;
/*
udanax-top.st:29696:IDDsp methodsFor: 'creation'!
create: space {IDSpace}
	super create.
	mySpace := space.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
/*
udanax-top.st:29703:IDDsp methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}!
*/
}
public static Heaper makeRcvr(Rcvr rcvr) {
	((SpecialistRcvr) rcvr).registerIbid(theDsp);
	return theDsp;
/*
udanax-top.st:29714:IDDsp class methodsFor: 'rcvr pseudo constructors'!
{Heaper} make.Rcvr: rcvr {Rcvr}
	(rcvr cast: SpecialistRcvr) registerIbid: theDsp.
	^theDsp!
*/
}
public static IDDsp make(IDSpace space) {
	return new IDDsp(space);
/*
udanax-top.st:29720:IDDsp class methodsFor: 'pseudo constructors'!
make: space {IDSpace}
	^self create: space!
*/
}
/**
 * @deprecated
 */
public static IDDsp make() {
	throw new PasseException();
/*
udanax-top.st:29726:IDDsp class methodsFor: 'smalltalk: passe'!
make
	self passe.
	^theDsp cast: IDDsp!
*/
}
public IDDsp(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
