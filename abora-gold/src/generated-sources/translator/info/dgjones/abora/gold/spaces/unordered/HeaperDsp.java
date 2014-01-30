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
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.unordered.HeaperDsp;
import info.dgjones.abora.gold.spaces.unordered.HeaperSpace;
import info.dgjones.abora.gold.spaces.unordered.IdentityDsp;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class HeaperDsp extends IdentityDsp {

/*
udanax-top.st:29638:
IdentityDsp subclass: #HeaperDsp
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Unordered'!
*/
/*
udanax-top.st:29642:
(HeaperDsp getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:29661:
HeaperDsp class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:29664:
(HeaperDsp getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HeaperDsp.class).setAttributes( new Set().add("PSEUDOCOPY").add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public CoordinateSpace coordinateSpace() {
	return HeaperSpace.make();
/*
udanax-top.st:29647:HeaperDsp methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	^HeaperSpace make!
*/
}
public HeaperDsp() {
	super();
/*
udanax-top.st:29653:HeaperDsp methodsFor: 'creation'!
create
	super create!
*/
}
public void sendSelfTo(Xmtr xmtr) {
/*
udanax-top.st:29658:HeaperDsp methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}!
*/
}
public static Dsp make() {
	return (HeaperDsp) ((IdentityDsp) theDsp);
/*
udanax-top.st:29669:HeaperDsp class methodsFor: 'pseudo constructors'!
{Dsp} make 
	^(theDsp basicCast: IdentityDsp) basicCast: HeaperDsp!
*/
}
public static Heaper makeRcvr(Rcvr rcvr) {
	((SpecialistRcvr) rcvr).registerIbid(theDsp);
	return theDsp;
/*
udanax-top.st:29672:HeaperDsp class methodsFor: 'pseudo constructors'!
{Heaper} make.Rcvr: rcvr {Rcvr}
	(rcvr cast: SpecialistRcvr) registerIbid: theDsp.
	^theDsp!
*/
}
public HeaperDsp(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
