/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.packer;

import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.packer.SpareStageSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class SpareStageSpace extends Thunk {

	protected int myCruftedSnarfCount;
	protected int myFlocksPerSnarf;
	protected static int CruftedSnarfCount;
	protected static int FlocksPerSnarf;
/*
udanax-top.st:57965:
Thunk subclass: #SpareStageSpace
	instanceVariableNames: '
		myCruftedSnarfCount {Int32}
		myFlocksPerSnarf {Int32}'
	classVariableNames: '
		CruftedSnarfCount {Int32} 
		FlocksPerSnarf {Int32} '
	poolDictionaries: ''
	category: 'Xanadu-packer'!
*/
/*
udanax-top.st:57973:
(SpareStageSpace getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
/*
udanax-top.st:57995:
SpareStageSpace class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:57998:
(SpareStageSpace getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SpareStageSpace.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public void execute() {
	CruftedSnarfCount = myCruftedSnarfCount;
	FlocksPerSnarf = myFlocksPerSnarf;
/*
udanax-top.st:57978:SpareStageSpace methodsFor: 'execute'!
{void} execute
	CruftedSnarfCount := myCruftedSnarfCount.
	FlocksPerSnarf := myFlocksPerSnarf.!
*/
}
public SpareStageSpace(Rcvr receiver) {
	super(receiver);
	myCruftedSnarfCount = receiver.receiveInt32();
	myFlocksPerSnarf = receiver.receiveInt32();
/*
udanax-top.st:57984:SpareStageSpace methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCruftedSnarfCount _ receiver receiveInt32.
	myFlocksPerSnarf _ receiver receiveInt32.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendInt32(myCruftedSnarfCount);
	xmtr.sendInt32(myFlocksPerSnarf);
/*
udanax-top.st:57989:SpareStageSpace methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendInt32: myCruftedSnarfCount.
	xmtr sendInt32: myFlocksPerSnarf.!
*/
}
public static void linkTimeNonInherited() {
	CruftedSnarfCount = 7;
	FlocksPerSnarf = 100;
/*
udanax-top.st:58003:SpareStageSpace class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	CruftedSnarfCount := 7.
	FlocksPerSnarf := 100.!
*/
}
public static int cruftedSnarfsGuess() {
	return CruftedSnarfCount;
/*
udanax-top.st:58009:SpareStageSpace class methodsFor: 'accessing'!
{Int32} cruftedSnarfsGuess
	^ CruftedSnarfCount!
*/
}
public static int flocksPerSnarfGuess() {
	return FlocksPerSnarf;
/*
udanax-top.st:58012:SpareStageSpace class methodsFor: 'accessing'!
{Int32} flocksPerSnarfGuess
	^ FlocksPerSnarf!
*/
}
public SpareStageSpace() {
/*

Generated during transformation
*/
}
}
