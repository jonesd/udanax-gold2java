/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.arrange;

import info.dgjones.abora.gold.arrange.Arrangement;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Generally represents a pair of an OrderSpec and a Region.  Arrangements map between
 * regions and primArrays.
 */
public class Arrangement extends Heaper {

/*
udanax-top.st:12528:
Heaper subclass: #Arrangement
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-arrange'!
*/
/*
udanax-top.st:12532:
Arrangement comment:
'Generally represents a pair of an OrderSpec and a Region.  Arrangements map between regions and primArrays.'!
*/
/*
udanax-top.st:12534:
(Arrangement getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Arrangement.class).setAttributes( new Set().add("DEFERRED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Copy elements into toArray arranged according to the receiver.
 * Copy them from fromArray arranged according to fromArrange.
 * The source region is fromRegion.  It gets tranformed by toDsp
 * into the toArray.
 */
public void copyElements(PrimArray toArray, Dsp toDsp, PrimArray fromArray, Arrangement fromArrange, XnRegion fromRegion) {
	Stepper stomper = fromRegion.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Position key = (Position) stomper.fetch();
		if (key == null) {
			continue ;
		}
		toArray.storeValue((indexOf((toDsp.of(key)))), (fromArray.fetchValue((fromArrange.indexOf(key)))));
	}
	stomper.destroy();
/*
udanax-top.st:12539:Arrangement methodsFor: 'accessing'!
{void} copyElements: toArray {PrimArray} with: toDsp {Dsp}
	with: fromArray {PrimArray} with: fromArrange {Arrangement} with: fromRegion {XnRegion}
	
	"Copy elements into toArray arranged according to the receiver. 
	 Copy them from fromArray arranged according to fromArrange.  
	 The source region is fromRegion.  It gets tranformed by toDsp
	 into the toArray."
	
	fromRegion stepper forEach: 
		[:key {Position} |
		toArray at: (self indexOf: (toDsp of: key)) DOTasLong
			storeValue: (fromArray fetchValue: (fromArrange indexOf: key) DOTasLong)]!
*/
}
/**
 * Return the index of position into my Region according to my OrderSpec.
 */
public int indexOf(Position position) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:12552:Arrangement methodsFor: 'accessing'!
{IntegerVar} indexOf: position {Position unused}
	"Return the index of position into my Region according to my OrderSpec."
	self subclassResponsibility!
*/
}
/**
 * Return the region of all the indices corresponding to positions in region.
 */
public IntegerRegion indicesOf(XnRegion region) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:12557:Arrangement methodsFor: 'accessing'!
{IntegerRegion} indicesOf: region {XnRegion}
	"Return the region of all the indices corresponding to positions in region."
	self subclassResponsibility!
*/
}
/**
 * Return the region that corresponds to a range of indices.
 */
public XnRegion keysOf(int start, int stop) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:12562:Arrangement methodsFor: 'accessing'!
{XnRegion} keysOf: start {Int32} with: stop {Int32}
	"Return the region that corresponds to a range of indices."
	self subclassResponsibility!
*/
}
/**
 * The region of positions in the arrangement
 */
public XnRegion region() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:12567:Arrangement methodsFor: 'accessing'!
{XnRegion} region
	"The region of positions in the arrangement"
	
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:12574:Arrangement methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public Arrangement(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:12579:Arrangement methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:12582:Arrangement methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public Arrangement() {
/*

Generated during transformation
*/
}
}
