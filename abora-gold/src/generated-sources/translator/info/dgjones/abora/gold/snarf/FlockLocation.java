/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.FlockLocation;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * Represent the location of a flock on disk.  This ID of the snarf in which the flock is
 * contained, and the index of the flock within that snarf.  This information side-effect
 * free, even in subclasses.
 */
public class FlockLocation extends Heaper {

	protected int mySnarfID;
	protected int myIndex;
/*
udanax-top.st:26480:
Heaper subclass: #FlockLocation
	instanceVariableNames: '
		mySnarfID {SnarfID}
		myIndex {Int4}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:26486:
FlockLocation comment:
'Represent the location of a flock on disk.  This ID of the snarf in which the flock is contained, and the index of the flock within that snarf.  This information side-effect free, even in subclasses.'!
*/
/*
udanax-top.st:26488:
(FlockLocation getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
/*
udanax-top.st:26525:
FlockLocation class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:26528:
(FlockLocation getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FlockLocation.class).setAttributes( new Set().add("CONCRETE").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * This is used to set the index when a flock is bumped from its snarf and forwarded by
 * way of the new flocks table
 */
public void index(int anIndex) {
	myIndex = anIndex;
/*
udanax-top.st:26493:FlockLocation methodsFor: 'protected: accessing'!
{void} index: anIndex {Int32}
	"This is used to set the index when a flock is bumped from its snarf and forwarded by
	way of the new flocks table"
	 myIndex := anIndex!
*/
}
public int index() {
	return myIndex;
/*
udanax-top.st:26500:FlockLocation methodsFor: 'accessing'!
{Int32 INLINE} index
	 ^myIndex!
*/
}
public int snarfID() {
	return mySnarfID;
/*
udanax-top.st:26503:FlockLocation methodsFor: 'accessing'!
{SnarfID INLINE} snarfID
	 ^mySnarfID!
*/
}
public FlockLocation(int snarfID, int index) {
	super();
	mySnarfID = snarfID;
	myIndex = index;
/*
udanax-top.st:26508:FlockLocation methodsFor: 'creation'!
create: snarfID {SnarfID} with: index {Int32}
	super create.
	mySnarfID _ snarfID.
	myIndex _ index!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(mySnarfID);
	oo.print(", ");
	oo.print(myIndex);
	oo.print(")");
/*
udanax-top.st:26515:FlockLocation methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << mySnarfID <<', ' << myIndex << ')'!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:26520:FlockLocation methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:26522:FlockLocation methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static FlockLocation make(int snarfID, int index) {
	return new FlockLocation(snarfID, index);
/*
udanax-top.st:26533:FlockLocation class methodsFor: 'creation'!
make: snarfID {SnarfID} with: index {Int32}
	^self create: snarfID with: index!
*/
}
public FlockLocation() {
/*

Generated during transformation
*/
}
public FlockLocation(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
