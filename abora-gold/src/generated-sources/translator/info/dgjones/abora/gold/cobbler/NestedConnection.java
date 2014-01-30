/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cobbler;

import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.cobbler.NestedConnection;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * We just made an object that wraps another object, so the connection needs to wrap the
 * connection by which that other object was obtained.
 */
public class NestedConnection extends Connection {

	protected Category myCategory;
	protected Heaper myHeaper;
	protected Connection mySub;
/*
udanax-top.st:13977:
Connection subclass: #NestedConnection
	instanceVariableNames: '
		myCategory {Category}
		myHeaper {Heaper}
		mySub {Connection}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cobbler'!
*/
/*
udanax-top.st:13984:
NestedConnection comment:
'We just made an object that wraps another object, so the connection needs to wrap the connection by which that other object was obtained.'!
*/
/*
udanax-top.st:13986:
(NestedConnection getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:14011:
NestedConnection class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:14014:
(NestedConnection getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(NestedConnection.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	return myCategory;
/*
udanax-top.st:13991:NestedConnection methodsFor: 'accessing'!
{Category} bootCategory
	^myCategory!
*/
}
public Heaper bootHeaper() {
	return myHeaper;
/*
udanax-top.st:13994:NestedConnection methodsFor: 'accessing'!
{Heaper} bootHeaper
	^myHeaper!
*/
}
public NestedConnection(Category cat, Heaper heaper, Connection sub) {
	super();
	myCategory = cat;
	myHeaper = heaper;
	mySub = sub;
/*
udanax-top.st:13999:NestedConnection methodsFor: 'creation'!
create: cat {Category} with: heaper {Heaper} with: sub {Connection}
	super create.
	myCategory _ cat.
	myHeaper _ heaper.
	mySub _ sub!
*/
}
public void destruct() {
	mySub.destroy();
	myHeaper.destroy();
	super.destruct();
/*
udanax-top.st:14005:NestedConnection methodsFor: 'creation'!
{void} destruct
	mySub destroy.
	myHeaper destroy.
	super destruct!
*/
}
public static Connection make(Category cat, Heaper heaper, Connection sub) {
	return new NestedConnection(cat, heaper, sub);
/*
udanax-top.st:14019:NestedConnection class methodsFor: 'creation'!
{Connection} make: cat {Category} with: heaper {Heaper} with: sub {Connection}
	^self create: cat with: heaper with: sub!
*/
}
public NestedConnection() {
/*

Generated during transformation
*/
}
public NestedConnection(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
