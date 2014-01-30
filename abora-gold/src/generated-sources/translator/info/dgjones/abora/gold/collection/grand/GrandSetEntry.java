/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.grand;

import info.dgjones.abora.gold.collection.grand.GrandEntry;
import info.dgjones.abora.gold.collection.grand.GrandSetEntry;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class GrandSetEntry extends GrandEntry {

/*
udanax-top.st:6568:
GrandEntry subclass: #GrandSetEntry
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Grand'!
*/
/*
udanax-top.st:6572:
(GrandSetEntry getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #COPY; add: #SHEPHERD.ANCESTOR; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:6604:
GrandSetEntry class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:6607:
(GrandSetEntry getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #COPY; add: #SHEPHERD.ANCESTOR; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandSetEntry.class).setAttributes( new Set().add("COPY").add("SHEPHERDANCESTOR").add("LOCKED").add("NOTATYPE").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean compare(Heaper anObj) {
	return value().isEqual(anObj);
/*
udanax-top.st:6577:GrandSetEntry methodsFor: 'testing'!
{BooleanVar} compare: anObj {Heaper | Position}
	^ self value isEqual: anObj!
*/
}
public boolean matches(GrandEntry anEntry) {
	return value().isEqual(anEntry.value());
/*
udanax-top.st:6580:GrandSetEntry methodsFor: 'testing'!
{BooleanVar} matches: anEntry {GrandEntry}
	^ self value isEqual: anEntry value!
*/
}
public GrandSetEntry(Heaper value, int hash) {
	super(value, hash);
	newShepherd();
	remember();
/*
udanax-top.st:6585:GrandSetEntry methodsFor: 'protected: creation'!
create: value {Heaper} with: hash {UInt32}
	super create: value with: hash.
	self newShepherd.
	self remember!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print("GrandSetEntry(hash=");
	aStream.print(hashForEqual());
	aStream.print(", value=");
	aStream.print(value());
	aStream.print(")");
/*
udanax-top.st:6592:GrandSetEntry methodsFor: 'printing'!
{void} printOn: aStream {ostream reference}
	aStream << 'GrandSetEntry(hash=' << self hashForEqual << ', value=' << self value << ')'!
*/
}
public GrandSetEntry(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:6597:GrandSetEntry methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:6600:GrandSetEntry methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static GrandEntry make(Heaper value, int hash) {
	return new GrandSetEntry(value, hash);
/*
udanax-top.st:6612:GrandSetEntry class methodsFor: 'create'!
{GrandEntry} make: value {Heaper} with: hash {UInt32}
	^ self create: value with: hash!
*/
}
public GrandSetEntry() {
/*

Generated during transformation
*/
}
}
