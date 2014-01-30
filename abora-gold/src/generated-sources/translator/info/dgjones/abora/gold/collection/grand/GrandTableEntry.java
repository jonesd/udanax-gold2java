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
import info.dgjones.abora.gold.collection.grand.GrandTableEntry;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class GrandTableEntry extends GrandEntry {

	protected Position keyInternal;
/*
udanax-top.st:6615:
GrandEntry subclass: #GrandTableEntry
	instanceVariableNames: 'keyInternal {Position}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Grand'!
*/
/*
udanax-top.st:6619:
(GrandTableEntry getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #COPY; add: #SHEPHERD.ANCESTOR; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:6667:
GrandTableEntry class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:6670:
(GrandTableEntry getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #COPY; add: #SHEPHERD.ANCESTOR; add: #LOCKED; add: #NOT.A.TYPE; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandTableEntry.class).setAttributes( new Set().add("COPY").add("SHEPHERDANCESTOR").add("LOCKED").add("NOTATYPE").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print("GrandTableEntry(hash=");
	aStream.print(hashForEqual());
	aStream.print(", key=");
	aStream.print(keyInternal);
	aStream.print(", value=");
	aStream.print(value());
	aStream.print(")");
/*
udanax-top.st:6624:GrandTableEntry methodsFor: 'printing'!
{void} printOn: aStream {ostream reference}
	aStream << 'GrandTableEntry(hash=' << self hashForEqual << ', key='<< keyInternal << ', value=' << self value << ')'!
*/
}
public Position key() {
	return keyInternal;
/*
udanax-top.st:6629:GrandTableEntry methodsFor: 'accessing'!
{Position} key
	^ keyInternal!
*/
}
public Position position() {
	return keyInternal;
/*
udanax-top.st:6632:GrandTableEntry methodsFor: 'accessing'!
{Position} position
	^ keyInternal!
*/
}
public boolean compare(Heaper anObj) {
	return keyInternal.isEqual(anObj);
/*
udanax-top.st:6637:GrandTableEntry methodsFor: 'testing'!
{BooleanVar} compare: anObj {Heaper | Position}
	^ keyInternal isEqual: anObj!
*/
}
public int contentsHash() {
	return super.contentsHash() ^ keyInternal.hashForEqual();
/*
udanax-top.st:6640:GrandTableEntry methodsFor: 'testing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: keyInternal hashForEqual!
*/
}
public boolean matches(GrandEntry anEntry) {
	return keyInternal.isEqual(((GrandTableEntry) anEntry).position());
/*
udanax-top.st:6645:GrandTableEntry methodsFor: 'testing'!
{BooleanVar} matches: anEntry {GrandEntry}
	^ keyInternal isEqual: (anEntry cast: GrandTableEntry) position!
*/
}
public GrandTableEntry(Heaper value, Position key, int hash) {
	super(value, hash);
	keyInternal = key;
	newShepherd();
	remember();
/*
udanax-top.st:6650:GrandTableEntry methodsFor: 'protected: creation'!
create: value {Heaper} with: key {Position} with: hash {UInt32}
	super create: value with: hash.
	keyInternal _ key.
	self newShepherd.
	self remember!
*/
}
public GrandTableEntry(Rcvr receiver) {
	super(receiver);
	keyInternal = (Position) receiver.receiveHeaper();
/*
udanax-top.st:6658:GrandTableEntry methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	keyInternal _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(keyInternal);
/*
udanax-top.st:6662:GrandTableEntry methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: keyInternal.!
*/
}
public static GrandEntry make(Heaper value, Position key, int hash) {
	return new GrandTableEntry(value, key, hash);
/*
udanax-top.st:6675:GrandTableEntry class methodsFor: 'create'!
{GrandEntry} make: value {Heaper} with: key {Position} with: hash {UInt32}
	^ self create: value with: key with: hash!
*/
}
public GrandTableEntry() {
/*

Generated during transformation
*/
}
}
