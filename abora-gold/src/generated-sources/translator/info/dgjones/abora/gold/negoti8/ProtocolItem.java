/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.negoti8;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.negoti8.ProtocolItem;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class ProtocolItem extends Heaper {

	protected String myName;
	protected ProtocolItem myNext;
	protected Heaper myItem;
/*
udanax-top.st:40984:
Heaper subclass: #ProtocolItem
	instanceVariableNames: '
		myName {char star}
		myNext {ProtocolItem}
		myItem {Heaper}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-negoti8'!
*/
/*
udanax-top.st:40991:
(ProtocolItem getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ProtocolItem.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper get(String name) {
	if ((name.compareTo(myName)) == 0) {
		return myItem;
	}
	if (myNext != null) {
		return myNext.get(name);
	}
	else {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_LIST);
	}
/*
udanax-top.st:40996:ProtocolItem methodsFor: 'accessing'!
{Heaper} get: name {char star}
	(String strcmp: name with: myName) == Int32Zero ifTrue:
		[ ^ myItem ].
	myNext ~~ NULL
		ifTrue: [ ^ myNext get: name ]
		ifFalse: [ Heaper BLAST: #NotInList ].
	^NULL "fodder"!
*/
}
public ProtocolItem(String name, Heaper item, ProtocolItem next) {
	super();
	myName = name;
	myItem = item;
	myNext = next;
/*
udanax-top.st:41006:ProtocolItem methodsFor: 'create'!
create: name {char star} with: item {Heaper} with: next {ProtocolItem} 
	super create.
	myName _ name.
	myItem _ item.
	myNext _ next!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:41014:ProtocolItem methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public ProtocolItem() {
/*

Generated during transformation
*/
}
public ProtocolItem(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
