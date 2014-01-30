/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.proman;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.ByteShuffler;
import info.dgjones.abora.gold.proman.NoShuffler;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * No transformation.
 */
public class NoShuffler extends ByteShuffler {

/*
udanax-top.st:12988:
ByteShuffler subclass: #NoShuffler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:12992:
NoShuffler comment:
'No transformation.'!
*/
/*
udanax-top.st:12994:
(NoShuffler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(NoShuffler.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Do nothing.
 */
public void shuffle16(UInt8Array buffer, int count) {
/*
udanax-top.st:12999:NoShuffler methodsFor: 'shuffle'!
{void} shuffle16: buffer {void star} with: count {Int32}
	"Do nothing."!
*/
}
/**
 * Do nothing.
 */
public void shuffle32(UInt8Array buffer, int count) {
/*
udanax-top.st:13002:NoShuffler methodsFor: 'shuffle'!
{void} shuffle32: buffer {void star} with: count {Int32}
	"Do nothing."!
*/
}
/**
 * Do nothing.
 */
public void shuffle64(UInt8Array buffer, int count) {
/*
udanax-top.st:13005:NoShuffler methodsFor: 'shuffle'!
{void} shuffle64: buffer {void star} with: count {Int32}
	"Do nothing."!
*/
}
public NoShuffler() {
/*

Generated during transformation
*/
}
public NoShuffler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
