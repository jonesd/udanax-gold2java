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
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.ByteShuffler;
import info.dgjones.abora.gold.proman.SimpleShuffler;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * shuffle big-endian to little-endian transformation.
 */
public class SimpleShuffler extends ByteShuffler {

/*
udanax-top.st:13008:
ByteShuffler subclass: #SimpleShuffler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:13012:
SimpleShuffler comment:
'shuffle big-endian to little-endian transformation.'!
*/
/*
udanax-top.st:13014:
(SimpleShuffler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SimpleShuffler.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * shuffle alternating bytes.
 */
public void shuffle16(UInt8Array buffer, int count) {
	for (int index = 0; index < count * 2; index += 2 ) {
		int temp;
		temp = buffer.at(index);
		buffer.storeUInt(index, (buffer.at(index + 1)));
		buffer.storeUInt(index + 1, temp);
	}
	/* Removed translateOnly */
/*
udanax-top.st:13019:SimpleShuffler methodsFor: 'shuffle'!
{void} shuffle16: buffer {void star} with: count {Int32}
	" shuffle alternating bytes. "
	[0 almostTo: count * 2 by: 2 do:
		[:index | | temp {Uint8} | 
		temp _ buffer at: index.
		buffer at: index storeUInt: (buffer at: index + 1).
		buffer at: index + 1 storeUInt: temp]] smalltalkOnly.
	'UInt8 temp;
	UInt8 * base = (UInt8 *) buffer;
	for (Int32 index = 0 ; index < count * 2 ; index += 2)
		{
		temp = base[index];
		base[index] = base[index + 1];
		base[index + 1] = temp;
		}
' translateOnly.!
*/
}
/**
 * shuffle alternating words.
 */
public void shuffle32(UInt8Array buffer, int count) {
	for (int index = 0; index < count * 4; index += 4 ) {
		int temp;
		temp = buffer.at(index);
		buffer.storeUInt(index, (buffer.at(index + 3)));
		buffer.storeUInt(index + 3, temp);
		temp = buffer.at(index + 1);
		buffer.storeUInt(index + 1, (buffer.at(index + 2)));
		buffer.storeUInt(index + 2, temp);
	}
	/* Removed translateOnly */
/*
udanax-top.st:13038:SimpleShuffler methodsFor: 'shuffle'!
{void} shuffle32: buffer {void star} with: count {Int32}
	" shuffle alternating words. "
	[0 almostTo: count * 4 by: 4 do:
		[:index | | temp {UInt8} | 
		temp _ buffer at: index.
		buffer at: index storeUInt: (buffer at: index + 3).
		buffer at: index + 3 storeUInt: temp.
		temp _ buffer at: index + 1.
		buffer at: index + 1 storeUInt: (buffer at: index + 2).
		buffer at: index + 2 storeUInt: temp.
		]] smalltalkOnly.
	'UInt8 temp;
	UInt8 * base = (UInt8 *) buffer;
	for (Int32 index = 0 ; index < count * 4; index += 4)
		{
		temp = base[index];
		base[index] = base[index + 3];
		base[index + 3] = temp;
		temp = base[index + 1];
		base[index + 1] = base[index + 2];
		base[index + 2] = temp;
		}' translateOnly.!
*/
}
public void shuffle64(UInt8Array buffer, int count) {
	throw new UnimplementedException();
/*
udanax-top.st:13062:SimpleShuffler methodsFor: 'shuffle'!
{void} shuffle64: buffer {void star} with: count {Int32}
	self unimplemented.!
*/
}
public SimpleShuffler() {
/*

Generated during transformation
*/
}
public SimpleShuffler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
