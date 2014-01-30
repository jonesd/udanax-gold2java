/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.ByteShuffler;
import info.dgjones.abora.gold.proman.SimpleShuffler;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.ShuffleTester;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

/**
 * test the ByteShufflers
 */
public class ShuffleTester extends Tester {

/*
udanax-top.st:61844:
Tester subclass: #ShuffleTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:61848:
ShuffleTester comment:
'test the ByteShufflers '!
*/
/*
udanax-top.st:61850:
(ShuffleTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ShuffleTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
/**
 * self tryTest: #test1On:
 */
public void test1On(PrintWriter aStream) {
	UInt8Array simpleString;
	ByteShuffler shuffler;
	UInt8Array copy;
	shuffler = new SimpleShuffler();
	simpleString = UInt8Array.string("abcdefghijkl");
	copy = (UInt8Array) simpleString.copy();
	shuffler.shuffle(16, copy.gutsOf(), 6);
	copy.noMoreGuts();
	aStream.print("\n"+
"Shuffling 16: ");
	aStream.print(simpleString);
	aStream.print(" to: ");
	aStream.print(copy);
	aStream.print("\n"+
"");
	copy = (UInt8Array) simpleString.copy();
	shuffler.shuffle(32, copy.gutsOf(), 3);
	copy.noMoreGuts();
	aStream.print("Shuffling 32: ");
	aStream.print(simpleString);
	aStream.print(" to: ");
	aStream.print(copy);
	aStream.print("\n"+
"");
	simpleString = UInt8Array.string("");
	copy = (UInt8Array) simpleString.copy();
	shuffler.shuffle(16, copy.gutsOf(), 0);
	copy.noMoreGuts();
	aStream.print("Shuffling an empty string 16: ");
	aStream.print(copy);
	aStream.print("\n"+
"");
	copy = (UInt8Array) simpleString.copy();
	shuffler.shuffle(32, copy.gutsOf(), 0);
	copy.noMoreGuts();
	aStream.print("Shuffling an empty string 32: ");
	aStream.print(copy);
	aStream.print("\n"+
"");
	copy = UInt8Array.string("ab");
	shuffler.shuffle(16, copy.gutsOf(), 1);
	copy.noMoreGuts();
	aStream.print("Shuffling a tiny string 16: ");
	aStream.print(copy);
	aStream.print("\n"+
"");
	simpleString = UInt8Array.string("abcd");
	copy = (UInt8Array) simpleString.copy();
	shuffler.shuffle(32, copy.gutsOf(), 1);
	copy.noMoreGuts();
	aStream.print("Shuffling a tiny string 32: ");
	aStream.print(copy);
	aStream.print("\n"+
"");
	copy = (UInt8Array) simpleString.copy();
	shuffler.shuffle(16, copy.gutsOf(), 2);
	copy.noMoreGuts();
	aStream.print("Shuffling a small string 16: ");
	aStream.print(copy);
	aStream.print("\n"+
"");
	copy = UInt8Array.string("abcdef");
	shuffler.shuffle(16, copy.gutsOf(), 3);
	copy.noMoreGuts();
	aStream.print("Shuffling a small string 16: ");
	aStream.print(copy);
	aStream.print("\n"+
"");
/*
udanax-top.st:61855:ShuffleTester methodsFor: 'testing'!
{void} test1On: aStream {ostream reference} 
	"self tryTest: #test1On:"
	| simpleString {UInt8Array} shuffler {ByteShuffler} copy {UInt8Array} |
	shuffler _ SimpleShuffler create.
	simpleString _ UInt8Array string: 'abcdefghijkl'.
	copy _ simpleString copy cast: UInt8Array.
	shuffler shuffle: 16 with: copy gutsOf with: 6. copy noMoreGuts.
	aStream << '
Shuffling 16: ' << simpleString << ' to: ' << copy << '
'.
	copy _ simpleString copy cast: UInt8Array.
	shuffler shuffle: 32 with: copy gutsOf with: 3. copy noMoreGuts.
	aStream << 'Shuffling 32: ' << simpleString << ' to: ' << copy << '
'.
	simpleString _ UInt8Array string: ''.
	copy _ simpleString copy cast: UInt8Array.
	shuffler shuffle: 16 with: copy gutsOf with: Int32Zero. copy noMoreGuts.
	aStream << 'Shuffling an empty string 16: ' << copy << '
'.
	copy _ simpleString copy cast: UInt8Array.
	shuffler shuffle: 32 with: copy gutsOf with: Int32Zero. copy noMoreGuts.
	aStream << 'Shuffling an empty string 32: ' << copy << '
'.
	copy _ UInt8Array string: 'ab'.
	shuffler shuffle: 16 with: copy gutsOf with: 1. copy noMoreGuts.
	aStream << 'Shuffling a tiny string 16: ' << copy << '
'.
	simpleString _ UInt8Array string: 'abcd'.
	copy _ simpleString copy cast: UInt8Array.
	shuffler shuffle: 32 with: copy gutsOf with: 1. copy noMoreGuts.
	aStream << 'Shuffling a tiny string 32: ' << copy << '
'.
	copy _ simpleString copy cast: UInt8Array.
	shuffler shuffle: 16 with: copy gutsOf with: 2. copy noMoreGuts.
	aStream << 'Shuffling a small string 16: ' << copy << '
'.
	copy _ UInt8Array string: 'abcdef'.
	shuffler shuffle: 16 with: copy gutsOf with: 3. copy noMoreGuts.
	aStream << 'Shuffling a small string 16: ' << copy << '
'.!
*/
}
/**
 * ShuffleTester runTest
 */
public void allTestsOn(PrintWriter aStream) {
	aStream.print("testing shuffler\n"+
"");
	test1On(aStream);
/*
udanax-top.st:61903:ShuffleTester methodsFor: 'running tests'!
{void} allTestsOn: aStream {ostream reference} 
	"ShuffleTester runTest"
	
	aStream << 'testing shuffler
'.
	self test1On: aStream.!
*/
}
public ShuffleTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:61912:ShuffleTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:61915:ShuffleTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public ShuffleTester() {
/*

Generated during transformation
*/
}
}
