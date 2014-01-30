/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.lock;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.lock.NoScrambler;
import info.dgjones.abora.gold.lock.Scrambler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Does not actually scramble anything.
 */
public class NoScrambler extends Scrambler {

/*
udanax-top.st:45104:
Scrambler subclass: #NoScrambler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-lock'!
*/
/*
udanax-top.st:45108:
NoScrambler comment:
'Does not actually scramble anything.'!
*/
/*
udanax-top.st:45110:
(NoScrambler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:45129:
NoScrambler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:45132:
(NoScrambler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(NoScrambler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public UInt8Array scramble(UInt8Array clear) {
	return clear;
/*
udanax-top.st:45115:NoScrambler methodsFor: 'scrambling'!
{UInt8Array} scramble: clear {UInt8Array}
	^clear!
*/
}
public int actualHashForEqual() {
	return HashHelper.hashForEqual(this.getClass()) + 1;
/*
udanax-top.st:45121:NoScrambler methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^#cat.U.NoScrambler hashForEqual + 1!
*/
}
public boolean isEqual(Heaper other) {
	return other instanceof NoScrambler;
/*
udanax-top.st:45124:NoScrambler methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	^other isKindOf: NoScrambler!
*/
}
public static void initTimeNonInherited() {
	Scrambler.DEFINEUSCRAMBLER("NoScrambler", NoScrambler.make());
/*
udanax-top.st:45137:NoScrambler class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	Scrambler DEFINE.U.SCRAMBLER: 'NoScrambler' with: NoScrambler make!
*/
}
public static Scrambler make() {
	return new NoScrambler();
/*
udanax-top.st:45143:NoScrambler class methodsFor: 'pseudo constructors'!
{Scrambler} make
	^self create!
*/
}
public NoScrambler() {
/*

Generated during transformation
*/
}
public NoScrambler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
