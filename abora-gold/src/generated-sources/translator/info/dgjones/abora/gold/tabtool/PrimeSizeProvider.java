/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.tabtool;

import info.dgjones.abora.gold.collection.basic.Int32Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tabtool.LPPrimeSizeProvider;
import info.dgjones.abora.gold.tabtool.PrimeSizeProvider;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * This is a non-stepper stepper that returns a stream of prime numbers.
 * SCPrimeSizeProvider rejects many primes to be nice for secondary clustering at the cost of
 * increased table size, LPPrimeSizeProvider does not claim to do this.
 * - michael 31 July 1991
 */
public class PrimeSizeProvider extends Heaper {

	protected Int32Array smallPrimeTable;
/*
udanax-top.st:33086:
Heaper subclass: #PrimeSizeProvider
	instanceVariableNames: 'smallPrimeTable {UInt32Array}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-tabtool'!
*/
/*
udanax-top.st:33090:
PrimeSizeProvider comment:
'This is a non-stepper stepper that returns a stream of prime numbers.
SCPrimeSizeProvider rejects many primes to be nice for secondary clustering at the cost of increased table size, LPPrimeSizeProvider does not claim to do this.
 - michael 31 July 1991'!
*/
/*
udanax-top.st:33096:
(PrimeSizeProvider getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
/*
udanax-top.st:33140:
PrimeSizeProvider class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:33143:
(PrimeSizeProvider getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimeSizeProvider.class).setAttributes( new Set().add("CONCRETE").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public int primeAfter(int attempt) {
	int val;
	int idx;
	int lim;
	idx = 0;
	val = attempt;
	lim = smallPrimeTable.count();
	while ((idx < lim) && (val > (smallPrimeTable.uIntAt(idx)))) {
		idx = idx + 1;
	}
	if (idx >= smallPrimeTable.count()) {
		return (attempt * 2) + 1;
	}
	else {
		return (smallPrimeTable.uIntAt(idx));
	}
/*
udanax-top.st:33101:PrimeSizeProvider methodsFor: 'accessing'!
{IntegerVar} primeAfter: attempt {IntegerVar}
	| val {UInt32} idx {UInt32} lim {UInt32} |
	
	idx _ UInt32Zero.
	val _ attempt DOTasLong.
	lim _ smallPrimeTable count.
	[(idx < lim) and: [val > (smallPrimeTable uIntAt: idx)]] 
		whileTrue: [idx _ idx + 1].
	idx >= smallPrimeTable count
		ifTrue: [^ (attempt * 2) + 1]
		ifFalse: [^ Integer IntegerVar: (smallPrimeTable uIntAt: idx)]!
*/
}
public int uInt32PrimeAfter(int attempt) {
	int val;
	int idx;
	int lim;
	idx = 0;
	val = attempt;
	lim = smallPrimeTable.count();
	while ((idx < lim) && (val > (smallPrimeTable.uIntAt(idx)))) {
		idx = idx + 1;
	}
	if (idx >= smallPrimeTable.count()) {
		return (attempt * 2) + 1;
	}
	else {
		return smallPrimeTable.uIntAt(idx);
	}
/*
udanax-top.st:33114:PrimeSizeProvider methodsFor: 'accessing'!
{UInt32} uInt32PrimeAfter: attempt {UInt32}
	| val {UInt32} idx {UInt32} lim {UInt32} |
	
	idx _ UInt32Zero.
	val _ attempt.
	lim _ smallPrimeTable count.
	[(idx < lim) and: [val > (smallPrimeTable uIntAt: idx)]] 
		whileTrue: [idx _ idx + 1].
	idx >= smallPrimeTable count
		ifTrue: [^ (attempt * 2) + 1]
		ifFalse: [^ smallPrimeTable uIntAt: idx]!
*/
}
public PrimeSizeProvider(Int32Array aSmallPrimeTable) {
	super();
	smallPrimeTable = aSmallPrimeTable;
/*
udanax-top.st:33129:PrimeSizeProvider methodsFor: 'creation'!
create: aSmallPrimeTable {UInt32Array}
	super create.
	smallPrimeTable _ aSmallPrimeTable.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:33135:PrimeSizeProvider methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:33137:PrimeSizeProvider methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static PrimeSizeProvider make() {
	return LPPrimeSizeProvider.make();
/*
udanax-top.st:33148:PrimeSizeProvider class methodsFor: 'creation'!
{PrimeSizeProvider INLINE} make
	^LPPrimeSizeProvider make!
*/
}
public static void initTimeNonInherited() {
/*
udanax-top.st:33153:PrimeSizeProvider class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	self REQUIRES: LPPrimeSizeProvider.!
*/
}
public PrimeSizeProvider() {
/*

Generated during transformation
*/
}
public PrimeSizeProvider(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
