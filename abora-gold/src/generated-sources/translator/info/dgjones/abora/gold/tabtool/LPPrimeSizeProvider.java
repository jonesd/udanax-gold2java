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
public class LPPrimeSizeProvider extends PrimeSizeProvider {

	protected static LPPrimeSizeProvider MySoleProvider;
/*
udanax-top.st:33157:
PrimeSizeProvider subclass: #LPPrimeSizeProvider
	instanceVariableNames: ''
	classVariableNames: 'MySoleProvider {LPPrimeSizeProvider} '
	poolDictionaries: ''
	category: 'Xanadu-tabtool'!
*/
/*
udanax-top.st:33161:
LPPrimeSizeProvider comment:
'This is a non-stepper stepper that returns a stream of prime numbers.
SCPrimeSizeProvider rejects many primes to be nice for secondary clustering at the cost of increased table size, LPPrimeSizeProvider does not claim to do this.
 - michael 31 July 1991'!
*/
/*
udanax-top.st:33167:
(LPPrimeSizeProvider getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
/*
udanax-top.st:33182:
LPPrimeSizeProvider class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:33185:
(LPPrimeSizeProvider getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(LPPrimeSizeProvider.class).setAttributes( new Set().add("CONCRETE").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public LPPrimeSizeProvider(Int32Array aSmallPrimeTable) {
	super(aSmallPrimeTable);
/*
udanax-top.st:33172:LPPrimeSizeProvider methodsFor: 'creation'!
create: aSmallPrimeTable {UInt32Array}
	super create: aSmallPrimeTable!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:33177:LPPrimeSizeProvider methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:33179:LPPrimeSizeProvider methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static PrimeSizeProvider make() {
	return MySoleProvider;
/*
udanax-top.st:33190:LPPrimeSizeProvider class methodsFor: 'make'!
{LPPrimeSizeProvider INLINE} make
	^ MySoleProvider!
*/
}
public static Int32Array primeTable() {
	Int32Array smallPrimeTable;
	smallPrimeTable = Int32Array.make(71);
	smallPrimeTable.storeUInt(0, 7);
	smallPrimeTable.storeUInt(1, 19);
	smallPrimeTable.storeUInt(2, 41);
	smallPrimeTable.storeUInt(3, 67);
	smallPrimeTable.storeUInt(4, 101);
	smallPrimeTable.storeUInt(5, 139);
	smallPrimeTable.storeUInt(6, 191);
	smallPrimeTable.storeUInt(7, 241);
	smallPrimeTable.storeUInt(8, 313);
	smallPrimeTable.storeUInt(9, 401);
	smallPrimeTable.storeUInt(10, 499);
	smallPrimeTable.storeUInt(11, 617);
	smallPrimeTable.storeUInt(12, 751);
	smallPrimeTable.storeUInt(13, 911);
	smallPrimeTable.storeUInt(14, 1091);
	smallPrimeTable.storeUInt(15, 1297);
	smallPrimeTable.storeUInt(16, 1543);
	smallPrimeTable.storeUInt(17, 1801);
	smallPrimeTable.storeUInt(18, 2113);
	smallPrimeTable.storeUInt(19, 2459);
	smallPrimeTable.storeUInt(20, 2851);
	smallPrimeTable.storeUInt(21, 3331);
	smallPrimeTable.storeUInt(22, 3833);
	smallPrimeTable.storeUInt(23, 4421);
	smallPrimeTable.storeUInt(24, 5059);
	smallPrimeTable.storeUInt(25, 5801);
	smallPrimeTable.storeUInt(26, 6607);
	smallPrimeTable.storeUInt(27, 7547);
	smallPrimeTable.storeUInt(28, 8599);
	smallPrimeTable.storeUInt(29, 9697);
	smallPrimeTable.storeUInt(30, 11004);
	smallPrimeTable.storeUInt(31, 12479);
	smallPrimeTable.storeUInt(32, 14057);
	smallPrimeTable.storeUInt(33, 15803);
	smallPrimeTable.storeUInt(34, 17881);
	smallPrimeTable.storeUInt(35, 20117);
	smallPrimeTable.storeUInt(36, 22573);
	smallPrimeTable.storeUInt(37, 28499);
	smallPrimeTable.storeUInt(38, 32003);
	smallPrimeTable.storeUInt(39, 35759);
	smallPrimeTable.storeUInt(40, 40009);
	smallPrimeTable.storeUInt(41, 44729);
	smallPrimeTable.storeUInt(42, 50053);
	smallPrimeTable.storeUInt(43, 55933);
	smallPrimeTable.storeUInt(44, 62483);
	smallPrimeTable.storeUInt(45, 69911);
	smallPrimeTable.storeUInt(46, 77839);
	smallPrimeTable.storeUInt(47, 86929);
	smallPrimeTable.storeUInt(48, 96787);
	smallPrimeTable.storeUInt(49, 108041);
	smallPrimeTable.storeUInt(50, 120473);
	smallPrimeTable.storeUInt(51, 134087);
	smallPrimeTable.storeUInt(52, 149287);
	smallPrimeTable.storeUInt(53, 166303);
	smallPrimeTable.storeUInt(54, 185063);
	smallPrimeTable.storeUInt(55, 205957);
	smallPrimeTable.storeUInt(56, 228887);
	smallPrimeTable.storeUInt(57, 254663);
	smallPrimeTable.storeUInt(58, 282833);
	smallPrimeTable.storeUInt(59, 313979);
	smallPrimeTable.storeUInt(60, 347287);
	smallPrimeTable.storeUInt(61, 384317);
	smallPrimeTable.storeUInt(62, 424667);
	smallPrimeTable.storeUInt(63, 468841);
	smallPrimeTable.storeUInt(64, 517073);
	smallPrimeTable.storeUInt(65, 569927);
	smallPrimeTable.storeUInt(66, 627553);
	smallPrimeTable.storeUInt(67, 691183);
	smallPrimeTable.storeUInt(68, 760657);
	smallPrimeTable.storeUInt(69, 836483);
	smallPrimeTable.storeUInt(70, 919757);
	return smallPrimeTable;
/*
udanax-top.st:33195:LPPrimeSizeProvider class methodsFor: 'initialization'!
{UInt32Array} primeTable
	| smallPrimeTable {UInt32Array} |
	smallPrimeTable _ UInt32Array make: 71.
	smallPrimeTable at: UInt32Zero storeUInt: 7.
	smallPrimeTable at: 1 storeUInt: 19.
	smallPrimeTable at: 2 storeUInt: 41.
	smallPrimeTable at: 3 storeUInt: 67.
	smallPrimeTable at: 4 storeUInt: 101.
	smallPrimeTable at: 5 storeUInt: 139.
	smallPrimeTable at: 6 storeUInt: 191.
	smallPrimeTable at: 7 storeUInt: 241.
	smallPrimeTable at: 8 storeUInt: 313.
	smallPrimeTable at: 9 storeUInt: 401.
	smallPrimeTable at: 10 storeUInt: 499.
	smallPrimeTable at: 11 storeUInt: 617.
	smallPrimeTable at: 12 storeUInt: 751.
	smallPrimeTable at: 13 storeUInt: 911.
	smallPrimeTable at: 14 storeUInt: 1091.
	smallPrimeTable at: 15 storeUInt: 1297.
	smallPrimeTable at: 16 storeUInt: 1543.
	smallPrimeTable at: 17 storeUInt: 1801.
	smallPrimeTable at: 18 storeUInt: 2113.
	smallPrimeTable at: 19 storeUInt: 2459.
	smallPrimeTable at: 20 storeUInt: 2851.
	smallPrimeTable at: 21 storeUInt: 3331.
	smallPrimeTable at: 22 storeUInt: 3833.
	smallPrimeTable at: 23 storeUInt: 4421.
	smallPrimeTable at: 24 storeUInt: 5059.
	smallPrimeTable at: 25 storeUInt: 5801.
	smallPrimeTable at: 26 storeUInt: 6607.
	smallPrimeTable at: 27 storeUInt: 7547.
	smallPrimeTable at: 28 storeUInt: 8599.
	smallPrimeTable at: 29 storeUInt: 9697.
	smallPrimeTable at: 30 storeUInt: 11004.
	smallPrimeTable at: 31 storeUInt: 12479.
	smallPrimeTable at: 32 storeUInt: 14057.
	smallPrimeTable at: 33 storeUInt: 15803.
	smallPrimeTable at: 34 storeUInt: 17881.
	smallPrimeTable at: 35 storeUInt: 20117.
	smallPrimeTable at: 36 storeUInt: 22573.
	smallPrimeTable at: 37 storeUInt: 28499.
	smallPrimeTable at: 38 storeUInt: 32003.
	smallPrimeTable at: 39 storeUInt: 35759.
	smallPrimeTable at: 40 storeUInt: 40009.
	smallPrimeTable at: 41 storeUInt: 44729.
	smallPrimeTable at: 42 storeUInt: 50053.
	smallPrimeTable at: 43 storeUInt: 55933.
	smallPrimeTable at: 44 storeUInt: 62483.
	smallPrimeTable at: 45 storeUInt: 69911.
	smallPrimeTable at: 46 storeUInt: 77839.
	smallPrimeTable at: 47 storeUInt: 86929.
	smallPrimeTable at: 48 storeUInt: 96787.
	smallPrimeTable at: 49 storeUInt: 108041.
	smallPrimeTable at: 50 storeUInt: 120473.
	smallPrimeTable at: 51 storeUInt: 134087.
	smallPrimeTable at: 52 storeUInt: 149287.
	smallPrimeTable at: 53 storeUInt: 166303.
	smallPrimeTable at: 54 storeUInt: 185063.
	smallPrimeTable at: 55 storeUInt: 205957.
	smallPrimeTable at: 56 storeUInt: 228887.
	smallPrimeTable at: 57 storeUInt: 254663.
	smallPrimeTable at: 58 storeUInt: 282833.
	smallPrimeTable at: 59 storeUInt: 313979.
	smallPrimeTable at: 60 storeUInt: 347287.
	smallPrimeTable at: 61 storeUInt: 384317.
	smallPrimeTable at: 62 storeUInt: 424667.
	smallPrimeTable at: 63 storeUInt: 468841.
	smallPrimeTable at: 64 storeUInt: 517073.
	smallPrimeTable at: 65 storeUInt: 569927.
	smallPrimeTable at: 66 storeUInt: 627553.
	smallPrimeTable at: 67 storeUInt: 691183.
	smallPrimeTable at: 68 storeUInt: 760657.
	smallPrimeTable at: 69 storeUInt: 836483.
	smallPrimeTable at: 70 storeUInt: 919757.
	^ smallPrimeTable!
*/
}
public static void initTimeNonInherited() {
	MySoleProvider = new LPPrimeSizeProvider(primeTable());
/*
udanax-top.st:33274:LPPrimeSizeProvider class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	self REQUIRES: UInt32Array.
	MySoleProvider _ LPPrimeSizeProvider create: self primeTable.!
*/
}
public static void linkTimeNonInherited() {
	MySoleProvider = null;
/*
udanax-top.st:33279:LPPrimeSizeProvider class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	MySoleProvider _ NULL.!
*/
}
public LPPrimeSizeProvider() {
/*

Generated during transformation
*/
}
public LPPrimeSizeProvider(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
