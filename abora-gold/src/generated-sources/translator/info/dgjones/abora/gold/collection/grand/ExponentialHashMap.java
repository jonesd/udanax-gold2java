/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.grand;

import info.dgjones.abora.gold.collection.basic.Int32Array;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.grand.ExponentialHashMap;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.FHash;
import info.dgjones.abora.gold.java.missing.RandomStepper;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class ExponentialHashMap extends Heaper {

	protected int domain;
	protected Int32Array rBottoms;
	protected Int32Array rSizes;
	protected Int32Array dBottoms;
	protected int dSize;
	protected static PtrArray FastHashMap;
	protected static int HashBits;
	protected static ExponentialHashMap TheExponentialMap;
/*
udanax-top.st:18950:
Heaper subclass: #ExponentialHashMap
	instanceVariableNames: '
		domain {Int32}
		rBottoms {UInt32Array}
		rSizes {UInt32Array}
		dBottoms {UInt32Array}
		dSize {Int32}'
	classVariableNames: '
		FastHashMap {PtrArray} 
		HashBits {UInt32} 
		TheExponentialMap {ExponentialHashMap} '
	poolDictionaries: ''
	category: 'Xanadu-Collection-Grand'!
*/
/*
udanax-top.st:18962:
(ExponentialHashMap getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:19004:
ExponentialHashMap class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19007:
(ExponentialHashMap getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ExponentialHashMap.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public int of(int aHash) {
	int pieceIndex;
	if (aHash > domain) {
		throw new AboraRuntimeException(AboraRuntimeException.OUT_OF_DOMAIN);
	}
	pieceIndex = aHash / dSize;
	return (rBottoms.uIntAt(pieceIndex)) + ((aHash - (dBottoms.uIntAt(pieceIndex))) * (rSizes.uIntAt(pieceIndex)) / dSize);
/*
udanax-top.st:18967:ExponentialHashMap methodsFor: 'mapping'!
{UInt32} of: aHash {UInt32}
	| pieceIndex {Int32} |
	(aHash > domain)
		ifTrue: [ Heaper BLAST: #outOfDomain ].
	pieceIndex _ aHash // dSize.
	^ (rBottoms uIntAt: pieceIndex) + ((aHash - (dBottoms uIntAt: pieceIndex))
		* (rSizes uIntAt: pieceIndex) // dSize)!
*/
}
public ExponentialHashMap(int numPieces, int range) {
	super();
	int rBottom;
	domain = range;
	dSize = range / numPieces;
	/* Depends on image having UInt32 _ Integer. */
	rBottoms = Int32Array.make(numPieces);
	rSizes = Int32Array.make(numPieces);
	dBottoms = Int32Array.make(numPieces);
	rBottom = 0;
	for (int d = 0; d < numPieces; d ++ ) {
		dBottoms.storeUInt(d, d * dSize);
		rBottoms.storeUInt(d, rBottom);
		rBottom = expFunc((d + 1) * dSize, range);
		rSizes.storeUInt(d, rBottom - (rBottoms.uIntAt(d)));
	}
/*
udanax-top.st:18977:ExponentialHashMap methodsFor: 'creation'!
create: numPieces {Int32} with: range {UInt32} 
	| rBottom {UInt32} |
	super create.
	domain _ range.
	dSize _ range // numPieces.
	"Depends on image having UInt32 _ Integer."
	rBottoms _ UInt32Array make: numPieces.
	rSizes _ UInt32Array make: numPieces.
	dBottoms _ UInt32Array make: numPieces.
	rBottom _ UInt32Zero.
	UInt32Zero almostTo: numPieces do: [ :d {UInt32} |
		dBottoms at: d storeUInt: d * dSize.
		rBottoms at: d storeUInt: rBottom.
		rBottom _ self expFunc: d + 1 * dSize within: range.
		rSizes at: d storeUInt: rBottom - (rBottoms uIntAt: d)].!
*/
}
public int expFunc(int domElem, int range) {
	return (int) (range * ((AboraSupport.pow(2.0, (float) domElem / (float) range)) - 1));
/*
udanax-top.st:18995:ExponentialHashMap methodsFor: 'private: calculation'!
{UInt32} expFunc: domElem {UInt32} within: range {UInt32} 
	^(range * ((2.0 raisedTo: domElem asFloat / range asFloat) - 1)) asInteger!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:19000:ExponentialHashMap methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public static int exponentialMap(int aHash) {
	return (TheExponentialMap.of(((FHash.fastHashUInt32(aHash)) & HashBits))) & HashBits;
/*
udanax-top.st:19012:ExponentialHashMap class methodsFor: 'accessing'!
{UInt32 INLINE} exponentialMap: aHash {UInt32}
	^ (TheExponentialMap of:  ((FHash fastHash.UInt32: aHash) bitAnd: HashBits)) bitAnd: HashBits!
*/
}
public static int hashBits() {
	return HashBits;
/*
udanax-top.st:19015:ExponentialHashMap class methodsFor: 'accessing'!
{UInt32 INLINE} hashBits
	^ HashBits!
*/
}
/**
 * ExponentialHashMap initTimeNonInherited
 */
public static void initTimeNonInherited() {
	TheExponentialMap = new ExponentialHashMap(256, HashBits + 1);
	RandomStepper rand;
	rand = RandomStepper.make(43, 11, 5);
	FastHashMap = PtrArray.nulls(8);
	for (int i = 0; i <= 7; i ++ ) {
		Int32Array array;
		array = Int32Array.make(256);
		for (int  j = 0;  j <= 255;  j ++ ) {
			array.storeUInt(j, rand.value());
			rand.step();
		}
		FastHashMap.store(i, array);
	}
/*
udanax-top.st:19020:ExponentialHashMap class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	"ExponentialHashMap initTimeNonInherited"
	
	TheExponentialMap _  ExponentialHashMap create: 256 with: HashBits + 1.
	[| rand {RandomStepper} |
	rand _ RandomStepper make: 43 with: 11 with: 5.
	FastHashMap _ PtrArray nulls: 8.
	UInt32Zero to: 7 do:
		[:i {UInt32} |
		| array {UInt32Array} |
		array _ UInt32Array make: 256.
		UInt32Zero to: 255 do: 
			[: j {UInt32} |
			array at: j storeUInt: rand value.
			rand step].
		FastHashMap at: i store: array]] smalltalkOnly!
*/
}
/**
 * ExponentialHashMap linkTimeNonInherited
 */
public static void linkTimeNonInherited() {
	HashBits = (1 << 30) - 1;
	TheExponentialMap = null;
	/* Removed smalltalkOnly */
	/* Removed smalltalkOnly */
/*
udanax-top.st:19037:ExponentialHashMap class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	"ExponentialHashMap linkTimeNonInherited"
	
	HashBits _ (1 bitShift: 30) - 1.
	TheExponentialMap _ NULL.
	[HashBits _ SmallInteger maxVal // 2 - 1] smalltalkOnly.
	[FastHashMap _ NULL] smalltalkOnly!
*/
}
public ExponentialHashMap() {
/*

Generated during transformation
*/
}
public ExponentialHashMap(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
