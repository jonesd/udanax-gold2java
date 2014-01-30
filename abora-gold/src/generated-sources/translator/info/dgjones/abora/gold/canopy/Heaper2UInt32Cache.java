/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.canopy;

import info.dgjones.abora.gold.canopy.Heaper2UInt32Cache;
import info.dgjones.abora.gold.collection.basic.Int32Array;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tabtool.PrimeSizeProvider;
import info.dgjones.abora.gold.x.PrimIntValue;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Caches a mapping from Heapers (using isEqual / hashForEqual) to UInt32s. Returns
 * myEmptyValue if there is no cached mapping.
 */
public class Heaper2UInt32Cache extends Heaper {

	protected PtrArray myKeys;
	protected Int32Array myValues;
	protected int myEmptyValue;
/*
udanax-top.st:27019:
Heaper subclass: #Heaper2UInt32Cache
	instanceVariableNames: '
		myKeys {PtrArray}
		myValues {UInt32Array}
		myEmptyValue {UInt32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-canopy'!
*/
/*
udanax-top.st:27026:
Heaper2UInt32Cache comment:
'Caches a mapping from Heapers (using isEqual / hashForEqual) to UInt32s. Returns myEmptyValue if there is no cached mapping.'!
*/
/*
udanax-top.st:27028:
(Heaper2UInt32Cache getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
/*
udanax-top.st:27079:
Heaper2UInt32Cache class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:27082:
(Heaper2UInt32Cache getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Heaper2UInt32Cache.class).setAttributes( new Set().add("CONCRETE").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Cache a value for a key
 */
public void cache(Heaper key, int value) {
	int index;
	index = AboraSupport.modulo(key.hashForEqual(), myKeys.count());
	myKeys.store(index, key);
	myValues.storeUInt(index, value);
/*
udanax-top.st:27033:Heaper2UInt32Cache methodsFor: 'accessing'!
{void} at: key {Heaper} cache: value {UInt32}
	"Cache a value for a key"
	
	| index {Int32} |
	index := key hashForEqual \\ myKeys count.
	myKeys at: index store: key.
	myValues at: index storeUInt: value.!
*/
}
/**
 * Return the cached value for the key, or my empty value if there is none
 */
public int fetch(Heaper key) {
	int index;
	Heaper k;
	index = AboraSupport.modulo(key.hashForEqual(), myKeys.count());
	k = myKeys.fetch(index);
	if (k != null && (k == key || (k.isEqual(key)))) {
		return myValues.uIntAt(index);
	}
	else {
		return myEmptyValue;
	}
/*
udanax-top.st:27041:Heaper2UInt32Cache methodsFor: 'accessing'!
{UInt32} fetch: key {Heaper}
	"Return the cached value for the key, or my empty value if there is none"
	
	| index {Int32} k {Heaper} |
	index := key hashForEqual \\ myKeys count.
	k := myKeys fetch: index.
	(k ~~ NULL and: [k == key or: [k isEqual: key]])
		ifTrue: [^myValues uIntAt: index]
		ifFalse: [^myEmptyValue]!
*/
}
/**
 * Return the cached value for the key, or BLAST if there is none
 */
public int get(Heaper key) {
	int index;
	Heaper k;
	index = AboraSupport.modulo(key.hashForEqual(), myKeys.count());
	k = myKeys.fetch(index);
	if ( ! (k != null && (k == key || (k.isEqual(key))))) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return myValues.uIntAt(index);
/*
udanax-top.st:27051:Heaper2UInt32Cache methodsFor: 'accessing'!
{UInt32} get: key {Heaper}
	"Return the cached value for the key, or BLAST if there is none"
	
	| index {Int32} k {Heaper} |
	index := key hashForEqual \\ myKeys count.
	k := myKeys fetch: index.
	(k ~~ NULL and: [k == key or: [k isEqual: key]])
		ifFalse: [Heaper BLAST: #NotInTable].
	^myValues uIntAt: index!
*/
}
public Heaper2UInt32Cache(int count, int empty) {
	super();
	myKeys = PtrArray.nulls(count);
	myValues = Int32Array.make(count);
	myEmptyValue = empty;
	if (empty != 0) {
		myValues.storeAll((PrimIntValue.make(empty)));
	}
/*
udanax-top.st:27063:Heaper2UInt32Cache methodsFor: 'create'!
create: count {Int32} with: empty {UInt32}
	super create.
	myKeys := PtrArray nulls: count.
	myValues := UInt32Array make: count.
	myEmptyValue := empty.
	empty ~~ UInt32Zero ifTrue:
		[myValues storeAll: (PrimIntValue make: empty)]!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:27074:Heaper2UInt32Cache methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:27076:Heaper2UInt32Cache methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static Heaper2UInt32Cache make(int n) {
	return make(n, 0);
/*
udanax-top.st:27087:Heaper2UInt32Cache class methodsFor: 'smalltalk: defaults'!
make: n
	^self make: n with: 0!
*/
}
public static Heaper2UInt32Cache make(int count, int empty) {
	return new Heaper2UInt32Cache((PrimeSizeProvider.make().uInt32PrimeAfter(count)), empty);
/*
udanax-top.st:27092:Heaper2UInt32Cache class methodsFor: 'create'!
make: count {Int32} with: empty {UInt32 default: UInt32Zero}
	^self create: (PrimeSizeProvider make uInt32PrimeAfter: count) with: empty!
*/
}
public static void initTimeNonInherited() {
/*
udanax-top.st:27098:Heaper2UInt32Cache class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	self REQUIRES: PrimArray.
	self REQUIRES: PrimeSizeProvider.!
*/
}
public Heaper2UInt32Cache() {
/*

Generated during transformation
*/
}
public Heaper2UInt32Cache(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
