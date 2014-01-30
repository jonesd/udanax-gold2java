/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cache;

import info.dgjones.abora.gold.cache.InstanceCache;
import info.dgjones.abora.gold.cache.SuspendedHeaper;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * InstanceCache is intended to store a small number of frequently used objects with the
 * intent of reducing memory allocation traffic.
 */
public class InstanceCache extends Heaper {

	protected PtrArray myArray;
	protected int myTop;
/*
udanax-top.st:27691:
Heaper subclass: #InstanceCache
	instanceVariableNames: '
		myArray {PtrArray}
		myTop {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cache'!
*/
/*
udanax-top.st:27697:
InstanceCache comment:
'InstanceCache is intended to store a small number of frequently used objects with the intent of reducing memory allocation traffic.'!
*/
/*
udanax-top.st:27699:
(InstanceCache getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
/*
udanax-top.st:27740:
InstanceCache class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:27743:
(InstanceCache getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(InstanceCache.class).setAttributes( new Set().add("CONCRETE").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	if (myTop >= 0) {
		Heaper result;
		result = myArray.fetch(myTop);
		myArray.store(myTop, null);
		myTop = myTop - 1;
		return result;
	}
	else {
		return null;
	}
/*
udanax-top.st:27704:InstanceCache methodsFor: 'accessing'!
{Heaper} fetch
	myTop >= Int32Zero
		ifTrue: [
			| result {Heaper} |
			result := myArray fetch: myTop.
			myArray at: myTop store: NULL.
			myTop := myTop - 1.
			^ result]
		ifFalse: [
			^ NULL]!
*/
}
public boolean store(Heaper object) {
	if (myTop < (myArray.count() - 1)) {
		myTop = myTop + 1;
		object.destruct();
		/* TODO newBecome */
		new SuspendedHeaper();
		myArray.store(myTop, object);
		return true;
	}
	else {
		return false;
	}
/*
udanax-top.st:27715:InstanceCache methodsFor: 'accessing'!
{BooleanVar} store: object {Heaper}
	myTop < (myArray count - 1)
		ifTrue: [
			myTop := myTop + 1.
			object destruct.
			(SuspendedHeaper new.Become: object) create.
			myArray at: myTop store: object.
			^ true]
		ifFalse: [
			^ false]!
*/
}
public InstanceCache(int size) {
	super();
	myArray = PtrArray.nulls(size);
	myTop = -1;
/*
udanax-top.st:27728:InstanceCache methodsFor: 'protected: create'!
create: size {Int32}
	super create.
	myArray := PtrArray nulls: size.
	myTop := -1!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:27735:InstanceCache methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:27737:InstanceCache methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static InstanceCache make(int size) {
	return new InstanceCache(size);
/*
udanax-top.st:27748:InstanceCache class methodsFor: 'create'!
make: size {Int32}
	^ self create: size!
*/
}
public InstanceCache() {
/*

Generated during transformation
*/
}
public InstanceCache(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
