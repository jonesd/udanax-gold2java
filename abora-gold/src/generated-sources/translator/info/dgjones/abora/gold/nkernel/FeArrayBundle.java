/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeArrayBundle;
import info.dgjones.abora.gold.nkernel.FeBundle;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Describes a chunk of information represented as an array. The number of elements in the
 * array are the same as my region, and they are ordered according to OrderSpec given to the
 * retrieve operation which produced me.
 */
public class FeArrayBundle extends FeBundle {

	protected PrimArray myArray;
	protected OrderSpec myOrder;
/*
udanax-top.st:19301:
FeBundle subclass: #FeArrayBundle
	instanceVariableNames: '
		myArray {PrimArray}
		myOrder {OrderSpec}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:19307:
FeArrayBundle comment:
'Describes a chunk of information represented as an array. The number of elements in the array are the same as my region, and they are ordered according to OrderSpec given to the retrieve operation which produced me.'!
*/
/*
udanax-top.st:19309:
(FeArrayBundle getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:19333:
FeArrayBundle class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19336:
(FeArrayBundle getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeArrayBundle.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential. The array of elements in this bundle
 */
public PrimArray array() {
	return myArray.copy();
/*
udanax-top.st:19314:FeArrayBundle methodsFor: 'accessing'!
{PrimArray CLIENT} array
	"Essential. The array of elements in this bundle"
	
	^myArray copy!
*/
}
/**
 * Essential. The order relating the elements in the array to the positions in the region.
 */
public OrderSpec ordering() {
	return myOrder;
/*
udanax-top.st:19319:FeArrayBundle methodsFor: 'accessing'!
{OrderSpec CLIENT} ordering
	"Essential. The order relating the elements in the array to the positions in the region."
	
	^myOrder!
*/
}
public FeArrayBundle(XnRegion region, PrimArray array, OrderSpec order) {
	super(region);
	myArray = array;
	myOrder = order;
/*
udanax-top.st:19326:FeArrayBundle methodsFor: 'private: create'!
create: region {XnRegion} with: array {PrimArray} with: order {OrderSpec}
	super create: region.
	myArray := array.
	myOrder _ order!
*/
}
public static FeArrayBundle make(XnRegion region, PrimArray array, OrderSpec order) {
	return new FeArrayBundle(region, array, order);
/*
udanax-top.st:19341:FeArrayBundle class methodsFor: 'create'!
make: region {XnRegion} with: array {PrimArray} with: order {OrderSpec}
	^self create: region with: array with: order!
*/
}
/**
 * {PrimArray CLIENT} array
 * {OrderSpec CLIENT} order
 */
public static void infostProtocol() {
/*
udanax-top.st:19347:FeArrayBundle class methodsFor: 'smalltalk: system'!
info.stProtocol
"{PrimArray CLIENT} array
{OrderSpec CLIENT} order
"!
*/
}
public FeArrayBundle() {
/*

Generated during transformation
*/
}
public FeArrayBundle(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
