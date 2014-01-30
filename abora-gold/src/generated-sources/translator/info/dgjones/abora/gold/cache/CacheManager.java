/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cache;

import info.dgjones.abora.gold.cache.CacheManager;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class CacheManager extends Heaper {

/*
udanax-top.st:13065:
Heaper subclass: #CacheManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cache'!
*/
/*
udanax-top.st:13069:
(CacheManager getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CacheManager.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Return the value associated with the key, if any.
 */
public Heaper fetch(Heaper key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:13074:CacheManager methodsFor: 'accessing'!
{Heaper | NULL} fetch: key {Heaper}
	"Return the value associated with the key, if any."
	
	self subclassResponsibility!
*/
}
/**
 * Does te cach contain something at the given key?
 */
public boolean hasMember(Heaper key) {
	/* Should the key be a Heaper or a Position? */
	throw new SubclassResponsibilityException();
/*
udanax-top.st:13079:CacheManager methodsFor: 'accessing'!
{BooleanVar} hasMember: key {Heaper}
	"Does te cach contain something at the given key?"
	
	"Should the key be a Heaper or a Position?"
	
	self subclassResponsibility!
*/
}
/**
 * Remove the cached association with key.  Return true if the cache contained something at
 * that key.
 */
public boolean wipe(Heaper key) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:13086:CacheManager methodsFor: 'accessing'!
{BooleanVar} wipe: key {Heaper}
	"Remove the cached association with key.  Return true if the cache contained something at that key."
	
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:13093:CacheManager methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public CacheManager() {
/*

Generated during transformation
*/
}
public CacheManager(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
