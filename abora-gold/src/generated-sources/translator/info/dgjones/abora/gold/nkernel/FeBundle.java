/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeBundle;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Describes a single chunk of information from an Edition
 */
public class FeBundle extends Heaper {

	protected XnRegion myRegion;
/*
udanax-top.st:19260:
Heaper subclass: #FeBundle
	instanceVariableNames: 'myRegion {XnRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:19264:
FeBundle comment:
'Describes a single chunk of information from an Edition'!
*/
/*
udanax-top.st:19266:
(FeBundle getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:19289:
FeBundle class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19292:
(FeBundle getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeBundle.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public FeBundle(XnRegion region) {
	super();
	myRegion = region;
/*
udanax-top.st:19271:FeBundle methodsFor: 'protected: create'!
create: region {XnRegion}
	super create.
	myRegion := region.!
*/
}
/**
 * Essential. The positions in the Edition for which I describe the contents
 */
public XnRegion region() {
	return myRegion;
/*
udanax-top.st:19278:FeBundle methodsFor: 'accessing'!
{XnRegion CLIENT} region
	"Essential. The positions in the Edition for which I describe the contents"
	
	^myRegion!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:19285:FeBundle methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
/**
 * {XnRegion CLIENT} region
 */
public static void infostProtocol() {
/*
udanax-top.st:19297:FeBundle class methodsFor: 'smalltalk: system'!
info.stProtocol
"{XnRegion CLIENT} region
"!
*/
}
public FeBundle() {
/*

Generated during transformation
*/
}
public FeBundle(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
