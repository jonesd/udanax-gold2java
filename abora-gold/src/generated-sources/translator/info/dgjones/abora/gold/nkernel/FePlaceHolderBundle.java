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
import info.dgjones.abora.gold.nkernel.FePlaceHolderBundle;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Describes a region of an Edition in which all indices in my region have a distinct
 * PlaceHolder.
 */
public class FePlaceHolderBundle extends FeBundle {

/*
udanax-top.st:19394:
FeBundle subclass: #FePlaceHolderBundle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:19398:
FePlaceHolderBundle comment:
'Describes a region of an Edition in which all indices in my region have a distinct PlaceHolder.'!
*/
/*
udanax-top.st:19400:
(FePlaceHolderBundle getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:19410:
FePlaceHolderBundle class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19413:
(FePlaceHolderBundle getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FePlaceHolderBundle.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public FePlaceHolderBundle(XnRegion region) {
	super(region);
/*
udanax-top.st:19405:FePlaceHolderBundle methodsFor: 'private: create'!
create: region {XnRegion}
	super create: region.!
*/
}
public static FePlaceHolderBundle make(XnRegion region) {
	return new FePlaceHolderBundle(region);
/*
udanax-top.st:19418:FePlaceHolderBundle class methodsFor: 'create'!
make: region {XnRegion}
	^self create: region!
*/
}
public FePlaceHolderBundle() {
/*

Generated during transformation
*/
}
public FePlaceHolderBundle(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
