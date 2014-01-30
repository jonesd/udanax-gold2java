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
import info.dgjones.abora.gold.nkernel.FeElementBundle;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Describes a region of an Edition in which all indices in my region hold the same
 * RangeElement.
 */
public class FeElementBundle extends FeBundle {

	protected FeRangeElement myElement;
/*
udanax-top.st:19352:
FeBundle subclass: #FeElementBundle
	instanceVariableNames: 'myElement {FeRangeElement}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:19356:
FeElementBundle comment:
'Describes a region of an Edition in which all indices in my region hold the same RangeElement.'!
*/
/*
udanax-top.st:19358:
(FeElementBundle getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:19376:
FeElementBundle class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19379:
(FeElementBundle getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeElementBundle.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential. The RangeElement which is at every position in my region
 */
public FeRangeElement element() {
	return myElement;
/*
udanax-top.st:19363:FeElementBundle methodsFor: 'accessing'!
{FeRangeElement CLIENT} element
	"Essential. The RangeElement which is at every position in my region"
	
	^myElement!
*/
}
public FeElementBundle(XnRegion region, FeRangeElement element) {
	super(region);
	myElement = element;
/*
udanax-top.st:19370:FeElementBundle methodsFor: 'private: create'!
create: region {XnRegion} with: element {FeRangeElement}
	super create: region.
	myElement := element!
*/
}
public static FeElementBundle make(XnRegion region, FeRangeElement element) {
	return new FeElementBundle(region, element);
/*
udanax-top.st:19384:FeElementBundle class methodsFor: 'create'!
make: region {XnRegion} with: element {FeRangeElement}
	^self create: region with: element!
*/
}
/**
 * {FeRangeElement CLIENT} element
 */
public static void infostProtocol() {
/*
udanax-top.st:19390:FeElementBundle class methodsFor: 'smalltalk: system'!
info.stProtocol
"{FeRangeElement CLIENT} element
"!
*/
}
public FeElementBundle() {
/*

Generated during transformation
*/
}
public FeElementBundle(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
