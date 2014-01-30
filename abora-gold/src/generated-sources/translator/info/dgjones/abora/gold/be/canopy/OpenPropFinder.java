/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.canopy;

import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.be.canopy.OpenPropFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * The finder which matches everything.  Used to indicate that everything below here
 * necessarily matches.
 */
public class OpenPropFinder extends PropFinder {

/*
udanax-top.st:39796:
PropFinder subclass: #OpenPropFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:39800:
OpenPropFinder comment:
'The finder which matches everything.  Used to indicate that everything below here necessarily matches.'!
*/
/*
udanax-top.st:39802:
(OpenPropFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(OpenPropFinder.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public PropFinder findPast(BeEdition stamp) {
	return this;
/*
udanax-top.st:39807:OpenPropFinder methodsFor: 'accessing'!
{PropFinder} findPast: stamp {BeEdition unused}
	
	^self!
*/
}
public boolean isFull() {
	return true;
/*
udanax-top.st:39811:OpenPropFinder methodsFor: 'accessing'!
{BooleanVar} isFull
	
	^true!
*/
}
/**
 * tell whether a prop matches this filter
 */
public boolean match(Prop prop) {
	return true;
/*
udanax-top.st:39815:OpenPropFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop unused}
	"tell whether a prop matches this filter"
	^true!
*/
}
public PropFinder pass(CanopyCrum crum) {
	return this;
/*
udanax-top.st:39819:OpenPropFinder methodsFor: 'accessing'!
{PropFinder} pass: crum {CanopyCrum unused}
	^self!
*/
}
public int actualHashForEqual() {
	return getCategory().hashForEqual();
/*
udanax-top.st:39825:OpenPropFinder methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^self getCategory hashForEqual!
*/
}
public boolean isEqual(Heaper other) {
	return other instanceof OpenPropFinder;
/*
udanax-top.st:39829:OpenPropFinder methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	^other isKindOf: OpenPropFinder!
*/
}
public OpenPropFinder() {
	super( ~ 0);
/*
udanax-top.st:39835:OpenPropFinder methodsFor: 'create'!
create
	super create: UInt32Zero bitInvert!
*/
}
public OpenPropFinder(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:39841:OpenPropFinder methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:39844:OpenPropFinder methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
}
