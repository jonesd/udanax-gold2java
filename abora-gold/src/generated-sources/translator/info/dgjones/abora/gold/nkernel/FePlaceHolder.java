/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BePlaceHolder;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.detect.FeFillDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeActualPlaceHolder;
import info.dgjones.abora.gold.nkernel.FePlaceHolder;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeVirtualPlaceHolder;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Represents a piece of pure identity in the Server.
 */
public class FePlaceHolder extends FeRangeElement {

/*
udanax-top.st:21790:
FeRangeElement subclass: #FePlaceHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:21794:
FePlaceHolder comment:
'Represents a piece of pure identity in the Server.'!
*/
/*
udanax-top.st:21796:
(FePlaceHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:21832:
FePlaceHolder class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:21835:
(FePlaceHolder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FePlaceHolder.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public void addFillDetector(FeFillDetector detector) {
	Heaper cast1 = getOrMakeBe();
	if (cast1 instanceof BePlaceHolder) {
		BePlaceHolder p = (BePlaceHolder) cast1;
		p.addDetector(detector);
	}
	else {
		/* in case it changed behind our backs */
		detector.filled(again());
	}
/*
udanax-top.st:21801:FePlaceHolder methodsFor: 'accessing'!
{void} addFillDetector: detector {FeFillDetector}
	self getOrMakeBe cast: BePlaceHolder into: [ :p |
		p addDetector: detector]
	others:
		["in case it changed behind our backs"
		detector filled: self again]!
*/
}
public FeRangeElement again() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:21809:FePlaceHolder methodsFor: 'accessing'!
{FeRangeElement} again
	self subclassResponsibility!
*/
}
public boolean canMakeIdentical(FeRangeElement newIdentity) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:21813:FePlaceHolder methodsFor: 'accessing'!
{BooleanVar} canMakeIdentical: newIdentity {FeRangeElement}
	self subclassResponsibility!
*/
}
public void makeIdentical(FeRangeElement newIdentity) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:21817:FePlaceHolder methodsFor: 'accessing'!
{void} makeIdentical: newIdentity {FeRangeElement}
	self subclassResponsibility!
*/
}
public BeRangeElement fetchBe() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:21823:FePlaceHolder methodsFor: 'server accessing'!
{BeRangeElement | NULL} fetchBe
	self subclassResponsibility!
*/
}
public BeRangeElement getOrMakeBe() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:21827:FePlaceHolder methodsFor: 'server accessing'!
{BeRangeElement} getOrMakeBe
	self subclassResponsibility!
*/
}
public static FePlaceHolder fake(BeEdition edition, Position key) {
	return new FeVirtualPlaceHolder(edition, key);
/*
udanax-top.st:21840:FePlaceHolder class methodsFor: 'creation'!
{FePlaceHolder} fake: edition {BeEdition} with: key {Position}
	^FeVirtualPlaceHolder create: edition with: key!
*/
}
public static FePlaceHolder on(BeRangeElement be) {
	FeRangeElement result;
	result = new FeActualPlaceHolder(be);
	be.addFeRangeElement(result);
	return (FePlaceHolder) result;
/*
udanax-top.st:21844:FePlaceHolder class methodsFor: 'creation'!
{FePlaceHolder} on: be {BeRangeElement}
	| result {FeRangeElement} |
	result := FeActualPlaceHolder create: be.
	be addFeRangeElement: result.
	^result cast: FePlaceHolder!
*/
}
/**
 * @deprecated
 */
public static FePlaceHolder grand(ID iD) {
	throw new PasseException();
/*
udanax-top.st:21853:FePlaceHolder class methodsFor: 'smalltalk: passe'!
{FePlaceHolder} grand: iD {ID}
	self passe.!
*/
}
public FePlaceHolder() {
/*

Generated during transformation
*/
}
public FePlaceHolder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
