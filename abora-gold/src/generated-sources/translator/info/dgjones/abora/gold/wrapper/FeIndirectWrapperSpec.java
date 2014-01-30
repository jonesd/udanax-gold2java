/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.wrapper;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.wrapper.FeConcreteWrapperSpec;
import info.dgjones.abora.gold.wrapper.FeIndirectWrapperDef;
import info.dgjones.abora.gold.wrapper.FeIndirectWrapperSpec;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;

public class FeIndirectWrapperSpec extends FeConcreteWrapperSpec {

	protected FeConcreteWrapperSpec myInner;
/*
udanax-top.st:26385:
FeConcreteWrapperSpec subclass: #FeIndirectWrapperSpec
	instanceVariableNames: 'myInner {FeConcreteWrapperSpec}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-wrapper'!
*/
/*
udanax-top.st:26389:
(FeIndirectWrapperSpec getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:26435:
FeIndirectWrapperSpec class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:26438:
(FeIndirectWrapperSpec getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeIndirectWrapperSpec.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean wraps(FeConcreteWrapperSpec other) {
	return this == other || (myInner.wraps(other));
/*
udanax-top.st:26394:FeIndirectWrapperSpec methodsFor: 'accessing'!
{BooleanVar} wraps: other {FeConcreteWrapperSpec}
	^self == other or: [myInner wraps: other]!
*/
}
/**
 * Try to certify as this type. If successful, return TRUE and endorse it; if not, return
 * FALSE.
 */
public boolean certify(FeEdition inner) {
	if (indirectDef().check(inner)) {
		endorse(inner);
		return true;
	}
	else {
		return false;
	}
/*
udanax-top.st:26400:FeIndirectWrapperSpec methodsFor: 'private:'!
{BooleanVar} certify: inner {FeEdition}
	"Try to certify as this type. If successful, return TRUE and endorse it; if not, return FALSE."
	(self indirectDef check: inner) ifTrue:
		[self endorse: inner.
		^true]
	ifFalse:
		[^false]!
*/
}
public FeIndirectWrapperDef indirectDef() {
	return (FeIndirectWrapperDef) def();
/*
udanax-top.st:26408:FeIndirectWrapperSpec methodsFor: 'private:'!
{FeIndirectWrapperDef} indirectDef
	^self def cast: FeIndirectWrapperDef!
*/
}
public void setup() {
	super.setup();
	myInner = (FeConcreteWrapperSpec) (FeWrapperSpec.get(indirectDef().innerDefName()));
/*
udanax-top.st:26413:FeIndirectWrapperSpec methodsFor: 'protected:'!
{void} setup
	super setup.
	myInner := (FeWrapperSpec get: self indirectDef innerDefName) cast: FeConcreteWrapperSpec!
*/
}
public FeIndirectWrapperSpec(FeIndirectWrapperDef def) {
	super(def);
	myInner = null;
/*
udanax-top.st:26420:FeIndirectWrapperSpec methodsFor: 'create'!
create: def {FeIndirectWrapperDef}
	super create: def.
	myInner := NULL.!
*/
}
public FeWrapper fetchWrap(FeEdition edition) {
	FeWrapper inner;
	inner = myInner.wrap(edition);
	if ((isCertified(edition)) || (certify(edition))) {
		return indirectDef().makeWrapper(edition, inner);
	}
	return null;
/*
udanax-top.st:26426:FeIndirectWrapperSpec methodsFor: 'vulnerable'!
{FeWrapper | NULL} fetchWrap: edition {FeEdition}
	| inner {FeWrapper} |
	inner := myInner wrap: edition.
	((self isCertified: edition) or: [self certify: edition]) ifTrue:
		[^self indirectDef makeWrapper: edition with: inner].
	^NULL!
*/
}
public static FeIndirectWrapperSpec make(FeIndirectWrapperDef def) {
	return new FeIndirectWrapperSpec(def);
/*
udanax-top.st:26443:FeIndirectWrapperSpec class methodsFor: 'pseudo constructors'!
make: def {FeIndirectWrapperDef}
	^self create: def!
*/
}
public FeIndirectWrapperSpec() {
/*

Generated during transformation
*/
}
public FeIndirectWrapperSpec(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
