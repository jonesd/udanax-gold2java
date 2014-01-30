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
import info.dgjones.abora.gold.wrapper.FeDirectWrapperDef;
import info.dgjones.abora.gold.wrapper.FeDirectWrapperSpec;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.xcvr.Rcvr;

public class FeDirectWrapperSpec extends FeConcreteWrapperSpec {

/*
udanax-top.st:26335:
FeConcreteWrapperSpec subclass: #FeDirectWrapperSpec
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-wrapper'!
*/
/*
udanax-top.st:26339:
(FeDirectWrapperSpec getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:26373:
FeDirectWrapperSpec class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:26376:
(FeDirectWrapperSpec getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeDirectWrapperSpec.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean wraps(FeConcreteWrapperSpec other) {
	return this == other;
/*
udanax-top.st:26344:FeDirectWrapperSpec methodsFor: 'accessing'!
{BooleanVar} wraps: other {FeConcreteWrapperSpec}
	^self == other!
*/
}
/**
 * Try to certify as this type. If successful, return TRUE and endorse it; if not, return
 * FALSE.
 */
public boolean certify(FeEdition edition) {
	if (((FeDirectWrapperDef) def()).check(edition)) {
		endorse(edition);
		return true;
	}
	else {
		return false;
	}
/*
udanax-top.st:26350:FeDirectWrapperSpec methodsFor: 'private:'!
{BooleanVar} certify: edition {FeEdition}
	"Try to certify as this type. If successful, return TRUE and endorse it; if not, return FALSE."
	((self def cast: FeDirectWrapperDef) check: edition) ifTrue:
		[self endorse: edition.
		^true]
	ifFalse:
		[^false]!
*/
}
public FeDirectWrapperSpec(FeDirectWrapperDef def) {
	super(def);
/*
udanax-top.st:26360:FeDirectWrapperSpec methodsFor: 'create'!
create: def {FeDirectWrapperDef}
	super create: def!
*/
}
public FeWrapper fetchWrap(FeEdition edition) {
	if ((isCertified(edition)) || (certify(edition))) {
		return ((FeDirectWrapperDef) def()).makeWrapper(edition);
	}
	else {
		return null;
	}
/*
udanax-top.st:26365:FeDirectWrapperSpec methodsFor: 'vulnerable'!
{FeWrapper} fetchWrap: edition {FeEdition}
	((self isCertified: edition) or: [self certify: edition]) ifTrue:
		[^(self def cast: FeDirectWrapperDef) makeWrapper: edition]
	ifFalse:
		[^NULL]!
*/
}
public static FeDirectWrapperSpec make(FeDirectWrapperDef def) {
	return new FeDirectWrapperSpec(def);
/*
udanax-top.st:26381:FeDirectWrapperSpec class methodsFor: 'pseudo constructors'!
make: def {FeDirectWrapperDef}
	^self create: def!
*/
}
public FeDirectWrapperSpec() {
/*

Generated during transformation
*/
}
public FeDirectWrapperSpec(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
