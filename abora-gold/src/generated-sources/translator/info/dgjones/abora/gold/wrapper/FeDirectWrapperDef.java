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
import info.dgjones.abora.gold.java.missing.FeDirectWrapperChecker;
import info.dgjones.abora.gold.java.missing.FeDirectWrapperMaker;
import info.dgjones.abora.gold.java.missing.FeWrapperSpecHolder;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.wrapper.FeDirectWrapperDef;
import info.dgjones.abora.gold.wrapper.FeDirectWrapperSpec;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperDef;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class FeDirectWrapperDef extends FeWrapperDef {

	protected FeDirectWrapperMaker myMaker;
	protected FeDirectWrapperChecker myChecker;
/*
udanax-top.st:25790:
FeWrapperDef subclass: #FeDirectWrapperDef
	instanceVariableNames: '
		myMaker {FeDirectWrapperMaker var}
		myChecker {FeDirectWrapperChecker var}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-wrapper'!
*/
/*
udanax-top.st:25796:
(FeDirectWrapperDef getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeDirectWrapperDef.class).setAttributes( new Set().add("CONCRETE").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public boolean check(FeEdition edition) {
	return myChecker.invokeFunction(edition);
/*
udanax-top.st:25801:FeDirectWrapperDef methodsFor: 'accessing'!
{BooleanVar} check: edition {FeEdition}
	^myChecker invokeFunction: edition!
*/
}
public FeWrapperSpec makeSpec() {
	return FeDirectWrapperSpec.make(this);
/*
udanax-top.st:25805:FeDirectWrapperDef methodsFor: 'accessing'!
{FeWrapperSpec} makeSpec
	
	^FeDirectWrapperSpec make: self!
*/
}
public FeWrapper makeWrapper(FeEdition edition) {
	return myMaker.invokeFunction(edition);
/*
udanax-top.st:25809:FeDirectWrapperDef methodsFor: 'accessing'!
{FeWrapper} makeWrapper: edition {FeEdition}
	^myMaker invokeFunction: edition!
*/
}
public FeDirectWrapperDef(Sequence name, Sequence superName, FeWrapperSpecHolder holder, FeDirectWrapperMaker maker, FeDirectWrapperChecker checker) {
	super(name, superName, holder);
	myMaker = maker;
	myChecker = checker;
/*
udanax-top.st:25815:FeDirectWrapperDef methodsFor: 'create'!
create: name {Sequence}
	with: superName {Sequence | NULL}
	with: holder {FeWrapperSpecHolder var | NULL}
	with: maker {FeDirectWrapperMaker var}
	with: checker {FeDirectWrapperChecker var}
	super create: name with: superName with: holder.
	myMaker := maker.
	myChecker := checker.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:25827:FeDirectWrapperDef methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:25829:FeDirectWrapperDef methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public FeDirectWrapperDef() {
/*

Generated during transformation
*/
}
public FeDirectWrapperDef(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
