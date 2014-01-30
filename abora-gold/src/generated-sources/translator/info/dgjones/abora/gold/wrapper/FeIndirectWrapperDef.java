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
import info.dgjones.abora.gold.java.missing.FeIndirectWrapperChecker;
import info.dgjones.abora.gold.java.missing.FeIndirectWrapperMaker;
import info.dgjones.abora.gold.java.missing.FeWrapperSpecHolder;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.wrapper.FeIndirectWrapperDef;
import info.dgjones.abora.gold.wrapper.FeIndirectWrapperSpec;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperDef;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class FeIndirectWrapperDef extends FeWrapperDef {

	protected Sequence myInner;
	protected FeIndirectWrapperMaker myMaker;
	protected FeIndirectWrapperChecker myChecker;
/*
udanax-top.st:25831:
FeWrapperDef subclass: #FeIndirectWrapperDef
	instanceVariableNames: '
		myInner {Sequence}
		myMaker {FeIndirectWrapperMaker var}
		myChecker {FeIndirectWrapperChecker var}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-wrapper'!
*/
/*
udanax-top.st:25838:
(FeIndirectWrapperDef getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeIndirectWrapperDef.class).setAttributes( new Set().add("CONCRETE").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public boolean check(FeEdition inner) {
	return myChecker.invokeFunction(inner);
/*
udanax-top.st:25843:FeIndirectWrapperDef methodsFor: 'accessing'!
{BooleanVar} check: inner {FeEdition}
	^myChecker invokeFunction: inner!
*/
}
public Sequence innerDefName() {
	return myInner;
/*
udanax-top.st:25846:FeIndirectWrapperDef methodsFor: 'accessing'!
{Sequence} innerDefName
	^myInner!
*/
}
public FeWrapperSpec makeSpec() {
	return FeIndirectWrapperSpec.make(this);
/*
udanax-top.st:25850:FeIndirectWrapperDef methodsFor: 'accessing'!
{FeWrapperSpec} makeSpec
	
	^FeIndirectWrapperSpec make: self!
*/
}
public FeWrapper makeWrapper(FeEdition edition, FeWrapper inner) {
	return myMaker.invokeFunction(edition, inner);
/*
udanax-top.st:25854:FeIndirectWrapperDef methodsFor: 'accessing'!
{FeWrapper} makeWrapper: edition {FeEdition} with: inner {FeWrapper}
	^myMaker invokeFunction: edition with: inner!
*/
}
public FeIndirectWrapperDef(Sequence name, Sequence superName, FeWrapperSpecHolder holder, FeIndirectWrapperMaker maker, FeIndirectWrapperChecker checker) {
	super(name, superName, holder);
	myMaker = maker;
	myChecker = checker;
/*
udanax-top.st:25860:FeIndirectWrapperDef methodsFor: 'create'!
create: name {Sequence}
	with: superName {Sequence | NULL}
	with: holder {FeWrapperSpecHolder var | NULL}
	with: maker {FeIndirectWrapperMaker var}
	with: checker {FeIndirectWrapperChecker var}
	super create: name with: superName with: holder.
	myMaker := maker.
	myChecker := checker.!
*/
}
public FeIndirectWrapperDef(Sequence name, Sequence superName, FeWrapperSpecHolder holder, Sequence inner, FeIndirectWrapperMaker maker, FeIndirectWrapperChecker checker) {
	super(name, superName, holder);
	myInner = inner;
	myMaker = maker;
	myChecker = checker;
/*
udanax-top.st:25870:FeIndirectWrapperDef methodsFor: 'create'!
create: name {Sequence}
	with: superName {Sequence | NULL}
	with: holder {FeWrapperSpecHolder var | NULL}
	with: inner {Sequence | NULL}
	with: maker {FeIndirectWrapperMaker var}
	with: checker {FeIndirectWrapperChecker var}
	super create: name with: superName with: holder.
	myInner := inner.
	myMaker := maker.
	myChecker := checker.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:25884:FeIndirectWrapperDef methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:25886:FeIndirectWrapperDef methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public FeIndirectWrapperDef() {
/*

Generated during transformation
*/
}
public FeIndirectWrapperDef(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
