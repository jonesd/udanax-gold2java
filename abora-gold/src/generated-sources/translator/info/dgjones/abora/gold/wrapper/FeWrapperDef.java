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
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.FeDirectWrapperChecker;
import info.dgjones.abora.gold.java.missing.FeDirectWrapperMaker;
import info.dgjones.abora.gold.java.missing.FeIndirectWrapperChecker;
import info.dgjones.abora.gold.java.missing.FeIndirectWrapperMaker;
import info.dgjones.abora.gold.java.missing.FeWrapperSpecHolder;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.wrapper.FeAbstractWrapperDef;
import info.dgjones.abora.gold.wrapper.FeDirectWrapperDef;
import info.dgjones.abora.gold.wrapper.FeIndirectWrapperDef;
import info.dgjones.abora.gold.wrapper.FeWrapperDef;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * ?I: names
 * ?P: strings
 * ?P: PackOBits
 */
public class FeWrapperDef extends Heaper {

	protected Sequence myName;
	protected Sequence mySuperDefName;
	protected FeWrapperSpecHolder mySpecHolder;
/*
udanax-top.st:25685:
Heaper subclass: #FeWrapperDef
	instanceVariableNames: '
		myName {Sequence}
		mySuperDefName {Sequence | NULL}
		mySpecHolder {FeWrapperSpecHolder var}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-wrapper'!
*/
/*
udanax-top.st:25692:
FeWrapperDef comment:
'?I: names
	?P: strings
	?P: PackOBits'!
*/
/*
udanax-top.st:25696:
(FeWrapperDef getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:25734:
FeWrapperDef class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:25737:
(FeWrapperDef getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeWrapperDef.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public Sequence fetchSuperDefName() {
	return mySuperDefName;
/*
udanax-top.st:25701:FeWrapperDef methodsFor: 'accessing'!
{Sequence | NULL} fetchSuperDefName
	^mySuperDefName!
*/
}
/**
 * Make a WrapperSpec for this definition and return it
 */
public FeWrapperSpec makeSpec() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:25704:FeWrapperDef methodsFor: 'accessing'!
{FeWrapperSpec} makeSpec
	"Make a WrapperSpec for this definition and return it"
	
	self subclassResponsibility!
*/
}
public Sequence name() {
	return myName;
/*
udanax-top.st:25709:FeWrapperDef methodsFor: 'accessing'!
{Sequence} name
	^myName!
*/
}
/**
 * Tell whoever cares about the spec for this type
 */
public void setSpec(FeWrapperSpec spec) {
	if (mySpecHolder != null) {
		mySpecHolder.invokeFunction(spec);
	}
/*
udanax-top.st:25712:FeWrapperDef methodsFor: 'accessing'!
{void} setSpec: spec {FeWrapperSpec}
	"Tell whoever cares about the spec for this type"
	
	mySpecHolder ~~ NULL ifTrue:
		[mySpecHolder invokeFunction: spec]!
*/
}
public FeWrapperDef(Sequence name, Sequence superName, FeWrapperSpecHolder holder) {
	super();
	myName = name;
	mySuperDefName = superName;
	mySpecHolder = holder;
/*
udanax-top.st:25720:FeWrapperDef methodsFor: 'create'!
create: name {Sequence} with: superName {Sequence | NULL} with: holder {FeWrapperSpecHolder var | NULL}
	super create.
	myName := name.
	mySuperDefName := superName.
	mySpecHolder := holder.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:25729:FeWrapperDef methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:25731:FeWrapperDef methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static FeWrapperDef abstractx(Sequence wrapperName, Sequence superName, FeWrapperSpecHolder holder) {
	return new FeAbstractWrapperDef(wrapperName, superName, holder);
/*
udanax-top.st:25742:FeWrapperDef class methodsFor: 'pseudo constructors'!
{FeWrapperDef} abstract: wrapperName {Sequence}
	with: superName {Sequence | NULL}
	with: holder {FeWrapperSpecHolder var | NULL}
	^FeAbstractWrapperDef create: wrapperName with: superName with: holder!
*/
}
public static FeWrapperDef makeDirect(Sequence wrapperName, Sequence superName, FeWrapperSpecHolder holder, FeDirectWrapperMaker maker, FeDirectWrapperChecker checker) {
	return new FeDirectWrapperDef(wrapperName, superName, holder, maker, checker);
/*
udanax-top.st:25748:FeWrapperDef class methodsFor: 'pseudo constructors'!
{FeWrapperDef} makeDirect: wrapperName {Sequence}
	with: superName {Sequence | NULL}
	with: holder {FeWrapperSpecHolder var | NULL}
	with: maker {FeDirectWrapperMaker var}
	with: checker {FeDirectWrapperChecker var}
	^FeDirectWrapperDef create: wrapperName with: superName with: holder with: maker with: checker!
*/
}
public static FeWrapperDef makeIndirect(Sequence wrapperName, Sequence superName, FeWrapperSpecHolder holder, Sequence innerName, FeIndirectWrapperMaker maker, FeIndirectWrapperChecker checker) {
	return new FeIndirectWrapperDef(wrapperName, superName, holder, innerName, maker, checker);
/*
udanax-top.st:25756:FeWrapperDef class methodsFor: 'pseudo constructors'!
{FeWrapperDef} makeIndirect: wrapperName {Sequence}
	with: superName {Sequence | NULL}
	with: holder {FeWrapperSpecHolder var | NULL}
	with: innerName {Sequence | NULL}
	with: maker {FeIndirectWrapperMaker var}
	with: checker {FeIndirectWrapperChecker var}
	^FeIndirectWrapperDef create: wrapperName with: superName with: holder with: innerName with: maker with: checker!
*/
}
public FeWrapperDef() {
/*

Generated during transformation
*/
}
public FeWrapperDef(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
