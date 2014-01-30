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
import info.dgjones.abora.gold.java.missing.FeWrapperSpecHolder;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.wrapper.FeAbstractWrapperDef;
import info.dgjones.abora.gold.wrapper.FeAbstractWrapperSpec;
import info.dgjones.abora.gold.wrapper.FeWrapperDef;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class FeAbstractWrapperDef extends FeWrapperDef {

/*
udanax-top.st:25765:
FeWrapperDef subclass: #FeAbstractWrapperDef
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-wrapper'!
*/
/*
udanax-top.st:25769:
(FeAbstractWrapperDef getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeAbstractWrapperDef.class).setAttributes( new Set().add("CONCRETE").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public FeAbstractWrapperDef(Sequence name, Sequence superName, FeWrapperSpecHolder holder) {
	super(name, superName, holder);
/*
udanax-top.st:25774:FeAbstractWrapperDef methodsFor: 'create'!
create: name {Sequence} with: superName {Sequence | NULL} with: holder {FeWrapperSpecHolder var | NULL}
	super create: name with: superName with: holder.!
*/
}
public FeWrapperSpec makeSpec() {
	return FeAbstractWrapperSpec.make(this);
/*
udanax-top.st:25780:FeAbstractWrapperDef methodsFor: 'accessing'!
{FeWrapperSpec} makeSpec
	
	^FeAbstractWrapperSpec make: self!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:25786:FeAbstractWrapperDef methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:25788:FeAbstractWrapperDef methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public FeAbstractWrapperDef() {
/*

Generated during transformation
*/
}
public FeAbstractWrapperDef(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
