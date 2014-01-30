/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.gchooks;

import info.dgjones.abora.gold.gchooks.SanitationEngineer;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * SanitationEngineers are used by modules that can perform clever resource management at
 * garbage collection time.  These modules should implement subclasses of SanitationEngineer
 * (SE) which implement the method {void} recycle.
 * The garbage collector calls the recycle method for each existing SE prior to the marking
 * phase.  SEs are registered
 * by construction and deregistered by destruction.
 */
public class SanitationEngineer extends Heaper {

	protected SanitationEngineer myNext;
	protected SanitationEngineer myPrev;
	protected static SanitationEngineer FirstEngineer;
/*
udanax-top.st:44892:
Heaper subclass: #SanitationEngineer
	instanceVariableNames: '
		myNext {SanitationEngineer}
		myPrev {SanitationEngineer wimpy}'
	classVariableNames: 'FirstEngineer {SanitationEngineer} '
	poolDictionaries: ''
	category: 'Xanadu-gchooks'!
*/
/*
udanax-top.st:44898:
SanitationEngineer comment:
'SanitationEngineers are used by modules that can perform clever resource management at garbage collection time.  These modules should implement subclasses of SanitationEngineer (SE) which implement the method {void} recycle.
The garbage collector calls the recycle method for each existing SE prior to the marking phase.  SEs are registered
by construction and deregistered by destruction.'!
*/
/*
udanax-top.st:44902:
(SanitationEngineer getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:44946:
SanitationEngineer class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:44949:
(SanitationEngineer getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SanitationEngineer.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public void recycle(boolean required) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:44907:SanitationEngineer methodsFor: 'invoking'!
{void} recycle: required {BooleanVar}
	self subclassResponsibility!
*/
}
public SanitationEngineer() {
	super();
	if (FirstEngineer != null) {
		FirstEngineer.setPrev(this);
	}
	myNext = FirstEngineer;
	myPrev = null;
	FirstEngineer = this;
/*
udanax-top.st:44912:SanitationEngineer methodsFor: 'protected: create'!
create
	super create.
	FirstEngineer ~~ NULL ifTrue:
		[ FirstEngineer setPrev: self ].
	myNext := FirstEngineer.
	myPrev := NULL.
	FirstEngineer := self!
*/
}
public void destruct() {
	if (myPrev != null && (myPrev instanceof SanitationEngineer)) {
		myPrev.setNext(myNext);
	}
	else {
		FirstEngineer = myNext;
	}
	if (myNext != null && (myNext instanceof SanitationEngineer)) {
		myNext.setPrev(myPrev);
	}
	super.destruct();
/*
udanax-top.st:44920:SanitationEngineer methodsFor: 'protected: create'!
{void} destruct
	(myPrev ~~ NULL and: [myPrev isKindOf: SanitationEngineer])
		ifTrue: [ myPrev setNext: myNext ]
		ifFalse: [ FirstEngineer := myNext cast: SanitationEngineer ].
	(myNext ~~ NULL and: [myNext isKindOf: SanitationEngineer]) ifTrue:
		[ myNext setPrev: myPrev ].
	super destruct.!
*/
}
public SanitationEngineer next() {
	return myNext;
/*
udanax-top.st:44930:SanitationEngineer methodsFor: 'private: accessing'!
{SanitationEngineer INLINE} next
	^ myNext!
*/
}
public void setNext(SanitationEngineer n) {
	myNext = n;
/*
udanax-top.st:44933:SanitationEngineer methodsFor: 'private: accessing'!
{void INLINE} setNext: n {SanitationEngineer}
	myNext :=  n!
*/
}
public void setPrev(SanitationEngineer p) {
	myPrev = p;
/*
udanax-top.st:44936:SanitationEngineer methodsFor: 'private: accessing'!
{void INLINE} setPrev: p {SanitationEngineer}
	myPrev := p!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:44941:SanitationEngineer methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:44943:SanitationEngineer methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void linkTimeNonInherited() {
	FirstEngineer = null;
/*
udanax-top.st:44954:SanitationEngineer class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	FirstEngineer := NULL!
*/
}
public static void garbageDay(boolean required) {
	SanitationEngineer se;
	se = FirstEngineer;
	while (se != null) {
		se.recycle(required);
		se = se.next();
	}
/*
udanax-top.st:44959:SanitationEngineer class methodsFor: 'sanitizing'!
{void} garbageDay: required {BooleanVar}
	| se {SanitationEngineer} |
	se := FirstEngineer.
	[se ~~ NULL] whileTrue:
		[ se recycle: required.
		  se := se next ]!
*/
}
public SanitationEngineer(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
