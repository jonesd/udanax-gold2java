/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.gchooks;

import info.dgjones.abora.gold.gchooks.RepairEngineer;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * RepairEngineers are invoked at the top of server loops and the like in order to perform
 * damage control after such events as a conservative GC or a conservative purge in response
 * to an resource emergency with a deep stack.  These modules should implement subclasses of
 * RepairEngineer (RE) which implement the method {void} repair.
 * REs are registered by construction and deregistered by destruction.
 */
public class RepairEngineer extends Heaper {

	protected RepairEngineer myNext;
	protected RepairEngineer myPrev;
	protected static RepairEngineer FirstEngineer;
/*
udanax-top.st:42104:
Heaper subclass: #RepairEngineer
	instanceVariableNames: '
		myNext {RepairEngineer}
		myPrev {RepairEngineer wimpy}'
	classVariableNames: 'FirstEngineer {RepairEngineer} '
	poolDictionaries: ''
	category: 'Xanadu-gchooks'!
*/
/*
udanax-top.st:42110:
RepairEngineer comment:
'RepairEngineers are invoked at the top of server loops and the like in order to perform damage control after such events as a conservative GC or a conservative purge in response to an resource emergency with a deep stack.  These modules should implement subclasses of RepairEngineer (RE) which implement the method {void} repair.
REs are registered by construction and deregistered by destruction.'!
*/
/*
udanax-top.st:42113:
(RepairEngineer getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:42157:
RepairEngineer class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:42160:
(RepairEngineer getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RepairEngineer.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public RepairEngineer() {
	super();
	if (FirstEngineer != null) {
		FirstEngineer.setPrev(this);
	}
	myNext = FirstEngineer;
	myPrev = null;
	FirstEngineer = this;
/*
udanax-top.st:42118:RepairEngineer methodsFor: 'protected: create'!
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
	if (myPrev != null && (myPrev instanceof RepairEngineer)) {
		myPrev.setNext(myNext);
	}
	else {
		FirstEngineer = myNext;
	}
	if (myNext != null && (myNext instanceof RepairEngineer)) {
		myNext.setPrev(myPrev);
	}
	super.destruct();
/*
udanax-top.st:42126:RepairEngineer methodsFor: 'protected: create'!
{void} destruct
	(myPrev ~~ NULL and: [myPrev isKindOf: RepairEngineer])
		ifTrue: [ myPrev setNext: myNext ]
		ifFalse: [ FirstEngineer := myNext cast: RepairEngineer ].
	(myNext ~~ NULL and: [myNext isKindOf: RepairEngineer]) ifTrue:
		[ myNext setPrev: myPrev ].
	super destruct.!
*/
}
public void repair() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:42136:RepairEngineer methodsFor: 'invoking'!
{void} repair
	self subclassResponsibility!
*/
}
public RepairEngineer next() {
	return myNext;
/*
udanax-top.st:42141:RepairEngineer methodsFor: 'private: accessing'!
{RepairEngineer INLINE} next
	^ myNext!
*/
}
public void setNext(RepairEngineer n) {
	myNext = n;
/*
udanax-top.st:42144:RepairEngineer methodsFor: 'private: accessing'!
{void INLINE} setNext: n {RepairEngineer}
	myNext :=  n!
*/
}
public void setPrev(RepairEngineer n) {
	myPrev = n;
/*
udanax-top.st:42147:RepairEngineer methodsFor: 'private: accessing'!
{void INLINE} setPrev: n {RepairEngineer}
	myPrev :=  n!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:42152:RepairEngineer methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:42154:RepairEngineer methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void linkTimeNonInherited() {
	FirstEngineer = null;
/*
udanax-top.st:42165:RepairEngineer class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	FirstEngineer := NULL!
*/
}
public static void repairThings() {
	RepairEngineer se;
	se = FirstEngineer;
	while (se != null) {
		se.repair();
		se = se.next();
	}
/*
udanax-top.st:42170:RepairEngineer class methodsFor: 'repairing'!
{void} repairThings
	| se {RepairEngineer} |
	se := FirstEngineer.
	[se ~~ NULL] whileTrue:
		[ se repair.
		  se := se next ]!
*/
}
public RepairEngineer(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
