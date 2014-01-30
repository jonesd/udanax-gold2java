/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nadmin;

import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.locks.Lock;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Describes how to obtain the authority of a Club.
 */
public class FeLockSmith extends FeWrapper {

	protected static FeWrapperSpec TheLockSmithSpec;
/*
udanax-top.st:24523:
FeWrapper subclass: #FeLockSmith
	instanceVariableNames: ''
	classVariableNames: 'TheLockSmithSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-nadmin'!
*/
/*
udanax-top.st:24527:
FeLockSmith comment:
'Describes how to obtain the authority of a Club.'!
*/
/*
udanax-top.st:24529:
(FeLockSmith getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:24546:
FeLockSmith class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:24549:
(FeLockSmith getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeLockSmith.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Create a new lock which, if satisfied, will give access to this club. If Club is NULL,
 * then the lock will never be satisfied.
 */
public Lock newLock(ID clubID) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:24534:FeLockSmith methodsFor: 'server locks'!
{Lock} newLock: clubID {ID unused | NULL}
	"Create a new lock which, if satisfied, will give access to this club. If Club is NULL, then the lock will never be satisfied."
	self subclassResponsibility!
*/
}
public FeLockSmith(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*
udanax-top.st:24541:FeLockSmith methodsFor: 'protected: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create: edition with: spec!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.ABSTRACTWRAPPER("LockSmith", "Wrapper", FE_LOCK_SMITH);
/*
udanax-top.st:24554:FeLockSmith class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	FeWrapperSpec ABSTRACTWRAPPER: 'LockSmith' with: 'Wrapper' with: #FeLockSmith!
*/
}
public static void linkTimeNonInherited() {
	TheLockSmithSpec = null;
/*
udanax-top.st:24558:FeLockSmith class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	TheLockSmithSpec := NULL.!
*/
}
public static void setSpec(FeWrapperSpec spec) {
	TheLockSmithSpec = spec;
/*
udanax-top.st:24564:FeLockSmith class methodsFor: 'private: wrapping'!
{void} setSpec: spec {FeWrapperSpec}
	TheLockSmithSpec := spec.!
*/
}
public static FeWrapperSpec spec() {
	return TheLockSmithSpec;
/*
udanax-top.st:24570:FeLockSmith class methodsFor: 'pseudo constructors'!
{FeWrapperSpec} spec
	^TheLockSmithSpec!
*/
}
public FeLockSmith() {
/*

Generated during transformation
*/
}
public FeLockSmith(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
