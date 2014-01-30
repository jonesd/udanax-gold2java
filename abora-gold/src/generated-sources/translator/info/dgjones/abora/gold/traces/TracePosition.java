/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.traces;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimIndexTable;
import info.dgjones.abora.gold.traces.BoundedTrace;
import info.dgjones.abora.gold.traces.BranchDescription;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class TracePosition extends Heaper {

/*
udanax-top.st:62873:
Heaper subclass: #TracePosition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Traces'!
*/
/*
udanax-top.st:62877:
(TracePosition getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:62912:
TracePosition class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:62915:
(TracePosition getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TracePosition.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Hash based on both the branch and position.
 */
public int actualHashForEqual() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:62882:TracePosition methodsFor: 'testing'!
{UInt32} actualHashForEqual
	"Hash based on both the branch and position."
	self subclassResponsibility!
*/
}
public boolean isEqual(Heaper another) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:62887:TracePosition methodsFor: 'testing'!
{BooleanVar} isEqual: another {Heaper} 
	self subclassResponsibility!
*/
}
public boolean isLE(TracePosition another) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:62890:TracePosition methodsFor: 'testing'!
{BooleanVar} isLE: another {TracePosition}
	self subclassResponsibility!
*/
}
public BranchDescription branch() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:62895:TracePosition methodsFor: 'accessing'!
{BranchDescription} branch
	self subclassResponsibility!
*/
}
public void cacheIn(PrimIndexTable navCache) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:62898:TracePosition methodsFor: 'accessing'!
{void} cacheIn: navCache {PrimIndexTable}
	self subclassResponsibility!
*/
}
public TracePosition newSuccessor() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:62901:TracePosition methodsFor: 'accessing'!
{TracePosition} newSuccessor
	self subclassResponsibility!
*/
}
/**
 * Return a new tracePosition that is after both the receiver and tracePos.
 */
public TracePosition newSuccessorAfter(TracePosition tracePos) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:62904:TracePosition methodsFor: 'accessing'!
{TracePosition} newSuccessorAfter: tracePos {TracePosition}
	"Return a new tracePosition that is after both the receiver and tracePos."
	self subclassResponsibility!
*/
}
public int position() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:62908:TracePosition methodsFor: 'accessing'!
{UInt32} position
	self subclassResponsibility!
*/
}
public static TracePosition make(BranchDescription branch, int position) {
	return new BoundedTrace(branch, position);
/*
udanax-top.st:62920:TracePosition class methodsFor: 'pseudo-constructors'!
make: branch {BranchDescription} with: position {UInt32}
	^BoundedTrace create: branch with: position!
*/
}
public TracePosition() {
/*

Generated during transformation
*/
}
public TracePosition(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
