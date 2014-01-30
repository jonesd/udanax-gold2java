/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.counter.Counter;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.SimpleTurtle;
import info.dgjones.abora.gold.snarf.Turtle;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class Turtle extends Abraham {

/*
udanax-top.st:11290:
Abraham subclass: #Turtle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:11294:
(Turtle getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #DEFERRED.LOCKED; yourself)!
*/
/*
udanax-top.st:11351:
Turtle class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:11354:
(Turtle getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #DEFERRED.LOCKED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Turtle.class).setAttributes( new Set().add("DEFERRED").add("COPY").add("SHEPHERDPATRIARCH").add("DEFERREDLOCKED"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:11299:Turtle methodsFor: 'accessing'!
{Category} bootCategory
	self subclassResponsibility!
*/
}
public Heaper bootHeaper() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:11302:Turtle methodsFor: 'accessing'!
{Heaper} bootHeaper
	self subclassResponsibility!
*/
}
public Cookbook cookbook() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:11305:Turtle methodsFor: 'accessing'!
{Cookbook} cookbook
	self subclassResponsibility!
*/
}
public Counter counter() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:11308:Turtle methodsFor: 'accessing'!
{Counter} counter
	self subclassResponsibility!
*/
}
/**
 * Under all normal conditions, a Turtle has an Agenda.  However, during the construction of
 * a Turtle, there may arise situations when a piece of code is invoked which normally asks
 * the Turtle for its agenda before the Turtle is mature enough to have one.
 */
public Agenda fetchAgenda() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:11311:Turtle methodsFor: 'accessing'!
{Agenda | NULL} fetchAgenda
	"Under all normal conditions, a Turtle has an Agenda.  However, during the construction of a Turtle, there may arise situations when a piece of code is invoked which normally asks the Turtle for its agenda before the Turtle is mature enough to have one."
	
	self subclassResponsibility!
*/
}
/**
 * See Turtle::fetchAgenda()
 */
public Agenda getAgenda() {
	Agenda result;
	result = fetchAgenda();
	if (result == null) {
		throw new AboraRuntimeException(AboraRuntimeException.TURTLE_NOT_MATURE);
	}
	return result;
/*
udanax-top.st:11316:Turtle methodsFor: 'accessing'!
{Agenda} getAgenda
	"See Turtle::fetchAgenda()"
	
	| result {Agenda | NULL} |
	result _ self fetchAgenda.
	result == NULL
		ifTrue: [Heaper BLAST: #TurtleNotMature].
	^result!
*/
}
public XcvrMaker protocol() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:11325:Turtle methodsFor: 'accessing'!
{XcvrMaker} protocol
	self subclassResponsibility!
*/
}
public void saveBootHeaper(Heaper boot) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:11328:Turtle methodsFor: 'accessing'!
{void} saveBootHeaper: boot {Heaper}
	self subclassResponsibility!
*/
}
public void setProtocol(XcvrMaker xcvrMaker, Cookbook book) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:11331:Turtle methodsFor: 'accessing'!
{void} setProtocol: xcvrMaker {XcvrMaker} with: book {Cookbook}
	self subclassResponsibility!
*/
}
public Turtle() {
	super();
/*
udanax-top.st:11336:Turtle methodsFor: 'protected: creation'!
create
	super create!
*/
}
public Turtle(int hash) {
	super(hash);
/*
udanax-top.st:11339:Turtle methodsFor: 'protected: creation'!
create: hash {UInt32}
	super create: hash!
*/
}
public Turtle(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:11344:Turtle methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:11347:Turtle methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static Turtle make(Cookbook cookbook, Category bootCategory, XcvrMaker maker) {
	return SimpleTurtle.make(cookbook, bootCategory, maker);
/*
udanax-top.st:11359:Turtle class methodsFor: 'pseudo-constructors'!
make: cookbook {Cookbook} with:  bootCategory {Category} with: maker {XcvrMaker}
	^SimpleTurtle make: cookbook with: bootCategory with: maker!
*/
}
}
