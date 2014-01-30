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
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.FakePacker;
import info.dgjones.abora.gold.snarf.MockTurtle;
import info.dgjones.abora.gold.snarf.Turtle;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * The MockTurtle is used with the FakePacker.  All it provides is an Agenda
 */
public class MockTurtle extends Turtle {

	protected Agenda myAgenda;
	protected Category myBootCategory;
/*
udanax-top.st:11362:
Turtle subclass: #MockTurtle
	instanceVariableNames: '
		myAgenda {Agenda | NULL}
		myBootCategory {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:11368:
MockTurtle comment:
'The MockTurtle is used with the FakePacker.  All it provides is an Agenda'!
*/
/*
udanax-top.st:11370:
(MockTurtle getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #LOCKED; add: #COPY; yourself)!
*/
/*
udanax-top.st:11428:
MockTurtle class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:11431:
(MockTurtle getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #LOCKED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(MockTurtle.class).setAttributes( new Set().add("CONCRETE").add("LOCKED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	return myBootCategory;
/*
udanax-top.st:11375:MockTurtle methodsFor: 'accessing'!
{Category} bootCategory
	^ myBootCategory!
*/
}
public Heaper bootHeaper() {
	throw new UnimplementedException();
/*
udanax-top.st:11378:MockTurtle methodsFor: 'accessing'!
{Heaper} bootHeaper
	self unimplemented.
	^NULL "fodder"!
*/
}
public Cookbook cookbook() {
	willNotImplement();
	return null;
/*
udanax-top.st:11382:MockTurtle methodsFor: 'accessing'!
{Cookbook} cookbook
	self willNotImplement.
	^ NULL!
*/
}
public Counter counter() {
	willNotImplement();
	return null;
/*
udanax-top.st:11386:MockTurtle methodsFor: 'accessing'!
{Counter} counter
	self willNotImplement.
	^NULL "fodder"!
*/
}
public Agenda fetchAgenda() {
	return myAgenda;
/*
udanax-top.st:11390:MockTurtle methodsFor: 'accessing'!
{Agenda | NULL} fetchAgenda
	^myAgenda!
*/
}
public XcvrMaker protocol() {
	willNotImplement();
	return null;
/*
udanax-top.st:11394:MockTurtle methodsFor: 'accessing'!
{XcvrMaker} protocol
	self willNotImplement.
	^ NULL!
*/
}
/**
 * Right
 */
public void saveBootHeaper(Heaper boot) {
	willNotImplement();
/*
udanax-top.st:11398:MockTurtle methodsFor: 'accessing'!
{void} saveBootHeaper: boot {Heaper}
	"Right"
	self willNotImplement.!
*/
}
/**
 * Right
 */
public void setProtocol(XcvrMaker xcvrMaker, Cookbook book) {
	willNotImplement();
/*
udanax-top.st:11402:MockTurtle methodsFor: 'accessing'!
{void} setProtocol: xcvrMaker {XcvrMaker} with: book {Cookbook}
	"Right"
	self willNotImplement.!
*/
}
public MockTurtle(Category bootCategory) {
	super();
	((FakePacker) ((DiskManager) CurrentPacker.fluidGet())).storeTurtle(this);
	myAgenda = null;
	myBootCategory = bootCategory;
	myAgenda = Agenda.make();
/*
udanax-top.st:11408:MockTurtle methodsFor: 'protected: creation'!
create: bootCategory {Category}
	super create.
	(CurrentPacker fluidGet cast: FakePacker) storeTurtle: self.
	myAgenda _ NULL.
	myBootCategory _ bootCategory.
	myAgenda _ Agenda make.!
*/
}
public MockTurtle(Rcvr receiver) {
	super(receiver);
	myAgenda = (Agenda) receiver.receiveHeaper();
	myBootCategory = (Category) receiver.receiveHeaper();
/*
udanax-top.st:11417:MockTurtle methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myAgenda _ receiver receiveHeaper.
	myBootCategory _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myAgenda);
	xmtr.sendHeaper(myBootCategory);
/*
udanax-top.st:11422:MockTurtle methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myAgenda.
	xmtr sendHeaper: myBootCategory.!
*/
}
public static Turtle make(Category category) {
	return new MockTurtle(category);
/*
udanax-top.st:11436:MockTurtle class methodsFor: 'pseudo-constructor'!
{Turtle} make: category {Category}
	^ self create: category!
*/
}
public MockTurtle() {
/*

Generated during transformation
*/
}
}
