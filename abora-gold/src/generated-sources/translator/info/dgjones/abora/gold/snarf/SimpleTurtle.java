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
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.SimpleTurtle;
import info.dgjones.abora.gold.snarf.Turtle;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class SimpleTurtle extends Turtle {

	protected Counter myCounter;
	protected Heaper myBootHeaper;
	protected XcvrMaker myProtocol;
	protected Cookbook myCookbook;
	protected Category myBootCategory;
	protected Agenda myAgenda;
/*
udanax-top.st:11439:
Turtle subclass: #SimpleTurtle
	instanceVariableNames: '
		myCounter {Counter}
		myBootHeaper {Heaper}
		myProtocol {XcvrMaker NOCOPY}
		myCookbook {Cookbook NOCOPY}
		myBootCategory {Category}
		myAgenda {Agenda | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:11449:
(SimpleTurtle getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #LOCKED; add: #COPY; yourself)!
*/
/*
udanax-top.st:11552:
SimpleTurtle class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:11555:
(SimpleTurtle getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #LOCKED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SimpleTurtle.class).setAttributes( new Set().add("CONCRETE").add("LOCKED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	return myBootCategory;
/*
udanax-top.st:11454:SimpleTurtle methodsFor: 'accessing'!
{Category} bootCategory
	^myBootCategory!
*/
}
public Heaper bootHeaper() {
	return myBootHeaper;
/*
udanax-top.st:11457:SimpleTurtle methodsFor: 'accessing'!
{Heaper} bootHeaper
	^myBootHeaper!
*/
}
public Cookbook cookbook() {
	return myCookbook;
/*
udanax-top.st:11460:SimpleTurtle methodsFor: 'accessing'!
{Cookbook} cookbook
	^myCookbook!
*/
}
public Counter counter() {
	return myCounter;
/*
udanax-top.st:11463:SimpleTurtle methodsFor: 'accessing'!
{Counter} counter
	^myCounter!
*/
}
public Agenda fetchAgenda() {
	return myAgenda;
/*
udanax-top.st:11466:SimpleTurtle methodsFor: 'accessing'!
{Agenda | NULL} fetchAgenda
	^myAgenda!
*/
}
public XcvrMaker protocol() {
	return myProtocol;
/*
udanax-top.st:11470:SimpleTurtle methodsFor: 'accessing'!
{XcvrMaker} protocol
	^myProtocol!
*/
}
public void saveBootHeaper(Heaper boot) {
	if ( ! (myBootHeaper == null)) {
		throw new AboraRuntimeException(AboraRuntimeException.DONT_CHANGE_TURTLES_BOOT_HEAPER);
	}
	else {
		AboraBlockSupport.enterConsistent(1);
		try {
			myBootHeaper = boot;
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
/*
udanax-top.st:11473:SimpleTurtle methodsFor: 'accessing'!
{void} saveBootHeaper: boot {Heaper} 
	
	myBootHeaper == NULL
		ifFalse: [Turtle BLAST: #DontChangeTurtlesBootHeaper]
		ifTrue: 
			[DiskManager consistent: 1 with:
				[myBootHeaper _ boot.
				self diskUpdate]]!
*/
}
public void setProtocol(XcvrMaker xcvrMaker, Cookbook book) {
	myProtocol = xcvrMaker;
	myCookbook = book;
/*
udanax-top.st:11482:SimpleTurtle methodsFor: 'accessing'!
{void} setProtocol: xcvrMaker {XcvrMaker} with: book {Cookbook}
	myProtocol _ xcvrMaker.
	myCookbook _ book.!
*/
}
public int contentsHash() {
	return ((super.contentsHash() ^ myCounter.hashForEqual()) ^ myBootHeaper.hashForEqual()) ^ myProtocol.hashForEqual();
/*
udanax-top.st:11488:SimpleTurtle methodsFor: 'testing'!
{UInt32} contentsHash
	^((super contentsHash
		bitXor: myCounter hashForEqual)
		bitXor: myBootHeaper hashForEqual)
		bitXor: myProtocol hashForEqual!
*/
}
public void restartSimpleTurtle(Rcvr rcvr) {
	myProtocol = XcvrMaker.make();
	/* The bogus protocol */
	myCookbook = Cookbook.make();
	/* with the empty cookbook */
/*
udanax-top.st:11497:SimpleTurtle methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartSimpleTurtle: rcvr {Rcvr unused default: NULL}
	myProtocol _ XcvrMaker make.  "The bogus protocol"
	myCookbook _ Cookbook make    "with the empty cookbook"!
*/
}
public SimpleTurtle(Cookbook cookbook, Category bootCategory, XcvrMaker maker) {
	super(1);
	DiskManager packer;
	packer = (DiskManager) ((DiskManager) CurrentPacker.fluidGet());
	AboraBlockSupport.enterConsistent(1);
	try {
		myCounter = null;
		myBootHeaper = null;
		myProtocol = maker;
		myCookbook = cookbook;
		myBootCategory = bootCategory;
		myAgenda = null;
		packer.storeInitialFlock(this, myProtocol, cookbook);
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
	AboraBlockSupport.enterConsistent(3);
	try {
		Someone.thingToDo();
		/* tune the number 5000 */
		myCounter = Counter.fakeCounter(3, 5000, 2);
		packer.setHashCounter(myCounter);
		remember();
		myCounter.newShepherd();
		myCounter.remember();
		myAgenda = Agenda.make();
		myAgenda.rememberYourself();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:11503:SimpleTurtle methodsFor: 'protected: creation'!
create: cookbook {Cookbook} with: bootCategory {Category} with: maker {XcvrMaker} 
	
	| packer {DiskManager} |
	super create: 1.
	packer _ CurrentPacker fluidGet cast: DiskManager.
	DiskManager consistent: 1 with:
		[myCounter _ NULL.
		myBootHeaper _ NULL.
		myProtocol _ maker.
		myCookbook _ cookbook.
		myBootCategory _ bootCategory.
		myAgenda _ NULL.
		packer storeInitialFlock: self
			with: myProtocol
			with: cookbook].
	DiskManager consistent: 3 with:
		[self thingToDo. "tune the number 5000"
		myCounter _ Counter fakeCounter: 3 with: 5000 with: 2.
		packer setHashCounter: myCounter.
		self remember.
		myCounter newShepherd.
		myCounter remember.
		myAgenda _ Agenda make.
		myAgenda rememberYourself]!
*/
}
/**
 * @deprecated
 */
public void newCounter(Counter counter) {
	throw new PasseException();
/*
udanax-top.st:11530:SimpleTurtle methodsFor: 'smalltalk: passe'!
{void} newCounter: counter {Counter}
	
	self passe!
*/
}
public SimpleTurtle(Rcvr receiver) {
	super(receiver);
	myCounter = (Counter) receiver.receiveHeaper();
	myBootHeaper = receiver.receiveHeaper();
	myBootCategory = (Category) receiver.receiveHeaper();
	myAgenda = (Agenda) receiver.receiveHeaper();
	restartSimpleTurtle(receiver);
/*
udanax-top.st:11536:SimpleTurtle methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCounter _ receiver receiveHeaper.
	myBootHeaper _ receiver receiveHeaper.
	myBootCategory _ receiver receiveHeaper.
	myAgenda _ receiver receiveHeaper.
	self restartSimpleTurtle: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myCounter);
	xmtr.sendHeaper(myBootHeaper);
	xmtr.sendHeaper(myBootCategory);
	xmtr.sendHeaper(myAgenda);
/*
udanax-top.st:11544:SimpleTurtle methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCounter.
	xmtr sendHeaper: myBootHeaper.
	xmtr sendHeaper: myBootCategory.
	xmtr sendHeaper: myAgenda.!
*/
}
public static Turtle make(Cookbook cookbook, Category bootCategory, XcvrMaker maker) {
	return new SimpleTurtle(cookbook, bootCategory, maker);
/*
udanax-top.st:11560:SimpleTurtle class methodsFor: 'pseudo-constructors'!
make: cookbook {Cookbook} with:  bootCategory {Category} with: maker {XcvrMaker}
	^SimpleTurtle create: cookbook with: bootCategory with: maker!
*/
}
public SimpleTurtle() {
/*

Generated during transformation
*/
}
}
