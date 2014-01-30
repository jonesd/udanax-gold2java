/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.TransferGeneralist;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class TransferGeneralist extends TransferSpecialist {

/*
udanax-top.st:63312:
TransferSpecialist subclass: #TransferGeneralist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:63316:
(TransferGeneralist getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:63337:
TransferGeneralist class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:63340:
(TransferGeneralist getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TransferGeneralist.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public TransferGeneralist(Cookbook aBook) {
	super(aBook);
/*
udanax-top.st:63321:TransferGeneralist methodsFor: 'create'!
create: aBook {Cookbook}
	super create: aBook.!
*/
}
/**
 * No special cases.  Punt to the rcvr.
 */
public Heaper receiveHeaperFrom(Category cat, SpecialistRcvr rcvr) {
	return rcvr.basicReceive((getRecipe(cat)));
/*
udanax-top.st:63326:TransferGeneralist methodsFor: 'communication'!
{Heaper} receiveHeaper: cat {Category} from: rcvr {SpecialistRcvr}
	"No special cases.  Punt to the rcvr."
	
	^rcvr basicReceive: (self getRecipe: cat)!
*/
}
/**
 * No special cases.  Punt to the rcvr.
 */
public void receiveHeaperIntoFrom(Category cat, Heaper memory, SpecialistRcvr rcvr) {
	rcvr.basicReceiveInto((getRecipe(cat)), memory);
/*
udanax-top.st:63331:TransferGeneralist methodsFor: 'communication'!
{void} receiveHeaper: cat {Category} into: memory {Heaper} from: rcvr {SpecialistRcvr}
	"No special cases.  Punt to the rcvr."
	
	rcvr basicReceive: (self getRecipe: cat) into: memory!
*/
}
public static TransferSpecialist make(Cookbook aBook) {
	return new TransferGeneralist(aBook);
/*
udanax-top.st:63345:TransferGeneralist class methodsFor: 'creation'!
{TransferSpecialist} make: aBook {Cookbook}
	^self create: aBook!
*/
}
public TransferGeneralist() {
/*

Generated during transformation
*/
}
public TransferGeneralist(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
