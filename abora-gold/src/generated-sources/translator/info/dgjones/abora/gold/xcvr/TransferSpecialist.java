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
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.SpecialistXmtr;
import info.dgjones.abora.gold.xcvr.TransferGeneralist;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class TransferSpecialist extends Heaper {

	protected Cookbook myCookbook;
/*
udanax-top.st:63059:
Heaper subclass: #TransferSpecialist
	instanceVariableNames: 'myCookbook {Cookbook}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:63063:
(TransferSpecialist getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:63112:
TransferSpecialist class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:63115:
(TransferSpecialist getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TransferSpecialist.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public Category getCategoryFor(int no) {
	return myCookbook.getCategoryFor(no);
/*
udanax-top.st:63068:TransferSpecialist methodsFor: 'cookbook'!
{Category} getCategoryFor: no {IntegerVar}
	^myCookbook getCategoryFor: no!
*/
}
public Recipe getRecipe(Category cat) {
	return myCookbook.getRecipe(cat);
/*
udanax-top.st:63071:TransferSpecialist methodsFor: 'cookbook'!
{Recipe} getRecipe: cat {Category}
	^myCookbook getRecipe: cat!
*/
}
public int numberOfCategory(Category cat) {
	return myCookbook.numberOfCategory(cat);
/*
udanax-top.st:63074:TransferSpecialist methodsFor: 'cookbook'!
{IntegerVar} numberOfCategory: cat {Category}
	^myCookbook numberOfCategory: cat!
*/
}
/**
 * Return an object from the rcvr or NULL if cat
 * is not a category that we handle specially.
 */
public Heaper receiveHeaperFrom(Category cat, SpecialistRcvr rcvr) {
	/* Make sure all objects created get rcvr registerIbid:
	 called on them so that the rcvr doesn't get out of sync. */
	throw new SubclassResponsibilityException();
/*
udanax-top.st:63079:TransferSpecialist methodsFor: 'communication'!
{Heaper} receiveHeaper: cat {Category} from: rcvr {SpecialistRcvr}
	"Return an object from the rcvr or NULL if cat
	 is not a category that we handle specially."
	"Make sure all objects created get rcvr registerIbid:
	 called on them so that the rcvr doesn't get out of sync."
	self subclassResponsibility!
*/
}
/**
 * Return an object from the rcvr or NULL if cat is not a category that we handle specially.
 */
public void receiveHeaperIntoFrom(Category cat, Heaper memory, SpecialistRcvr rcvr) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:63087:TransferSpecialist methodsFor: 'communication'!
{void} receiveHeaper: cat {Category} into: memory {Heaper} from: rcvr {SpecialistRcvr}
	"Return an object from the rcvr or NULL if cat is not a category that we handle specially."
	self subclassResponsibility!
*/
}
/**
 * Transmit heapers on xmtr.  Subclasses intercept and handle special cases here.
 */
public void sendHeaperTo(Heaper hpr, SpecialistXmtr xmtr) {
	xmtr.startInstance(hpr, hpr.getCategory());
	hpr.sendSelfTo(xmtr);
	xmtr.endInstance();
/*
udanax-top.st:63092:TransferSpecialist methodsFor: 'communication'!
{void} sendHeaper: hpr {Heaper} to: xmtr {SpecialistXmtr}
	"Transmit heapers on xmtr.  Subclasses intercept and handle special cases here."
	
	xmtr startInstance: hpr with: hpr getCategory.
	hpr sendSelfTo: xmtr.
	xmtr endInstance!
*/
}
public TransferSpecialist(Cookbook aBook) {
	super();
	myCookbook = aBook;
/*
udanax-top.st:63101:TransferSpecialist methodsFor: 'creation'!
create: aBook {Cookbook}
	super create.
	myCookbook _ aBook!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:63107:TransferSpecialist methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:63109:TransferSpecialist methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
/**
 * Return a specialist that does nothing.
 */
public static TransferSpecialist make(Cookbook aBook) {
	return new TransferGeneralist(aBook);
/*
udanax-top.st:63120:TransferSpecialist class methodsFor: 'creation'!
make: aBook {Cookbook}
	"Return a specialist that does nothing."
	
	^TransferGeneralist create: aBook!
*/
}
public TransferSpecialist() {
/*

Generated during transformation
*/
}
public TransferSpecialist(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
