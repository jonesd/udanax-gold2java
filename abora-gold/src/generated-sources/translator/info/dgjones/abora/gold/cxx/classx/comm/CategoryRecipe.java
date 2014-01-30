/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cxx.classx.comm;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.cxx.classx.comm.CategoryRecipe;
import info.dgjones.abora.gold.cxx.otherclass.CopyRecipe;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Association;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.java.missing.smalltalk.Smalltalk;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class CategoryRecipe extends CopyRecipe {

/*
udanax-top.st:41970:
CopyRecipe subclass: #CategoryRecipe
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cxx-class-comm'!
*/
/*
udanax-top.st:41974:
(CategoryRecipe getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:41998:
CategoryRecipe class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:42001:
(CategoryRecipe getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CategoryRecipe.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper parse(SpecialistRcvr rcvr) {
	Category cat;
	cat = (rcvr).receiveCategory();
	rcvr.registerIbid(cat);
	return cat;
/*
udanax-top.st:41979:CategoryRecipe methodsFor: 'accessing'!
{Heaper} parse: rcvr {SpecialistRcvr}
	| cat {Category} |
	cat _ (rcvr cast: SpecialistRcvr) receiveCategory.
	rcvr registerIbid: cat.
	^cat!
*/
}
public void parseInto(Rcvr rcvr, PtrArray memory) {
	throw new AboraRuntimeException(AboraRuntimeException.NOT_BECOMABLE);
/*
udanax-top.st:41986:CategoryRecipe methodsFor: 'accessing'!
{void} parse: rcvr {Rcvr unused} into: memory {void star unused}
	Heaper BLAST: #NotBecomable!
*/
}
/**
 * cuisine points to the *variable* in which the receiver should be registered.
 */
public CategoryRecipe(Category cat, Association cuisine) {
	super(cat, cuisine);
/*
udanax-top.st:41992:CategoryRecipe methodsFor: 'creation'!
create: cat {Category} with: cuisine {Recipe star vector}
	"cuisine points to the *variable* in which the receiver should be registered."
	
	super create: cat with: cuisine!
*/
}
public static void staticTimeNonInherited() {
	/* Removed translateOnly */
	new CategoryRecipe(AboraSupport.findCategory(Category.class), (Smalltalk.associationAt(XPP_CUISINE)));
/*
udanax-top.st:42006:CategoryRecipe class methodsFor: 'smalltalk: smalltalk initialization'!
staticTimeNonInherited
	
	'extern Category * cat_Category;
		CategoryRecipe categoryRecipe(cat_Category, &XppCuisine);' translateOnly.
	[self create: Category with: (Smalltalk associationAt: #XppCuisine)] smalltalkOnly!
*/
}
public CategoryRecipe() {
/*

Generated during transformation
*/
}
public CategoryRecipe(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
