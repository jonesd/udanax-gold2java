/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cxx.otherclass;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.cxx.otherclass.CopyRecipe;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Association;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.DeletedHeaper;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class CopyRecipe extends Recipe {

/*
udanax-top.st:41933:
Recipe subclass: #CopyRecipe
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cxx-OtherClass'!
*/
/*
udanax-top.st:41937:
(CopyRecipe getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CopyRecipe.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * create a new object from the information in the transceiver
 */
public Heaper parse(SpecialistRcvr rcvr) {
	Heaper result;
	AboraSupport.translateOnly();
	{
		/*  
	result = Heaper::operator new (0, xcsj, this->categoryOfDish()); */
	}
	AboraSupport.smalltalkOnly();
	{
		result = new DeletedHeaper();
	}
	rcvr.registerIbid((result));
	parseInto(rcvr, (PtrArray) result);
	return result;
/*
udanax-top.st:41942:CopyRecipe methodsFor: 'accessing'!
{Heaper} parse: rcvr {SpecialistRcvr}
	"create a new object from the information in the transceiver"
	| result {void star} |	
	
	' 
	result = Heaper::operator new (0, xcsj, this->categoryOfDish());' translateOnly.
	
	[result _ DeletedHeaper create] smalltalkOnly.
	
	rcvr registerIbid: (result basicCast: Heaper).
	self parse: rcvr into: result.
	^ result basicCast: Heaper.!
*/
}
/**
 * create a new object from the information in the rcvr and give it the identity
 * of memory. The c++ version of this builds the first object received into the
 * area of supplied memory.
 */
public void parseInto(Rcvr rcvr, PtrArray memory) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41956:CopyRecipe methodsFor: 'accessing'!
{void} parse: rcvr {Rcvr} into: memory {void star} 
	"create a new object from the information in the rcvr and give it the identity 
	of memory. The c++ version of this builds the first object received into the 
	area of supplied memory."
	
	self subclassResponsibility!
*/
}
/**
 * cuisine points to the *variable* in which the receiver should be registered.
 */
public CopyRecipe(Category cat, Association cuisine) {
	super(cat, cuisine);
/*
udanax-top.st:41965:CopyRecipe methodsFor: 'protected: creation'!
create: cat {Category} with: cuisine {Recipe star vector}
	"cuisine points to the *variable* in which the receiver should be registered."
	
	super create: cat with: cuisine!
*/
}
public CopyRecipe() {
/*

Generated during transformation
*/
}
public CopyRecipe(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
