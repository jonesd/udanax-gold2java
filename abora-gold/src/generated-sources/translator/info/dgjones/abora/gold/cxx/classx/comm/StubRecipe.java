/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cxx.classx.comm;

import info.dgjones.abora.gold.cxx.classx.comm.StubRecipe;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Association;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class StubRecipe extends Recipe {

/*
udanax-top.st:42012:
Recipe subclass: #StubRecipe
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cxx-class-comm'!
*/
/*
udanax-top.st:42016:
(StubRecipe getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(StubRecipe.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper parseStub(Rcvr rcvr, int hash) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:42021:StubRecipe methodsFor: 'accessing'!
{Heaper} parseStub: rcvr {Rcvr} with: hash {UInt32}
	
	self subclassResponsibility!
*/
}
/**
 * cuisine points to the *variable* in which the receiver should be registered.
 */
public StubRecipe(Category cat, Association cuisine) {
	super(cat, cuisine);
/*
udanax-top.st:42027:StubRecipe methodsFor: 'protected: creation'!
create: cat {Category} with: cuisine {Recipe star vector}
	"cuisine points to the *variable* in which the receiver should be registered."
	
	super create: cat with: cuisine!
*/
}
public StubRecipe() {
/*

Generated during transformation
*/
}
public StubRecipe(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
