/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cobbler;

import info.dgjones.abora.gold.cobbler.BootPlan;
import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.java.missing.smalltalk.Smalltalk;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xpp.basic.Category;

public class BootPlan extends Thunk {

/*
udanax-top.st:56798:
Thunk subclass: #BootPlan
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cobbler'!
*/
/*
udanax-top.st:56802:
(BootPlan getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:56824:
BootPlan class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:56827:
(BootPlan getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BootPlan.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:56807:BootPlan methodsFor: 'accessing'!
{Category} bootCategory
	self subclassResponsibility!
*/
}
/**
 * Return the object representing the connection.  This gives the client a handle by which to
 * terminate the connection.
 */
public Connection connection() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:56810:BootPlan methodsFor: 'accessing'!
{Connection} connection
	"Return the object representing the connection.  This gives the client a handle by which to terminate the connection."
	
	self subclassResponsibility!
*/
}
/**
 * A comm hook couldn't register the bootPlan because it's working with a not-fully
 * constructed object, so we have to make bootPlans thunks and register them here.
 */
public void execute() {
	Connection.registerBootPlan(this);
/*
udanax-top.st:56817:BootPlan methodsFor: 'operate'!
{void} execute
	"A comm hook couldn't register the bootPlan because it's working with a not-fully
	 constructed object, so we have to make bootPlans thunks and register them here."
	Connection registerBootPlan: self!
*/
}
public static void cleanupGarbage() {
	Smalltalk.atPut(BOOT_CUISINE, null);
/*
udanax-top.st:56832:BootPlan class methodsFor: 'smalltalk: init'!
cleanupGarbage
	BootCuisine _ NULL!
*/
}
public static void initTimeNonInherited() {
	Cookbook.declareCookbook("boot", AboraSupport.findCategory(BootPlan.class), Smalltalk.associationAt(BOOT_CUISINE).refValue(), Smalltalk.associationAt(XPP_CUISINE).refValue());
/*
udanax-top.st:56835:BootPlan class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	Cookbook declareCookbook: 'boot' with: BootPlan with: BootCuisine with: XppCuisine!
*/
}
public static void linkTimeNonInherited() {
	Recipe.defineGlobal(BOOT_CUISINE, null);
/*
udanax-top.st:56838:BootPlan class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	Recipe star defineGlobal: #BootCuisine with: NULL.!
*/
}
public BootPlan() {
/*

Generated during transformation
*/
}
public BootPlan(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
