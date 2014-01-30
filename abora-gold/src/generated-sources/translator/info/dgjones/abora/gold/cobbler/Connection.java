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
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimPtr2PtrTable;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Suclasses represent particular kinds of connections.  The connection object serves two
 * purposes:  you can get the boot object from it, and you can destroy it to break the
 * connection.  Note that destroying the bootObject does not break the connection because you
 * might have gotten other objects from it.
 */
public class Connection extends Heaper {

	protected static PrimPtr2PtrTable TheBootPlans;
/*
udanax-top.st:13857:
Heaper subclass: #Connection
	instanceVariableNames: ''
	classVariableNames: 'TheBootPlans {PrimPtr2PtrTable of: Category with: BootPlan} '
	poolDictionaries: ''
	category: 'Xanadu-cobbler'!
*/
/*
udanax-top.st:13861:
Connection comment:
'Suclasses represent particular kinds of connections.  The connection object serves two purposes:  you can get the boot object from it, and you can destroy it to break the connection.  Note that destroying the bootObject does not break the connection because you might have gotten other objects from it.'!
*/
/*
udanax-top.st:13863:
(Connection getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:13883:
Connection class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:13886:
(Connection getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Connection.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:13868:Connection methodsFor: 'accessing'!
{Category} bootCategory
	self subclassResponsibility!
*/
}
public Heaper bootHeaper() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:13872:Connection methodsFor: 'accessing'!
{Heaper} bootHeaper
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:13878:Connection methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:13880:Connection methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void initTimeNonInherited() {
	TheBootPlans = PrimPtr2PtrTable.make(8);
/*
udanax-top.st:13891:Connection class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	TheBootPlans _ PrimPtr2PtrTable make: 8.!
*/
}
public static void linkTimeNonInherited() {
	TheBootPlans = null;
/*
udanax-top.st:13894:Connection class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheBootPlans _ NULL!
*/
}
/**
 * Throw out any plan associated with cat.
 */
public static void clearPlan(Category cat) {
	TheBootPlans.remove(cat);
/*
udanax-top.st:13899:Connection class methodsFor: 'registration'!
{void} clearPlan: cat {Category}
	"Throw out any plan associated with cat."
	
	TheBootPlans remove: cat!
*/
}
/**
 * For the current run, return plan if anyone looks for a bootPlan that returns an instance
 * of the category that plan returns.
 */
public static void registerBootPlan(BootPlan plan) {
	TheBootPlans.introduce(plan.bootCategory(), plan);
/*
udanax-top.st:13904:Connection class methodsFor: 'registration'!
{void} registerBootPlan: plan {BootPlan}
	"For the current run, return plan if anyone looks for a bootPlan that returns an instance of the category that plan returns."
	
	TheBootPlans at: plan bootCategory introduce: plan!
*/
}
public static Connection make(Category category) {
	return ((BootPlan) (TheBootPlans.get(category))).connection();
/*
udanax-top.st:13911:Connection class methodsFor: 'creation'!
make: category {Category}
	^((TheBootPlans get: category) cast: BootPlan) connection!
*/
}
public Connection() {
/*

Generated during transformation
*/
}
public Connection(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
