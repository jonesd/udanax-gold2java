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
import info.dgjones.abora.gold.cobbler.ClearPlan;
import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;

/**
 * Remove a particular entry from the table of current BootPlans.
 */
public class ClearPlan extends BootPlan {

	protected Category myCategory;
/*
udanax-top.st:57022:
BootPlan subclass: #ClearPlan
	instanceVariableNames: 'myCategory {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cobbler'!
*/
/*
udanax-top.st:57026:
ClearPlan comment:
'Remove a particular entry from the table of current BootPlans.'!
*/
/*
udanax-top.st:57028:
(ClearPlan getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ClearPlan.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	return myCategory;
/*
udanax-top.st:57033:ClearPlan methodsFor: 'accessing'!
{Category} bootCategory
	^myCategory!
*/
}
/**
 * Return the object representing the connection.  This gives the client a handle by which to
 * terminate the connection.
 */
public Connection connection() {
	throw new AboraRuntimeException(AboraRuntimeException.NO_BOOT_PLAN);
/*
udanax-top.st:57036:ClearPlan methodsFor: 'accessing'!
{Connection} connection
	"Return the object representing the connection.  This gives the client a handle by which to terminate the connection."
	
	Heaper BLAST: #NoBootPlan.
	^NULL "fodder"!
*/
}
/**
 * Use this hook to clear the element out of the bootPlan registration table.
 */
public void execute() {
	Connection.clearPlan(myCategory);
/*
udanax-top.st:57044:ClearPlan methodsFor: 'operate'!
{void} execute
	"Use this hook to clear the element out of the bootPlan registration table."
	Connection clearPlan: myCategory!
*/
}
public ClearPlan(Category cat) {
	super();
	myCategory = cat;
/*
udanax-top.st:57051:ClearPlan methodsFor: 'creation'!
create: cat {Category}
	super create.
	myCategory _ cat!
*/
}
public ClearPlan(Rcvr receiver) {
	super(receiver);
	myCategory = (Category) receiver.receiveHeaper();
/*
udanax-top.st:57057:ClearPlan methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCategory _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myCategory);
/*
udanax-top.st:57061:ClearPlan methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCategory.!
*/
}
public ClearPlan() {
/*

Generated during transformation
*/
}
}
